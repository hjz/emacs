;;; git-find-file-in-project.el --- Find files in a project quickly.

;; Copyright (C) 2006-2009, 2011
;;   Phil Hagelberg, Doug Alcorn, and Will Farrington

;; Author: Phil Hagelberg, Doug Alcorn, and Will Farrington
;; URL: http://www.emacswiki.org/cgi-bin/wiki/FindFileInProject
;; Git: git://github.com/technomancy/git-find-file-in-project.git
;; Version: 2.1
;; Created: 2008-03-18
;; Keywords: project, convenience
;; EmacsWiki: FindFileInProject

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library provides a couple methods for quickly finding any file
;; in a given project.  It depends on GNU find.

;; A project is found by searching up the directory tree until a file
;; is found that matches `ffip-project-file'.  (".git" by default.)
;; You can set `ffip-project-root-function' to provide an alternate
;; function to search for the project root.  By default, it looks only
;; for files whose names match `ffip-patterns',

;; If you have so many files that it becomes unwieldy, you can set
;; `ffip-find-options' to a string which will be passed to the `find'
;; invocation in order to exclude irrelevant subdirectories.  For
;; instance, in a Ruby on Rails project, you may be interested in all
;; .rb files that don't exist in the "vendor" directory.  In that case
;; you could set `ffip-find-options' to "-not -regex \".*vendor.*\"".

;; All these variables may be overridden on a per-directory basis in
;; your .dir-locals.el.  See (info "(Emacs) Directory Variables") for
;; details.

;; Recommended binding: (global-set-key (kbd "C-x f") 'git-find-file-in-project)

;;; TODO:

;; Add compatibility with BSD find (PDI; I can't virtualize OS X)

;;; Code:

(defvar ffip-patterns
  '("*.html" "*.org" "*.txt" "*.md" "*.el" "*.clj" "*.py" "*.rb" "*.js" "*.pl" "*.scala" "*.java" "*.thrift"
    "*.sh" "*.erl" "*.hs" "*.ml" "*.erb" "*.haml")
  "List of patterns to look for with `git-find-file-in-project'.")

(defun ffip-project-files (repo)
  "Return an alist of all filenames in the project and their path."
  (let ((file-alist nil))
    (mapcar (lambda (file)
              (let ((file-cons (cons (file-name-nondirectory file) file)))
                (add-to-list 'file-alist file-cons)
                file-cons))
            (split-string (shell-command-to-string (format "cd %s && git ls-files" repo))))))

(defun find-git-repo (dir)
  (if (string= "/" dir)
      (message "not in a git repo.")
    (if (file-exists-p (expand-file-name ".git/" dir))
        dir
      (find-git-repo (expand-file-name "../" dir)))))

(defun ffip-completing-read (prompt names)
  "Perform a completing read over NAMES prompted by PROMPT.

ido is used for the completing read if available."
  (if (and (boundp 'ido-mode) ido-mode)
      (ido-completing-read prompt names nil t)
    (completing-read prompt names nil t)))

;;;###autoload

(defun last-component (str sep)
  (car (last (split-string str sep t))))

(defun git-find-file ()
  "Prompt with a completing list of all files in the project to find one.

The project's scope is defined as the first directory containing
an `.emacs-project' file.  You can override this by locally
setting the variable `ffip-project-root'."
  (interactive)
  (let* ((root (find-git-repo default-directory))
         (project-files (ffip-project-files root))
         (files (mapcar 'car project-files))
         (file-name (ffip-completing-read (concat (last-component root "/") ": ") files))
         (file-paths (delq 'nil (mapcar '(lambda (file-cons)
                                           (when (string-match file-name (car file-cons))
                                             (cdr file-cons))) project-files)))
         (file-path (if (cdr file-paths)
                        (ffip-completing-read "Disambiguate: " file-paths)
                      (car file-paths))))
    (find-file (concat root file-path))))

;;;###autoload
(defalias 'gffip 'git-find-file)

;; safe locals
;;;###autoload
(progn
  (put 'ffip-patterns 'safe-local-variable 'listp)
  (put 'ffip-find-options 'safe-local-variable 'stringp)
  (put 'ffip-project-file 'safe-local-variable 'stringp)
  (put 'ffip-project-root 'safe-local-variable 'stringp)
  (put 'ffip-project-root-function 'safe-local-variable 'functionp)
  (put 'ffip-limit 'safe-local-variable 'integerp))

(provide 'git-find-file)
;;; git-find-file.el ends here
