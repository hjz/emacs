;;; git-find-file.el --- Find files in a git project quickly.

;; Author: Justin Zhu
;; Based on URL: http://www.emacswiki.org/cgi-bin/wiki/FindFileInProject
;;               http://github.com/re5et/find-file-in-git-repo

(defun ffip-project-files (repo)
  "Return an alist of all filenames in the project and their path."
  (let ((file-alist nil))
    (mapcar (lambda (file)
              (let ((file-cons (cons (file-name-nondirectory file) file)))
                (add-to-list 'file-alist file-cons)
                file-cons))
            (split-string (shell-command-to-string (format "cd %s && git ls-files" repo))))))

(defun find-git-repo (dir)
  "Find base git directory"
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

(defun last-component (str sep)
  (car (last (split-string str sep t))))

(defun git-find-file ()
  "Prompt with a completing list of all files in the project to find one."
  (interactive)
  (let* ((root (find-git-repo default-directory))
         (project-files (ffip-project-files root))
         (files (delete-dups (mapcar 'car project-files)))
         (file-name (ffip-completing-read (concat (last-component root "/") ": ") files))
         (file-paths (delq 'nil (mapcar '(lambda (file-cons)
                                           (when (string= file-name (car file-cons))
                                             (cdr file-cons))) project-files)))
         (file-path (if (cdr file-paths)
                        (ffip-completing-read "Disambiguate: " file-paths)
                      (car file-paths))))
    (find-file (concat root file-path))))

;;;###autoload
(defalias 'gffip 'git-find-file)

(provide 'git-find-file)
;;; git-find-file.el ends here
