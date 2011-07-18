;; file cache
(require 'filecache)
(require 'ido)

(defun file-cache-ido-find-file (file)
  "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive (list (file-cache-ido-read "File: "
                                          (mapcar
                                           (lambda (x)
                                             (car x))
                                           file-cache-alist))))
  (let* ((record (assoc file file-cache-alist)))
    (find-file
     (expand-file-name
      file
      (if (= (length record) 2)
          (car (cdr record))
        (file-cache-ido-read
         (format "Find %s in dir: " file) (cdr record)))))))

(defun file-cache-ido-read (prompt choices)
  (let ((ido-make-buffer-list-hook
	 (lambda ()
	   (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))


(add-to-list 'file-cache-filter-regexps "project/boot")
(add-to-list 'file-cache-filter-regexps "target")

;(add-to-list 'file-cache-filter-regexps "\\.sw\w$")
(add-to-list 'file-cache-filter-regexps "/\\..*")
;;(add-to-list 'file-cache-filter-regexps "/\\.#.*")
(add-to-list 'file-cache-filter-regexps "\\.class$")
(add-to-list 'file-cache-filter-regexps "\\.gz$")
(add-to-list 'file-cache-filter-regexps "\\.jpg$")
(add-to-list 'file-cache-filter-regexps "\\.gif$")
(add-to-list 'file-cache-filter-regexps "\\.png$")
(add-to-list 'file-cache-filter-regexps "\\.jar$")
(add-to-list 'file-cache-filter-regexps "\\.pyc$")
(add-to-list 'file-cache-filter-regexps "\\.svn-base$")
(add-to-list 'file-cache-filter-regexps "\\.dump$")
(add-to-list 'file-cache-filter-regexps "/\\.\w+$")
(add-to-list 'file-cache-filter-regexps "/\\.git")
(add-to-list 'file-cache-filter-regexps "/\\.svn")

(defvar file-cache-file "~/.file_cache")

(defun file-cache-refresh ()
  (interactive)
  (message "Loading file cache...")
  (file-cache-clear-cache)
  (file-cache-add-directory-using-find "~/ps/birdcage")
  (file-cache-add-directory-using-find "~/ps/macaw")
  (file-cache-add-directory-using-find "~/ps/querulous")
  (file-cache-add-directory-using-find "~/ps/twitter")
  (file-cache-add-directory-using-find dotfiles-dir)
  ;; (file-cache-add-directory-using-find "~/ps/science")
  ;; (file-cache-add-directory "~/")
  ;; (file-cache-add-file-list (list "~/foo/bar" "~/baz/bar"))
  (file-cache-save-cache-to-file)
  (message "File cache loaded and saved to %s" file-cache-file))

(defun file-cache-save-cache-to-file ()
  "Save contents of `file-cache-alist' to ~/.file_cache.
For later retrieval using `file-cache-read-cache-from-file'"
  (interactive)
  (with-temp-file (expand-file-name file-cache-file)
    (prin1 file-cache-alist (current-buffer))))

(defun file-cache-read-cache-from-file ()
  "Clear `file-cache-alist' and read cache from FILE.
  The file cache can be saved to a file using
  `file-cache-save-cache-to-file'."
  (interactive)
  (file-cache-clear-cache)
  (save-excursion
    (set-buffer (find-file-noselect file-cache-file))
    (beginning-of-buffer)
    (setq file-cache-alist (read (current-buffer)))))

(file-cache-read-cache-from-file)
