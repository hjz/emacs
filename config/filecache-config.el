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


(defun file-cache-refresh ()
  (message "Loading file cache...")
  (file-cache-clear-cache)
  (file-cache-add-directory-using-find "~/ps/birdcage")
;  (file-cache-add-directory-using-find "~/ps/science")
  (file-cache-add-directory-using-find "~/ps/macaw")
  (file-cache-add-directory-using-find "~/ps/querulous")
;  (file-cache-add-directory-using-find "~/ps/twitter")
  (file-cache-add-directory-using-find dotfiles-dir)
  ;	    (file-cache-add-directory "~/")
  ;	    (file-cache-add-file-list (list "~/foo/bar" "~/baz/bar"))
  )

(file-cache-refresh)

(add-to-list 'file-cache-filter-regexps "project/boot")
(add-to-list 'file-cache-filter-regexps "target")

(add-to-list 'file-cache-filter-regexps "\\.sw\w$")
(add-to-list 'file-cache-filter-regexps "/\\.#.*")
(add-to-list 'file-cache-filter-regexps "\\.class$")
(add-to-list 'file-cache-filter-regexps "\\.gz$")
(add-to-list 'file-cache-filter-regexps "\\.jpg$")
(add-to-list 'file-cache-filter-regexps "\\.gif$")
(add-to-list 'file-cache-filter-regexps "\\.png$")
(add-to-list 'file-cache-filter-regexps "\\.jar$")
(add-to-list 'file-cache-filter-regexps "\\.svn-base$")
(add-to-list 'file-cache-filter-regexps "\\.dump$")
(add-to-list 'file-cache-filter-regexps "/[.]\w+$")
(add-to-list 'file-cache-filter-regexps "/[.]git")
(add-to-list 'file-cache-filter-regexps "/[.]svn")
