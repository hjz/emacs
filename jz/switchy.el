(require 'autoinsert)
(require 'cl)
(auto-insert-mode 1)
(setq auto-insert-query nil)

(setq auto-insert-directory (expand-file-name "~/.emacs.d/auto/"))

;; TODO move cursor to right place
(setq auto-insert-alist
      '(
        ("Spec\\.scala$" . ["insertSpec.scala" auto-update-scala-source-file])
        ("\\.scala$" . ["insertBase.scala" auto-update-scala-source-file])
        ))

(defun filepath-to-package-name (s)
  (reduce (lambda (acc val) (if acc (concat (concat acc val) ".") (when (string= val "scala") ""))) (split-string s "/") :initial-value nil))

(defun auto-update-scala-source-file ()
  (setq bse (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
  (setq bsd (filepath-to-package-name (file-name-directory buffer-file-name)))
  (save-excursion
    ;; replace PPP with package name
    (while (search-forward "PPP" nil t)
           (save-restriction
             (narrow-to-region (match-beginning 0) (match-end 0))
             (replace-match (substring bsd 0 -2) t)
             ))
    )
  (save-excursion
    ;; Replace @@@ with file name
    (while (search-forward "@@@" nil t)
           (save-restriction
             (narrow-to-region (match-beginning 0) (match-end 0))
             (replace-match bse)
             ))
    )
  (save-excursion
    ;; Replace $$$ with src name
    (while (search-forward "$$$" nil t)
           (save-restriction
             (narrow-to-region (match-beginning 0) (match-end 0))
             (replace-match (substring bse 0 -4))
             ))
    )
)

(defun src-to-spec-filename (bse)
  (replace-regexp-in-string "main" "test" (concat bse "Spec.scala")))

(defun switch-between-test-and-source ()
  "Switch between a scala test (*Spec) and its corresponding source"
  (interactive)
  ;; grab the base of the current buffer's file name
  (setq bse (file-name-sans-extension buffer-file-name))
  ;; and the extension, converted to lowercase
  (setq ext (downcase (file-name-extension buffer-file-name)))
  (setq typ (substring bse -4 nil))
  (cond
   ;; first condition - switch to src
   ((equal typ "Spec")
    (setq nfn (replace-regexp-in-string "test" "main" (concat (substring bse 0 -4) ".scala")))

    ;; create directory if doesn't exist
    (setq dirname (file-name-directory nfn))
    (unless (file-accessible-directory-p dirname) (make-directory dirname t))
    (auto-insert-mode 1)
    (find-file nfn)
    )
   ;; second condition - switch to test file
   ((or (equal ext "scala"))
    (setq nfn (src-to-spec-filename bse))
    (setq dirname (file-name-directory nfn))
    (unless (file-accessible-directory-p dirname) (make-directory dirname t))
    (auto-insert-mode 1)
    (find-file nfn)
    )
   )
  )

(defun get-spec-path ()
  (interactive)
  (setq bse (file-name-sans-extension buffer-file-name))
  (setq typ (substring bse -4 nil))
  (if (equal typ "Spec") buffer-file-name (src-to-spec-filename bse)))

(defun get-spec-class ()
  (interactive)
  (substring (filepath-to-package-name (get-spec-path)) 0 -7))

(add-hook 'scala-mode-hook
  (lambda () (local-set-key (kbd "C-c SPC") 'switch-between-test-and-source)))

(setq cgit-workdir "/Users/jz/ps/")
(setq cgit-httpdir "http://cgit.local.twitter.com/")

(defun get-cgit-path (&optional plain-view)
  (interactive)
  (setq bfn (buffer-file-name))
  (setq proj-regex (concat cgit-workdir "\\([^/]+\\)/"))
  (string-match proj-regex bfn)
  (setq proj-name (match-string 1 bfn))
  (replace-regexp-in-string proj-regex (concat cgit-httpdir proj-name (if plain-view "/plain/" "/tree/")) bfn)
  )

(defun cgit-browse (&optional plain-view)
  (interactive)
  (browse-url (get-cgit-path plain-view)))

(defun cgit-yank (&optional plain-view)
  (interactive)
  (setq cgit-url (get-cgit-path plain-view))
  (kill-new cgit-url)
  (message cgit-url))

(provide 'switchy)
