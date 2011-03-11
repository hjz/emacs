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
    (setq nfn (replace-regexp-in-string "test" "src" (concat (substring bse 0 -4) ".scala")))
    (find-file nfn)
    )
   ;; second condition - switch to test file
   ((or (equal ext "scala"))
    (setq nfn (replace-regexp-in-string "src" "test" (concat bse "Spec.scala")))
    (find-file nfn)
    )
   )
  )
(global-set-key (kbd "C-c s") 'switch-between-test-and-source)
(provide 'switchy)
