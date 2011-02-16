(defvar default-file-coding-system-alist nil
  "list of pairs of a file name regexp and a coding system.")
(add-hook 'find-file-hook
          '(lambda ()
             (let ((code (assoc-default buffer-file-name
                                        default-file-coding-system-alist
                                        'string-match nil)))
               (if code
                   (progn
                     (make-local-variable 'default-buffer-file-coding-system)
                     (setq default-buffer-file-coding-system code))))))

(provide 'default-file-coding-systems)
