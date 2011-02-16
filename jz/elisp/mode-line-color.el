(defgroup mode-line-color nil
  "Mode line color."
  :prefix "mode-line-color-"
  :group 'mode-line)

(defcustom mode-line-color-buffers-regexp '("^\\*scratch\\*$")
  "List of regular expressions of buffer names to enable mode-line-color-mode automatically."
  :group 'mode-line-color
  :type '(repeat 'string))

(defcustom mode-line-color-exclude-buffers-regexp '("^ .*" "^\\*")
  "List of regular expressions of buffer names not to enable mode-line-color-mode automatically."
  :group 'mode-line-color
  :type '(repeat 'string))

(defvar mode-line-color-hook nil
  "hook for setting mode line color

   Usage:
     (defun your-function-to-set-mode-line-color (setter)
       (funcall setter \"yellow\"))
     (add-hook 'mode-line-color-hook 'your-function-to-set-mode-line-color)")

(defvar mode-line-color-mode nil)
(defvar mode-line-color-color nil)
(defvar mode-line-color-original "white")

(defun mode-line-color-set-color (color)
  (setq mode-line-color-color color))

(defun mode-line-color-active-p ()
  (let ((buf (buffer-name (current-buffer)))
        (mem-pat
         '(lambda (x l)
            (member t (mapcar '(lambda (r) (when (string-match r x) t)) l)))))
    (and mode-line-color-mode
         (not (minibufferp (current-buffer)))
         (or (funcall mem-pat buf mode-line-color-buffers-regexp)
             (not (funcall mem-pat buf
                           mode-line-color-exclude-buffers-regexp))))))

(defun mode-line-color-update ()
  (when (mode-line-color-active-p)
    (let ((mode-line-color-color nil))
      (run-hook-with-args 'mode-line-color-hook 'mode-line-color-set-color)
      (set-face-background 'mode-line (or mode-line-color-color
                                          mode-line-color-original)))))

(defun mode-line-color-install ()
  (setq mode-line-color-original (face-background 'mode-line))
  (add-hook 'post-command-hook 'mode-line-color-update))

(defun mode-line-color-uninstall ()
  (remove-hook 'post-command-hook 'mode-line-color-update))

;;;###autoload
(define-minor-mode mode-line-color-mode
  "Set color of mode line."
  :global t
  :group 'mode-line-color
  (if mode-line-color-mode
      (mode-line-color-install)
    (mode-line-color-uninstall)))

(provide 'mode-line-color)
