(autoload 'gtags-mode "gtags" "" t)
;; (add-hook 'gtags-select-mode-hook
;;   '(lambda ()
;;      (setq hl-line-face 'underline)
;;      (hl-line-mode 1)
;; ))
;;
(require 'xgtags)
(require 'xgtags-extension)
(require 'vimpulse)

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (interactive)
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update ()
  "Make GTAGS incremental update"
  (call-process "global" nil nil nil "-u"))

(defun gtags-update-hook ()
  (interactive)
  (when (gtags-root-dir)
    (gtags-update)))

(defun ww-next-gtag ()
  "Find next matching tag, for GTAGS."
  (interactive)
  (let ((latest-gtags-buffer
         (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                 (buffer-list)) ))))
    (cond (latest-gtags-buffer
           (switch-to-buffer latest-gtags-buffer)
           (next-line)
           (gtags-select-it nil))
          ) ))

(global-set-key [(control meta .)] 'gtags-find-rtag)   ;; C-M-. find all references of tag
(global-set-key [(control meta ,)] 'gtags-find-symbol) ;; C-M-, find all usages of symbol.

(vimpulse-map (kbd "M-.") 'gtags-find-tag 'scala-mode)
(vimpulse-map (kbd "s-.") 'ww-next-gtag 'scala-mode)   ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
(vimpulse-map (kbd "M-,") 'gtags-pop-stack 'scala-mode)
