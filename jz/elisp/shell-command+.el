;; Shell Command (with saving the last command for default value)
(defun default-from-history (fun prompt history make-arg)
  `(lambda ()
     (interactive)
     (let ((default (car ,history)))
       (if (null default)
           (call-interactively (quote ,fun))
         (let ((arg (read-from-minibuffer
                    (concat ,prompt " (default '" default "'): ")
                    nil nil nil (quote ,history) default)))
           (let ((args (funcall (quote ,make-arg)
                                (if (or (null arg) (string= arg ""))
                                    default arg))))
             (apply (quote ,fun) args)))))))

(defun shell-command+ (&optional cmd)
  (interactive)
  (let ((fun (default-from-history
               'shell-command "command" 'shell-command-history 'list)))
    (if (interactive-p)
        (call-interactively fun)
      (funcall fun cmd))))
(defun shell-command-on-region+ (&optional start end cmd)
  (interactive)
  (let ((fun (default-from-history
               'shell-command-on-region
               "command on region" 'shell-command-history
               '(lambda (arg) (list (region-beginning) (region-end) arg)))))
    (if (interactive-p)
        (call-interactively fun)
      (funcall fun start end cmd))))

(provide 'shell-command+)
