
;;needed in insert-word-or-file-name
(defun backward-to-non-blank () "go to 1st non blank (after blank) to left"
 (interactive)
       (if (re-search-backward "[ \t\n][^ \t\n]" (point-min) t)
           (forward-char 1)
           (if (string-match "[^ \t\n]" (buffer-substring 1 2))
               (goto-char (point-min)))))


;;needed in insert-buffer/file/dir-name functions
(defun buffer-name-not-mini ()
  "Return the name of current buffer, as a string.
If current buffer is the *mini-buffer* return name of previous-window."
       (buffer-name (if (window-minibuffer-p)
                           (if (eq (get-lru-window) (next-window))
                               (window-buffer (previous-window))
                               (window-buffer (next-window)))
                           nil)))


(defun insert-word-or-file-name ()
  "copy word cursor is on or file name to minibuff input"
       (interactive)
       (let* ((bfl (current-buffer))
              (str ""))
           (set-buffer (buffer-name-not-mini))
           (cond
               ((eq major-mode 'dired-mode)
                       (setq str (dired-get-filename t t)))
               (t
                       (let (bch ech)
                           (forward-char 1)
                           (backward-to-non-blank)
                           (setq bch (point))
                           (re-search-forward "[^ \t\n][ \t\n]" (point-max) t)
                           (setq ech (1- (point)))
                           (setq str (buffer-substring bch ech)))))
           (set-buffer bfl)
           (insert str)))


(defun insert-buffer-name ()
  "insert buffer name of current buffer or most recent buffer when in
minibuffer"
       (interactive)
       (insert (buffer-name-not-mini)))


(defun insert-buffer-dir-name ()
  "insert dir name of current buffer or most recent buffer when in minibuffer"
       (interactive)
       (let* ((bfn (buffer-file-name (get-buffer (buffer-name-not-mini)))))
           (if bfn
               (insert (file-name-directory bfn)))))


(defun insert-buffer-file-name ()
  "insert file name of current buffer or most recent buffer when in minibuffer"
       (interactive)
       (let* ((bfn (buffer-file-name (get-buffer (buffer-name-not-mini)))))
           (if bfn
               (insert (file-name-nondirectory bfn)))))


(defun complete-from-minibuffer-history ()
  "Take the history list and make it available as a `completions' buffer"
  (interactive)
       (with-output-to-temp-buffer "*Completions*"
           (display-completion-list (symbol-value minibuffer-history-variable))
           (save-excursion
               (set-buffer standard-output)
               (setq completion-base-size 0))))


(defun insert-current-date-time-minibuf ()
  "insert the current date and time into mini-buffer."
       (interactive)
       (insert (format-time-string "%y%m%d_%H%M%S" (current-time))))


(defun keymap-test (var)           ; internal function for keymap checking
       (and (boundp var)
            (keymapp (symbol-value var))))

(let ((minimaps (apropos-internal "mini" 'keymap-test))
      map op)
       (while minimaps
           (setq map (symbol-value (car minimaps)))
           (setq minimaps (cdr minimaps))
           (setq op (lookup-key map [9]))               ;tab char (^I)
           (if op
               (define-key map '[tab] op))
           (define-key map "\C-b" 'insert-buffer-name)
           (define-key map "\C-d" 'insert-buffer-dir-name)
           (define-key map "\C-f" 'insert-buffer-file-name)
           (define-key map "\C-w" 'insert-word-or-file-name)
           (define-key map "\C-t" 'insert-current-date-time-minibuf)
           (define-key map "\eh"  'complete-from-minibuffer-history)))
