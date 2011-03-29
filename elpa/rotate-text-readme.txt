;;; Commentary:
;;
;; rotate-text allows you cycle through commonly interchanged text with a single
;; keystroke.  For example, you can toggle between "frame-width" and
;; "frame-height", between "public", "protected" and "private" and between
;; "variable1", "variable2" through "variableN".
;;
;; Add the following to your .emacs:
;;
;; (add-to-list 'load-path "/path/to/rotate-text")
;; (autoload 'rotate-text "rotate-text" nil t)
;; (autoload 'rotate-text-backward "rotate-text" nil t)
;;
;; Customize the variables `rotate-text-patterns', `rotate-text-symbols' and
;; `rotate-text-words'.  You can make buffer-local additions in
;; `rotate-text-local-patterns', `rotate-text-local-symbols' and
;; `rotate-text-local-words'.
;;
;; Use the commands `rotate-text' and `rotate-text-backward' to rotate the
;; text.
;;
