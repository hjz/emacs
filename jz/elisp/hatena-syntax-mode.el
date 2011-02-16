(require 'derived)

(defvar hatena-syntax-heading1-face 'font-lock-keyword-face)
(defvar hatena-syntax-heading2-face 'font-lock-type-face)
(defvar hatena-syntax-heading3-face 'font-lock-comment-face)
(defvar hatena-syntax-heading4-face 'font-lock-variable-name-face)
(defvar hatena-syntax-markup-face 'font-lock-function-name-face)
(defvar hatena-syntax-html-face 'font-lock-function-name-face)
(defvar hatena-syntax-link-face 'font-lock-string-face)

(defvar hatena-syntax-font-lock-keywords
  '(
    ("^\\*[^*].*$"
     (0 hatena-syntax-heading1-face))
    ("^\\*\\*[^*].*$"
     (0 hatena-syntax-heading2-face))
    ("^\\*\\*\\*[^*].*$"
     (0 hatena-syntax-heading3-face))
    ("^\\*\\*\\*\\*[^*].*$"
     (0 hatena-syntax-heading4-face))
    ("\\(<[^\n/].*>\\)\\([^<>\n]*\\)\\(</.*>\\)"
     (1 hatena-syntax-html-face)
     (2 hatena-syntax-link-face)
     (3 hatena-syntax-html-face))
    ("\\(\\[?\\(f\\|id\\|google\\|isbn\\|asin\\):[a-zA-Z0-9_+:-]+\\]?\\)"
     (hatena-syntax-markup-face))
    ("^\\(:\\)\\([^:\n]+\\)\\(:\\)"
     (1 hatena-syntax-markup-face)
     (2 hatena-syntax-link-face)
     (3 hatena-syntax-markup-face))
    ("^\\([-+]+\\)"
     (1 hatena-syntax-markup-face))
    ("\\(((\\).*\\())\\)"
     (1 hatena-syntax-markup-face)
     (2 hatena-syntax-markup-face))
    ("^\\(>>\\|<<\\|<!--\\(-[^-]\\|--[^>]\\|[^-]\\)*-->\\|>|?|\\||?|<\\)"
     (1 hatena-syntax-markup-face))
    ("\\(s?https?://\[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#\]+\\)"
     (0 hatena-syntax-link-face))))

(define-derived-mode hatena-syntax-mode text-mode "hatena-syntax"
  "Hatena syntax mode."
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((hatena-syntax-font-lock-keywords) t nil))
  (set-buffer-modified-p nil)
  (run-hooks 'hatena-syntax-mode-hook))

(provide 'hatena-syntax-mode)
