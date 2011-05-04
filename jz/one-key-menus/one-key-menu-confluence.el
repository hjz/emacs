;; one-key menu for confluence

(defvar one-key-menu-confluence-alist nil
  "The `one-key' menu alist for confluence.")

(setq one-key-menu-confluence-alist
      '(
	(("C-j" . "Confluence Newline And Indent (C-j)") . confluence-newline-and-indent)
	;; (("C-x" . "Prefix Command (C-x)") . Prefix Command)
	;; (("ESC" . "Prefix Command (ESC)") . Prefix Command)

	;; (("C-x w" . "Prefix Command (C-x w)") . Prefix Command)

	(("M-TAB" . "Ispell Complete Word (M-TAB)") . ispell-complete-word)

	;; (("C-x w *" . "Confluence Pop Tag Stack (C-x w *)") . confluence-pop-tag-stack)
	;; (("C-x w ." . "Confluence Get Page At Point (C-x w .)") . confluence-get-page-at-point)
	;; (("C-x w =" . "Confluence Ediff Current Page (C-x w =)") . confluence-ediff-current-page)
	(("d" . "Confluence Get Attachment (C-x w a)") . confluence-get-attachment)
	(("g" . "Confluence Browse Page (C-x w b)") . confluence-browse-page)
	(("m" . "Confluence Create Page (C-x w c)") . confluence-create-page)

	(("j" . "Confluence Get Page (C-x w f)") . confluence-get-page)
	(("E" . "Confluence Ediff Merge Current Page (C-x w m)") . confluence-ediff-merge-current-page)
	(("p" . "Confluence Get Parent Page (C-x w p)") . confluence-get-parent-page)
	(("R" . "Confluence Rename Page (C-x w r)") . confluence-rename-page)
	(("k" . "Confluence Search (C-x w s)") . confluence-search)
	(("P" . "Confluence Preview (C-x w v)") . confluence-preview)
	(("r" . "Confluence Get Related Page (C-x w x)") . confluence-get-related-page)

	;; (("C-x w l a" . "Confluence Add Label (C-x w l a)") . confluence-add-label)
	;; (("C-x w l g" . "Confluence Get Labels (C-x w l g)") . confluence-get-labels)
	;; (("C-x w l r" . "Confluence Remove Label (C-x w l r)") . confluence-remove-label)

	(("A" . "Confluence Insert Anchor (C-x w j A)") . confluence-insert-anchor)
	(("C" . "Confluence Cite Text (C-x w j C)") . confluence-cite-text)
	(("S" . "Confluence Subscript Text (C-x w j S)") . confluence-subscript-text)
	(("a" . "Confluence Linkify Anchor Text (C-x w j a)") . confluence-linkify-anchor-text)
	(("b" . "Confluence Boldify Text (C-x w j b)") . confluence-boldify-text)
	(("c" . "Confluence Codify Text (C-x w j c)") . confluence-codify-text)
	(("e" . "Confluence Embed Text (C-x w j e)") . confluence-embed-text)
	(("h" . "Confluence Insert Horizontal Rule (C-x w j h)") . confluence-insert-horizontal-rule)
	(("i" . "Confluence Italicize Text (C-x w j i)") . confluence-italicize-text)
	(("l" . "Confluence Linkify Text (C-x w j l)") . confluence-linkify-text)
	(("s" . "Confluence Superscript Text (C-x w j s)") . confluence-superscript-text)
	(("t" . "Confluence Linkify Attachment Text (C-x w j t)") . confluence-linkify-attachment-text)
	(("u" . "Confluence Underline Text (C-x w j u)") . confluence-underline-text)
	(("x" . "Confluence Strike Text (C-x w j x)") . confluence-strike-text)
	))

(defun one-key-menu-confluence ()
  "The `one-key' menu for confluence"
  (interactive)
  (one-key-menu "confluence" one-key-menu-confluence-alist))

;; Use the `one-key-get-menu' command to show menu/keybindings for this buffer.

;; Uncomment and edit following line to set this menu as default for mode.
;;(add-to-list 'one-key-mode-alist '(confluence-mode . one-key-menu-confluence))
;; Uncomment and edit following line to add this menu to toplevel menu.
;;(add-to-list 'one-key-toplevel-alist '(("type key here" . "confluence") . one-key-menu-confluence))
