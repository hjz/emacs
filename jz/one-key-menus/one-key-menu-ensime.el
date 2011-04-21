;;
;; one-key menu for ensime

(defvar one-key-menu-ensime-alist nil
  "The `one-key' menu alist for ensime.")

(setq one-key-menu-ensime-alist
      '(
	(("M-," . "Ensime Pop Find Definition Stack (M-,)") . ensime-pop-find-definition-stack)
	(("M-." . "Ensime Edit Definition (M-.)") . ensime-edit-definition)
	(("M-n" . "Ensime Forward Note (M-n)") . ensime-forward-note)
	(("M-p" . "Ensime Backward Note (M-p)") . ensime-backward-note)

	(("=" . "Ensime Builder Build (C-c C-b b)") . ensime-builder-build)
	(("+" . "Ensime Builder Rebuild (C-c C-b r)") . ensime-builder-rebuild)

	(("y" . "Ensime Refactor Inline Local (C-c C-r i)") . ensime-refactor-inline-local)
	(("L" . "Ensime Refactor Extract Local (C-c C-r l)") . ensime-refactor-extract-local)
	(("m" . "Ensime Refactor Extract Method (C-c C-r m)") . ensime-refactor-extract-method)
	(("O" . "Ensime Refactor Organize Imports (C-c C-r o)") . ensime-refactor-organize-imports)
	(("r" . "Ensime Refactor Rename (C-c C-r r)") . ensime-refactor-rename)
	(("T" . "Ensime Import Type At Point (C-c C-r t)") . ensime-import-type-at-point)

	;; (("b" . "Ensime Db Set Break (C-c C-d b)") . ensime-db-set-break)
	;; (("c" . "Ensime Db Continue (C-c C-d c)") . ensime-db-continue)
	;; (("d" . "Ensime Db Start (C-c C-d d)") . ensime-db-start)
	;; (("x" . "Ensime Db List Locals (C-c C-d l)") . ensime-db-list-locals)
	;; (("n" . "Ensime Db Next (C-c C-d n)") . ensime-db-next)
	;; (("q" . "Ensime Db Quit (C-c C-d q)") . ensime-db-quit)
	;; (("z" . "Ensime Db Run (C-c C-d r)") . ensime-db-run)
	;; (("s" . "Ensime Db Step (C-c C-d s)") . ensime-db-step)
	;; (("U" . "Ensime Db Clear Break (C-c C-d u)") . ensime-db-clear-break)

	(("." . "Ensime Expand Selection Command (C-c C-v .)") . ensime-expand-selection-command)
	(("a" . "Ensime Typecheck All (C-c C-v a)") . ensime-typecheck-all)
	(("t" . "Ensime Typecheck Current File (C-c C-v c)") . ensime-typecheck-current-file)
	(("f" . "Ensime Format Source (C-c C-v f)") . ensime-format-source)
	(("j" . "Ensime Inspect Type At Point (C-c C-v i)") . ensime-inspect-type-at-point)
	(("o" . "Ensime Inspect Project Package (C-c C-v o)") . ensime-inspect-project-package)
	(("k" . "Ensime Inspect Package At Point (C-c C-v p)") . ensime-inspect-package-at-point)
	(("u" . "Ensime Show Uses Of Symbol At Point (C-c C-v r)") . ensime-show-uses-of-symbol-at-point)
	(("SPC" . "Ensime Sbt Switch (C-c C-v s)") . ensime-sbt-switch)
	((";" . "Ensime Undo Peek (C-c C-v u)") . ensime-undo-peek)
	(("/" . "Ensime Search (C-c C-v v)") . ensime-search)
	(("i" . "Ensime Inf Switch (C-c C-v z)") . ensime-inf-switch)

	(("l" . "Start ensime") . ensime)

	;; (("5 i" . "Ensime Inspect Type At Point Other Frame (C-c C-v 5 i)") . ensime-inspect-type-at-point-other-frame)
	))

(defun one-key-menu-ensime ()
  "The `one-key' menu for ensime"
  (interactive)
  (one-key-menu "ensime" one-key-menu-ensime-alist))

;; Use the `one-key-get-menu' command to show menu/keybindings for this buffer.

;; Uncomment and edit following line to set this menu as default for mode.
(add-to-list 'one-key-mode-alist '(ensime-mode . one-key-menu-ensime))
