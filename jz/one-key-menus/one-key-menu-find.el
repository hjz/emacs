;; one-key menu for find

(defvar one-key-menu-find-alist nil
  "The `one-key' menu alist for find.")

(setq one-key-menu-find-alist
      '(
        (("d" . "moccur-grep") . moccur-grep)
        (("h" . "moccur-grep-find") . moccur-grep-find)
        (("g" . "dmoccur") . dmoccur)
        (("s" . "ack-same") . ack-same)
        (("a" . "ack") . ack)
        (("f" . "ack-find-file") . ack-find-file)
        (("r" . "replace-regexp") . replace-regexp)
        (("l" . "lazy-search-menu") . lazy-search-menu)
        (("p" . "find-grep-dired") .  find-grep-dired)
        (("n" . "find-name-dired") .  find-name-dired)
        (("o" . "dired-do-moccur") .  dired-do-moccur)
        (("i" . "ibuffer-do-occur") .  ibuffer-do-occur)))

(defun one-key-menu-find ()
  "The `one-key' menu for find"
  (interactive)
  (one-key-menu "find" one-key-menu-find-alist))

;; Use the `one-key-get-menu' command to show menu/keybindings for this buffer.

;; Uncomment and edit following line to set this menu as default for mode.
;;(add-to-list 'one-key-mode-alist '(find-mode . one-key-menu-find))
;; Uncomment and edit following line to add this menu to toplevel menu.
;;(add-to-list 'one-key-toplevel-alist '(("type key here" . "find") . one-key-menu-find))
