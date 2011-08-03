(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq user-dir (concat dotfiles-dir user-login-name))

;; no graphic dialog
(setq use-dialog-box nil)

(add-to-list 'load-path (concat user-dir "/elisp"))
(add-to-list 'load-path (concat user-dir "/apel-10.8"))
(add-to-list 'load-path (concat user-dir "/yasnippet-read-only"))
(add-to-list 'load-path (concat user-dir "/ensime_2.8.2-SNAPSHOT-0.5.1/elisp"))
(add-to-list 'load-path (concat user-dir "/vimpulse"))
(add-to-list 'load-path (concat user-dir "/vimpulse-surround"))
(add-to-list 'load-path (concat user-dir "/vimpulse-plugins"))
(add-to-list 'load-path (concat user-dir "/viper-in-more-modes"))
(add-to-list 'load-path (concat user-dir "/ecb"))
(add-to-list 'load-path (concat user-dir "/scala-mode"))
(add-to-list 'load-path (concat user-dir "/anything-config"))
(add-to-list 'load-path (concat user-dir "/elscreen-1.4.6"))
(add-to-list 'load-path (concat user-dir "/yaml-mode"))
(add-to-list 'load-path (concat user-dir "/moccur"))
(add-to-list 'load-path (concat user-dir "/popwin"))
(add-to-list 'load-path (concat user-dir "/auto-complete"))
(add-to-list 'load-path (concat user-dir "/full-ack"))
(add-to-list 'load-path (concat user-dir "/dired-extras"))
(add-to-list 'load-path (concat user-dir "/smex"))
(add-to-list 'load-path (concat user-dir "/twittering-mode"))
(add-to-list 'load-path (concat user-dir "/browse-kill-ring"))
(add-to-list 'load-path (concat user-dir "/one-key-menus"))
(add-to-list 'load-path (concat user-dir "/magit-1.0.0"))
(add-to-list 'load-path (concat user-dir "/confluence-mode"))
(add-to-list 'load-path (concat user-dir "/minimap"))
(add-to-list 'load-path (concat user-dir "/google-weather"))
(add-to-list 'load-path (concat user-dir "/find-file-in-project"))
(add-to-list 'load-path (concat user-dir "/org-mode"))
(add-to-list 'load-path (concat user-dir "/cc-mode-5.31.3"))
(add-to-list 'load-path (concat user-dir "/switch-window"))
(add-to-list 'load-path (concat user-dir "/org-jekyll"))

(require 'switch-window)
(require 'jz-file-utils)
(require 'growl)

(setq locate-command "mdfind")

;; backup and autosave settings
(defvar temporary-file-directory "~/.saves")
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist `((".*" . ,temporary-file-directory))
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   delete-old-versions t
   kept-new-versions 16
   kept-old-versions 2
   version-control t)       ; use versioned backups

(defun vendor (library)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/vendor/" file))
         (suffix (concat normal ".el")))
    (cond
     ((file-directory-p normal)
      (add-to-list 'load-path normal)
      (require library))
     ((file-directory-p suffix)
      (add-to-list 'load-path suffix)
      (require library))
     ((file-exists-p suffix)
      (require library)))))

(vendor 'gist)

(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

Set it to nil using let in around-advice for functions where the
original completing-read is required.  For example, if a function
foo absolutely must use the original completing-read, define some
advice like this:

(defadvice foo (around original-completing-read-only activate)
  (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                               allcomp
                               nil require-match initial-input hist def))
        ad-do-it))))


(defun load-config (module)
  (load (concat dotfiles-dir "config/" module ".el")))

(load-config "desktop")
(load-config "erc")
(load-config "twittering")
(load-config "filecache")
(load-config "aliases")
(load-config "org")

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(defun toggle-kbd-macro-recording-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key
    global-map
    (this-command-keys)
    'toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun toggle-kbd-macro-recording-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key
    global-map
    (this-command-keys)
    'toggle-kbd-macro-recording-on)
  (end-kbd-macro))

(require 'dot-mode)

(add-hook 'find-file-hooks 'dot-mode-on)
; C-. mapped to flyspell... remap it do dotmode
(define-key (current-global-map) [remap flyspell-auto-correct-word] 'dot-mode-execute)

(require 'confluence)
;(require 'rinari)

(autoload 'confluence-get-page "confluence" nil t)

(eval-after-load "confluence"
  '(progn
       (add-hook 'confluence-mode-hook '(lambda ()
                                         (auto-fill-mode -1)
                                         (local-set-key "\C-j" 'confluence-newline-and-indent)))))

;; open confluence page
(global-set-key "\C-xwf" 'confluence-get-page)

;(add-to-list 'load-path (concat user-dir "/scamacs/scamacs"))
;(add-to-list 'load-path (concat user-dir "/scamacs/ecb"))

(add-to-list 'load-path (concat user-dir "/mo-git-blame"))
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

(setq exec-path (append exec-path '("/usr/local/bin")))

(require 'one-key)
(require 'lazy-search-extension)

 (defun rotate-windows ()
  "Rotate your windows" (interactive) (cond ((not (> (count-windows) 1)) (message "You can't rotate a single window!"))
 (t
  (setq i 1)
  (setq numWindows (count-windows))
  (while  (< i numWindows)
    (let* (
           (w1 (elt (window-list) i))
           (w2 (elt (window-list) (+ (% i numWindows) 1)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2))
           )
      (set-window-buffer w1  b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)
      (setq i (1+ i)))))))

;; M-x IDO
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'mouse+)
(require 'google-search)
(require 'magit)

(defun quit-close ()
  (interactive)
  (quit-window)
  (delete-window))

(add-hook 'magit-mode-hook '
          (lambda ()
            (local-set-key (kbd "q") 'quit-close)))

;; PATH
;; (defun read-system-path ()
;;   (with-temp-buffer
;;     (insert-file-contents "/etc/paths")
;;     (goto-char (point-min))
;;     (replace-regexp "\n" ":")
;;     (thing-at-point 'line)))

;(setenv "PATH" (read-system-path))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;(require 'edit-server)
;(edit-server-start)
;(add-hook 'after-change-major-mode-hook 'edit-server-edit-mode)

;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

(push '("*Shell Command Output*" :height 20) popwin:special-display-config)
(setq anything-samewindow nil)
(push '("*anything*" :height 20) popwin:special-display-config)
(push '("*anything for files*" :height 20) popwin:special-display-config)
;; (push '("*ensime-sbt*" :height 25 :position bottom :stick t) popwin:special-display-config)
(push '("*pianobar*" :width 60 :position right) popwin:special-display-config)
(push '("*ENSIME-Compilation-Result*" :height 50 :position bottom :stick t) popwin:special-display-config)
(push '("*ensime-inferior-scala*" :width 60 :position right :stick t) popwin:special-display-config)
(push '("*scratch*") popwin:special-display-config)
(push '("*viper-info*") popwin:special-display-config)
(push '("*Messages*") popwin:special-display-config)
(push '("*grep*" :height 50) popwin:special-display-config)
(push '("*Kill Ring*" :height 30) popwin:special-display-config)
(push '("*Inspector*" :width 60 :position right) popwin:special-display-config)
(push '(dired-mode :position right :width 70) popwin:special-display-config) ; dired-jump-other-window (C-x 4 C-j)
(push '("*Warnings*") popwin:special-display-config)
(push '("*Help*" :height 30 :position bottom) popwin:special-display-config)
(push '("*Completions*" :height 30 :position bottom) popwin:special-display-config)
(push '("*One-Key*") popwin:special-display-config)

;(require 'sunrise-commander)

;;
(require 'dired+)                 ; Extensions to Dired.
(when (require 'dired-sort-menu nil t)
  (require 'dired-sort-menu+ nil t))    ; Menu/dialogue for dired sort options
(require 'dired-details+)         ; Make file details hideable in dired

(toggle-dired-find-file-reuse-dir 1)

(add-hook 'dired-mode-hook 'my-dired-mode-hook)
(defun my-dired-mode-hook ()
  (local-set-key (kbd "<mouse-1>") 'dired-mouse-find-file)
  (define-key dired-mode-map ";" 'dired-details-toggle)
  (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map "c" 'dired-do-copy))

(add-hook 'wdired-mode-hook 'viper-mode)

;; side by side dired for copying
(setq dired-dwim-target t)

(defun dired-mouse-find-file (event)
  "In Dired, visit the file or directory name you click on."
  (interactive "e")
  (require 'cl)
  (flet ((find-file-other-window
          (filename &optional wildcards)
          (find-file filename wildcards)))
    (dired-mouse-find-file-other-window event)))

;; colorise css hex values
(autoload 'css-color-mode "mon-css-color" "" t)
(add-hook 'css-mode-hook  'css-color-turn-on-in-buffer)

(setq ring-bell-function 'ignore)

;; Theme
(require 'color-theme)
(load-theme 'zenburn)
(set-face-foreground 'vertical-border "#282828")

;;;;;;;;;;;;;;;;; CEDET ;;;;;;;;;;;;;;;;;;;;;;
;(defcustom semantic-ectag-program "/opt/local/bin/ctags"
  ;"The Exuberent CTags program to use."
  ;:group 'semantic
  ;:type 'program)

;(semantic-mode 1)

;(global-ede-mode t)

;(if (boundp 'semantic-load-enable-excessive-code-helpers)
    ;; Add-on CEDET
    ;(progn
      ;(semantic-load-enable-excessive-code-helpers)
      ;; TODO: should already be enabled by previous line
      ;(global-semantic-idle-completions-mode)
      ;(global-semantic-tag-folding-mode))
   ;; Integrated CEDET
  ;(setq semantic-default-submodes
        ;'(global-semanticdb-minor-mode
          ;global-semantic-idle-scheduler-mode
          ;global-semantic-idle-summary-mode
          ;global-semantic-idle-completions-mode
          ;global-semantic-decoration-mode
          ;global-semantic-highlight-func-mode
          ;global-semantic-stickyfunc-mode)))

;(if (boundp 'semantic-ia) (require 'semantic-ia))
;(if (boundp 'semantic-gcc) (require 'semantic-gcc))

;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;(load "jz/cedet-1.0pre7/contrib/semantic-ectag-scala.el")

;(semantic-load-enable-excessive-code-helpers)

;(semantic-load-enable-minimum-features)      ; Enable prototype help and smart completion
;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;(semantic-load-enable-primary-exuberent-ctags-support)
;(defun my-semantic-hook ()
;  (imenu-add-to-menubar "TAGS"))
;(add-hook 'semantic-init-hooks 'my-semantic-hook)

;; TABS
;(define-key global-map [(alt j)] 'tabbar-backward)
;(define-key global-map [(alt k)] 'tabbar-forward)
(require 'tabkey2)

;;;;;;;;;;;;;;;; CEDET END ;;;;;;;;;;;;;;;;;;

(require 'anything-config)

(require 'autopair)
(autopair-global-mode t)
(setq autopair-autowrap t)
(require 'undo-tree)
(global-undo-tree-mode t)

;;;;;;;;;;;;;;;; VIM STA ;;;;;;;;;;;;;;;;;;

(require 'vimpulse)
(require 'viper-in-more-modes)

; turns it off in unwanted places
(require 'linum-off)

; right adjust and add blank on right
(setq linum-format
      (lambda (line)
        (propertize (format
                      (let ((w (length (number-to-string
                                         (count-lines (point-min) (point-max))))))
                        (concat "%" (number-to-string w) "d "))
                      line)
                    'face 'linum)))

(global-linum-mode 1)
;(require 'vimpulse-relative-linum)
(require 'vimpulse-operator-comment)

;;;;;;;;;;;;;;;; VIM END ;;;;;;;;;;;;;;;;;;

(blink-cursor-mode 1)
(defun uncamel (s &optional sep start)
 (interactive)
  "Convert CamelCase string S to lower case with word separator SEP.
  Default for SEP is a hyphen \"-\".
  If third argument START is non-nil, convert words after that
  index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
           (setq s (replace-match (concat (or sep "_")
                                          (downcase (match-string 0 s)))
                                  t nil s)))
    (downcase s)))
;;;;;; Camel Case ;;;;;;;
(defun mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
     (if list
       (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))
(defun camelize (s)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                         '(lambda (word) (capitalize (downcase word)))
                         (split-string s "_")) ""))
(defun camelize-method (s)
  "Convert under_score string S to camelCase string."
  (mapconcat 'identity (mapcar-head
                         '(lambda (word) (downcase word))
                         '(lambda (word) (capitalize (downcase word)))
                         (split-string s "_")) ""))

(require 'ecb)
(setq ecb-tip-of-the-day nil)

;;; IDO

(require 'ido)
;; stolen from emacs-fu.blogspot.com
(ido-mode 'both) ;; for buffers and files
(setq
  ido-everwhere t
  idosave-directory-list-file "~/.emacs.d/cache/ido.last"

  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*ECB*" "^\*")
  ido-work-directory-list '("~/ps/")
  ido-case-fold  t                 ; be case-insensitive

  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point 'guess ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ido-create-new-buffer 'always    ; create buf for no match
  ido-enable-flex-matching t
  ido-max-prospects 10              ; don't spam my minibuffer
  ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\" return.")

;; elisp, return
(defun electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match' then
open and indent an empty line between the cursor and the text.  Move the
cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

;; TODO remove other paredit mode invotations
                                        ; (autoload 'paredit-mode "paredit"
                                        ; "Minor mode for pseudo-structurally editing Lisp code." t)
(when (fboundp 'paredit-mode)
  (mapc (lambda (hook)
          (add-hook hook (lambda ()
                           (paredit-mode +1)
                           ;; TODO this my be bad.. mkay
                           (vimpulse-imap (kbd "RET") 'electrify-return-if-match)
                           (show-paren-mode t))))
        '(emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook slime-repl-mode-hook)))

; (add-hook 'el-mode-hook
;         (lambda () (local-set-key [return] 'newline-and-indent)))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))

;;; SCALA

(require 'scala-mode-auto)

(defun electrify-return-if-match-scala (arg)
  "If the text after the cursor matches `electrify-return-match' then
open and indent an empty line between the cursor and the text.  Move the
cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
        (save-excursion (setq last-command nil) (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(add-hook 'el-mode-hook 'highlight-fixmes-mode)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'highlight-80+-mode)
(add-hook 'scala-mode-hook 'idle-highlight)
(defun me-turn-off-indent-tabs-mode ()
  (setq indent-tabs-mode nil))
(add-hook 'scala-mode-hook 'me-turn-off-indent-tabs-mode)
(add-hook 'scala-mode-hook 'hs-minor-mode)
(add-hook 'scala-mode-hook 'subword-mode)
(add-hook 'scala-mode-hook 'autopair-mode)

; highlighting for TODO
(require 'highlight-fixmes-mode)
(add-hook 'scala-mode-hook 'highlight-fixmes-mode)

(add-hook 'scala-mode-hook 'hl-line-mode)
(add-hook 'scala-mode-hook
 (lambda ()
   (define-key scala-mode-map (kbd "C-n") 'ensime-forward-note)
   (define-key scala-mode-map (kbd "C-p") 'ensime-backward-note)
   (define-key scala-mode-map (kbd "M-q") 'c-fill-paragraph)
))
;; reclaim some binding used by shell mode and shell-command.
;; the shell mode and associated mode and commands use keys in comint-mode-map.
(add-hook 'comint-mode-hook
 (lambda ()
   ;; rebind displaced commands that i still want a key
   (define-key comint-mode-map (kbd "TAB") 'comint-dynamic-complete)
   (define-key comint-mode-map (kbd "<up") 'comint-previous-input)
   (define-key comint-mode-map (kbd "<down>") 'comint-next-input)))

(add-hook 'ensime-sbt-mode-hook (lambda () (setq left-fringe-width 5)))
;; ECB support
;(require 'ensime-ecb)
;;;;;;;;;;;;;;; END Scala ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;
(global-set-key "\C-c\C-w" 'backward-kill-word)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(menu-bar-mode 1)

;; Session
;(require 'session)
;(add-hook 'after-init-hook 'session-initialize)

;;;;;;;;;;;;;;;;;;; KEY BINDINGS ;;;;;;;;;;;;;;;;;;;;;;

(defun maximize-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))

; Window Movement
(winner-mode 1)
(windmove-default-keybindings 'meta)

(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))

(global-set-key (kbd "C-c v") 'halve-other-window-height)

;; TODO try these?
; open file
;(global-set-key [(super o)] 'find-file)

; use full-ack for Find
;(global-set-key [(super F)] 'ack)

; close window
(global-set-key (kbd "s-w") 'kill-buffer-and-window)
(global-set-key (kbd "s-W") 'kill-this-buffer)

; navigating through errors
(global-set-key [(meta j)] 'next-error)
(global-set-key [(meta k)] 'previous-error)

; magit
(global-set-key (kbd "C-c i") 'magit-status)

;; os x cust
; use default Mac browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

; delete files by moving them to the OS X trash
(setq delete-by-moving-to-trash t)

; run macro
(global-set-key [f6] 'call-last-kbd-macro)
(global-set-key (kbd "C-/") 'toggle-kbd-macro-recording-on)

(autoload 'idomenu "idomenu" nil t)

(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(global-set-key [(meta f12)] 'recentf-open-files)

; nicer naming of buffers with identical names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

; kill ring browsing
(require 'browse-kill-ring+)
(browse-kill-ring-default-keybindings)
;; popup menu
;(global-set-key "\C-cy" '(lambda ()
;   (interactive)
;   (popup-menu 'yank-menu)))

; automatically clean up old buffers
(require 'midnight)

(require 'unbound)

; pick up changes to files on disk automatically (ie, after git pull)
(global-auto-revert-mode 1)

; don't confirm opening non-existant files/buffers
(setq confirm-nonexistent-file-or-buffer nil)

; yes, I want to kill buffers with processes attached
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))


;; ---------------------------------------
;; load elscreen
;; ---------------------------------------
;; F9 creates a new elscreen, shift-F9 kills it
(global-set-key (kbd "s-t"  ) 'elscreen-create)
(global-set-key (kbd "s-d"  ) 'elscreen-kill)

;; Windowskey+PgUP/PgDown switches between elscreens
(global-set-key (kbd "<C-prior>") 'elscreen-previous)
(global-set-key (kbd "<C-next>")  'elscreen-next)

(defun elscreen-frame-title-update ()
   (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
     (let* ((screen-list (sort (elscreen-get-screen-list) '<))
           (screen-to-name-alist (elscreen-get-screen-to-name-alist))
           (title (mapconcat
                   (lambda (screen)
                     (format "%d%s %s"
                             screen (elscreen-status-label screen)
                               (replace-regexp-in-string "\*.*\*" ""
                                (get-alist screen screen-to-name-alist))))
                   screen-list " ")))

       (if (fboundp 'set-frame-name)
          (set-frame-name title)
        (setq frame-title-format title)))))

; (eval-after-load "elscreen"
;   '(add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update))

(load "elscreen" "ElScreen" t)
(require 'elscreen-buffer-list)

;; show column #
(column-number-mode t)
(line-number-mode nil)

;; surround
(require 'vimpulse-surround)

(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(setq term-default-bg-color "#1f1f1f")
(setq term-default-fg-color "#dcdccc")
;; only needed if you use autopair
(add-hook 'term-mode-hook
  #'(lambda () (setq autopair-dont-activate t)))


(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

(add-hook 'emacs-lisp-mode-hook
  (lambda()
    (setq mode-name "el")))

;; Major modes
(add-hook 'scala-mode-hook
  (lambda()
    (setq mode-name "S")))

; Paren mode faces
(require 'paren)
(set-face-background 'show-paren-match-face "#0F4E8B")
(set-face-foreground 'show-paren-match-face "#dcdccc")

;; TODO make this scala only
(define-key (current-global-map) [remap vimpulse-jump-to-tag-at-point] 'ensime-edit-definition)

;(define-key (current-global-map) [remap viper-forward-word] 'forward-word)
;(define-key (current-global-map) [remap viper-backward-word] 'backward-word)
;(define-key (current-global-map) [remap vimpulse-operator-backward-word] 'backward-word)
(define-key (current-global-map) [remap vimpulse-operator-forward-word] 'subword-forward)
;(define-key (current-global-map) [remap vimpulse-operator-forward-word] 'viper-forward-word)

(setq campfire-room-name "API")
(setq campfire-room-id "188551")

; Spelling
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)
(add-hook 'scala-mode-hook 'flyspell-prog-mode)

(add-hook 'flyspell-mode-hook
          '(lambda ()
             (define-key flyspell-mode-map (kbd "C-;") 'save-buffer)))
(global-set-key (kbd "C-;") 'save-buffer)

(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-process-directory (expand-file-name "~/"))

;;;;;;;;;;;;;;;;;; ERC END ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-something-to-mode-hooks (mode-list something)
  "helper function to add a callback to multiple hooks"
  (dolist (mode mode-list)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))

;(add-something-to-mode-hooks '(c++ scala emacs-lisp) 'turn-on-fic-ext-mode)

; TODO img for campfire

;; Redefine the 8 primary terminal colors to look good against black ;; set in zenburn
(setq ansi-term-color-vector
[unspecified "#000000" "#963F3C" "#5FFB65" "#FFFD65"
"#0082FF" "#FF2180" "#57DCDB" "#FFFFFF"])

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


(setq initial-scratch-message nil)

;; For teh tunez
(autoload 'pianobar "pianobar" nil t)
(require 'switchy)

;; Viper is overreaching by caring whether a visited file is under version
;; ;; control -- disable this check.
(defadvice viper-maybe-checkout (around viper-vcs-check-is-retarded activate)
   nil)

;(setq viper-custom-file-name
;       (convert-standard-filename "~/.emacs.d/jz/dot-viper.el"))
(defun jao-toggle-selective-display (column)
  "Activate selective display based on the column at point"
   (interactive "P")
     (set-selective-display
         (if selective-display nil (or column 4))))


(setq woman-use-own-frame nil)     ; don't create new frame for manpages
(setq woman-use-topic-at-point t)  ; don't prompt upon K key (manpage display)

;; multi buffer occur
(require 'color-moccur)
(require 'moccur-edit)
(defadvice moccur-edit-change-file
  (after save-after-moccur-edit-buffer activate)
  (save-buffer))

; TODO set fav list, etc for moccur

;; MAPPINGS
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-key my-keys-minor-mode-map (kbd "M-;") 'repeat-complex-command)

;; Movement
;; TODO write utility to remove duplication
(define-key my-keys-minor-mode-map (kbd "C-w C-h") 'windmove-left)
(define-key my-keys-minor-mode-map (kbd "C-w C-l") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "C-w C-k") 'windmove-up)
(define-key my-keys-minor-mode-map (kbd "C-w C-j") 'windmove-down)

(define-key my-keys-minor-mode-map (kbd "C-w h") 'windmove-left)
(define-key my-keys-minor-mode-map (kbd "C-w l") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "C-w k") 'windmove-up)
(define-key my-keys-minor-mode-map (kbd "C-w j") 'windmove-down)

(define-key my-keys-minor-mode-map (kbd "s-l") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "s-k") 'windmove-up)
(define-key my-keys-minor-mode-map (kbd "s-j") 'windmove-down)
(define-key my-keys-minor-mode-map (kbd "s-SPC") 'delete-window)

(define-key my-keys-minor-mode-map (kbd "C-w ,") '(lambda () (interactive) (split-window-vertically) (other-window 1)))
(define-key my-keys-minor-mode-map (kbd "C-w .") '(lambda () (interactive) (split-window-horizontally) (other-window 1)))
(define-key my-keys-minor-mode-map (kbd "s-,") '(lambda () (interactive) (split-window-vertically) (other-window 1)))
(define-key my-keys-minor-mode-map (kbd "s-.") '(lambda () (interactive) (split-window-horizontally) (other-window 1)))
(define-key my-keys-minor-mode-map (kbd "C-SPC") 'delete-window)
(define-key my-keys-minor-mode-map (kbd "<C-tab>") 'other-frame)
(define-key my-keys-minor-mode-map (kbd "C-w ;") 'rotate-windows)
(define-key my-keys-minor-mode-map (kbd "C-w C-;") 'rotate-windows)
(define-key my-keys-minor-mode-map (kbd "C-w e") 'balance-windows)
(define-key my-keys-minor-mode-map (kbd "C-w C-e") 'balance-windows)
(defun swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window  (selected-window))
             (this-buffer  (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start   (window-start this-window))
             (other-start  (window-start other-window)))
        (set-window-buffer this-window  other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start  this-window  other-start)
        (set-window-start  other-window this-start)))))

(define-key my-keys-minor-mode-map (kbd "C-s-J") (lambda () (interactive) (swap-with 'down)))
(define-key my-keys-minor-mode-map (kbd "C-s-K") (lambda () (interactive) (swap-with 'up)))
(define-key my-keys-minor-mode-map (kbd "C-s-H") (lambda () (interactive) (swap-with 'left)))
(define-key my-keys-minor-mode-map (kbd "C-s-L") (lambda () (interactive) (swap-with 'right)))

(define-key my-keys-minor-mode-map (kbd "s-J") (lambda () (interactive) (enlarge-window 1)))
(define-key my-keys-minor-mode-map (kbd "s-K") (lambda () (interactive) (enlarge-window -1)))
(define-key my-keys-minor-mode-map (kbd "s-H") (lambda () (interactive) (enlarge-window -1 t)))
(define-key my-keys-minor-mode-map (kbd "s-L") (lambda () (interactive) (enlarge-window 1 t)))


(define-key my-keys-minor-mode-map (kbd "C-c o") 'rename-file-and-buffer)
(define-key my-keys-minor-mode-map (kbd "C-c g") 'customize-group)
(define-key my-keys-minor-mode-map (kbd "C-c C-l") 'dired-jump-other-window)
(define-key my-keys-minor-mode-map (kbd "C-l") 'dired-jump)
(define-key my-keys-minor-mode-map (kbd "C-b") 'ido-switch-buffer)
(define-key my-keys-minor-mode-map (kbd "C-x b") 'display-buffer)

(define-key my-keys-minor-mode-map (kbd "C-f d") 'moccur-grep)
(define-key my-keys-minor-mode-map (kbd "C-f h") 'moccur-grep-find)
(define-key my-keys-minor-mode-map (kbd "C-f g") 'dmoccur)
(define-key my-keys-minor-mode-map (kbd "C-f s") 'ack-same)
(define-key my-keys-minor-mode-map (kbd "C-f a") 'ack)
(define-key my-keys-minor-mode-map (kbd "C-f f") 'find-file-in-project)
(define-key my-keys-minor-mode-map (kbd "C-f r") 'replace-regexp)
(define-key my-keys-minor-mode-map (kbd "C-f l") 'lazy-search-menu)
(define-key my-keys-minor-mode-map (kbd "C-f p") 'find-grep-dired)
(define-key my-keys-minor-mode-map (kbd "C-f n") 'find-name-dired)
(define-key my-keys-minor-mode-map (kbd "C-f o") 'dired-do-moccur)
(define-key my-keys-minor-mode-map (kbd "C-f i") 'ibuffer-do-occur)
(define-key my-keys-minor-mode-map (kbd "C-f c") 'file-cache-refresh)
(define-key my-keys-minor-mode-map (kbd "C-f j") 'file-cache-ido-find-file)
(define-key my-keys-minor-mode-map (kbd "C-f k") 'recentf-ido-find-file)

;; searching
(define-key my-keys-minor-mode-map (kbd "C-c d") 'ediff-revision)
(define-key my-keys-minor-mode-map (kbd "M-i") 'google-search-selection)
(define-key my-keys-minor-mode-map (kbd "s-i") 'google-it)

(define-key my-keys-minor-mode-map (kbd "C-c s") 'confluence-search)

(fset 'yank-to-end
   "y$")

(fset 'surround-paren
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([105 40 escape 108 100 105 119 84 40 105 25 escape 37 105] 0 "%d")) arg)))

(fset 'surround-square
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([105 91 escape 108 100 105 119 84 91 105 25 escape 37 105] 0 "%d")) arg)))

(fset 'surround-brace
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([105 123 escape 108 100 105 119 84 123 105 25 escape 37 105] 0 "%d")) arg)))

(defun minimap-toggle ()
  "Show minimap if hidden, hide if present."
  (interactive)
  (if (and minimap-bufname
	       (get-buffer minimap-bufname)
	       (get-buffer-window (get-buffer minimap-bufname)))
      (minimap-kill)
    (minimap-create))
  )

(vimpulse-map (kbd ",c") 'surround-brace)
(vimpulse-map (kbd ",b") 'surround-paren)
(vimpulse-map (kbd ",s") 'surround-square)
(vimpulse-map (kbd "S-C-y") 'viper-scroll-down-one)
(vimpulse-map (kbd "<up>") 'comint-previous-input 'comint-mode)
(vimpulse-map (kbd "<down>") 'comint-next-input 'comint-mode)

(vimpulse-imap (kbd "RET") 'reindent-then-newline-and-indent)

(vimpulse-imap (kbd "RET") 'electrify-return-if-match-scala 'scala-mode)
(vimpulse-imap (kbd "C-SPC") 'auto-complete 'scala-mode)

(vimpulse-map (kbd "&") 'lazy-search-menu)
(vimpulse-vmap (kbd "&") 'lazy-search-menu)
(vimpulse-vmap (kbd "Q") 'query-replace-regexp)
(vimpulse-map (kbd "Q") 'query-replace-regexp)

(vimpulse-map (kbd "C-b") 'ido-switch-buffer)
(vimpulse-map (kbd "Y") 'yank-to-end)
(vimpulse-map (kbd "A") 'viper-append)
(vimpulse-map (kbd "a") 'viper-Append)

; aligh by equal sign
(fset 'equal-align
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([118 105 112 61 61 return] 0 "%d")) arg)))

(fset 'sort-parag
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("vip]" 0 "%d")) arg)))

(vimpulse-map (kbd "M-a") 'equal-align) ;; bound to backward-sentence
(vimpulse-map (kbd "s-s") 'sort-parag) ;; bound to save buffer
(vimpulse-vmap (kbd "=") 'align-regexp)
(vimpulse-imap (kbd "C-y") 'yank)

(vimpulse-vmap (kbd "TAB") 'vimpulse-shift-right)
(vimpulse-vmap (kbd "<S-tab>") 'vimpulse-shift-left)
;; TODO unbind , C-e

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " M " 'my-keys-minor-mode-map)

(define-key dired-mode-map "j" 'dired-next-line)
(define-key dired-mode-map "J" 'dired-goto-file)
(define-key dired-mode-map "k" 'dired-previous-line)
(define-key dired-mode-map "K" 'dired-do-kill-lines)

(my-keys-minor-mode 1)

; Cleanup mode line
;; Minor modes
(when (require 'diminish nil 'noerror)
  (eval-after-load "Undo-Tree"
      '(diminish 'undo-tree-mode "U"))
  (eval-after-load "pair"
    '(diminish 'autopair-mode "P"))
  (eval-after-load "fixme"
    '(diminish 'highlight-fixmes-mode "F"))
  (eval-after-load "yasnippet"
    '(diminish 'yas/minor-mode "Y")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie expand.  Groovy vans with tie-dyes.

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat user-dir "/auto-complete/dict"))
(ac-config-default)

(setq ac-dwim t)
(setq ac-expand-on-auto-complete t)
(setq ac-ignore-case 'smart)
(setq ac-delay 0.5)
(setq ac-auto-start nil)
(setq ac-use-comphist t)
(setq ac-use-quick-help t)
(setq ac-delete-dups t)
(setq ac-use-fuzzy t)
(setq ac-auto-show-menu t)

(add-to-list 'ac-modes 'scala-mode)
(add-to-list 'ac-modes 'confluence-mode)

(define-key ac-completing-map [return] 'ac-complete)
(define-key ac-completing-map (kbd "<S-tab>") 'ac-previous)
(define-key ac-completing-map (kbd "ESC") 'ac-stop)
(define-key ac-completing-map (kbd "C-SPC") 'ac-expand)

(ac-set-trigger-key "TAB")

;; Change the default hippie-expand order and add yasnippet to the front.
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-whole-kill))

(global-set-key (kbd "TAB") 'hippie-expand)
;; Enables tab completion in the `eval-expression` minibuffer
(defun hippie-unexpand ()
 (interactive)
 (hippie-expand 0))
(define-key minibuffer-local-map (kbd "TAB") 'hippie-expand)
(define-key minibuffer-local-map  (kbd "<S-tab>") 'hippie-unexpand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet

(require 'ensime)
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat user-dir "/yasnippet-read-only/snippets"))

;; Speed up birdcage
;; (setenv "SBT_INTRANSITIVE" "1")
(setenv "NO_PROJECT_DEPS" "1")

(add-hook 'scala-mode-hook 'yas/minor-mode-on)
(yas/global-mode 1)

(setq yas/trigger-key (kbd "C-c <kp-multiply>"))

;; BUG DONT COMPILE YASNIPPET!!!!!!!!!!
; Replace yasnippets's TAB
; (add-hook 'yas/minor-mode-hook
; (lambda () (define-key yas/minor-mode-map
;       (kbd "TAB") 'auto-complete )))   ; was yas/expand

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; onekey
;;
(vimpulse-map (kbd "C-e") 'one-key-menu-ensime 'scala-mode)
;(vimpulse-map (kbd "C-f") 'one-key-menu-find)

(vimpulse-map (kbd "SPC") 'confluence-get-page-at-point 'confluence-mode)
(vimpulse-map (kbd "C-e") 'one-key-menu-confluence 'confluence-mode)
(vimpulse-map [backspace] 'confluence-pop-tag-stack 'confluence-mode)
(vimpulse-imap [return] 'confluence-newline-and-indent 'confluence-mode)

(vimpulse-map (kbd "TAB") 'confluence-list-indent-dwim 'confluence-mode)
(vimpulse-map (kbd "<S-tab>") '(lambda () (interactive) (confluence-list-indent-dwim -1)) 'confluence-mode)
;; (vimpulse-imap (kbd "TAB") 'auto-complete 'confluence-mode)
(vimpulse-imap (kbd "TAB") 'confluence-list-indent-dwim 'confluence-mode)
(vimpulse-imap (kbd "<S-tab>") '(lambda () (interactive) (confluence-list-indent-dwim -1)) 'confluence-mode)

(vimpulse-map (kbd "C-k") 'viper-backward-paragraph)
(vimpulse-map (kbd "C-j") 'viper-forward-paragraph)
(vimpulse-map (kbd "C-i") 'vimpulse-jump-forward)

(vimpulse-map (kbd "K") 'viper-paren-match)
(vimpulse-vmap (kbd "]") 'sort-lines)

(vimpulse-vmap (kbd "m") 'apply-macro-to-region-lines)

(vimpulse-map ";" 'viper-ex)
(vimpulse-vmap ";" 'vimpulse-visual-ex)
(vimpulse-map (kbd "SPC") 'hs-toggle-hiding)
(vimpulse-vmap (kbd "SPC") 'vimpulse-indent)
;(vimpulse-map (kbd "SPC") 'vimpulse-indent)

(defun save-sbt-action (string)
  (save-buffer)
  (ensime-sbt-action string))

(vimpulse-map "b" 'backward-word)

;; Scala stuff

(vimpulse-map (kbd ",,") 'switch-between-test-and-source 'scala-mode)
;(vimpulse-map (kbd "C-m") 'call-last-kbd-macro) ;; This seems to intercept Enter

; SBT
(vimpulse-map (kbd ",p") '(lambda () (interactive) (save-sbt-action "project gluebird"))  'scala-mode)
(vimpulse-map (kbd ",.") '(lambda () (interactive) (save-sbt-action "compile"))  'scala-mode)
(vimpulse-map (kbd ",m") '(lambda () (interactive) (save-sbt-action (concat "test-only " (get-spec-class)))) 'scala-mode)
(vimpulse-map (kbd ",a") '(lambda () (interactive) (save-sbt-action "test")) 'scala-mode)
(vimpulse-map (kbd ",j") '(lambda () (interactive) (save-sbt-action "test-quick")) 'scala-mode)
(vimpulse-map (kbd ",f") '(lambda () (interactive) (save-sbt-action "test-failed")) 'scala-mode)
(vimpulse-map (kbd ",k") '(lambda () (interactive) (save-sbt-action "test")) 'scala-mode)
(vimpulse-map (kbd ",l") '(lambda () (interactive) (save-sbt-action "!!")) 'scala-mode)
(vimpulse-map (kbd ",t") '(lambda () (interactive) (save-sbt-action "console")) 'scala-mode)
(vimpulse-map (kbd ",u") '(lambda () (interactive) (save-sbt-action "update")) 'scala-mode)
(vimpulse-map (kbd ",n") '(lambda () (interactive) (save-sbt-action "; clean ; update ; compile-thrift-java ; compile")) 'scala-mode)

; Browsing cgit
(vimpulse-map (kbd ",y") '(lambda () (interactive) (cgit-yank t)) 'scala-mode)
(vimpulse-map (kbd ",Y") '(lambda () (interactive) (cgit-yank)) 'scala-mode)
(vimpulse-map (kbd ",v") '(lambda () (interactive) (cgit-browse t)) 'scala-mode)
(vimpulse-map (kbd ",V") '(lambda () (interactive) (cgit-browse)) 'scala-mode)

(vimpulse-map (kbd ",g") 'magit-status)
(vimpulse-map (kbd ",/") 'minimap-toggle)
(vimpulse-map (kbd ",r") 'jao-toggle-selective-display)

;; search functions
(vimpulse-map (kbd "C-f d") 'moccur-grep)
(vimpulse-map (kbd "C-f h") 'moccur-grep-find)
(vimpulse-map (kbd "C-f g") 'dmoccur)
(vimpulse-map (kbd "C-f s") 'ack-same)
(vimpulse-map (kbd "C-f a") 'ack)
(vimpulse-map (kbd "C-f f") 'find-file-in-project)
(vimpulse-map (kbd "C-f r") 'replace-regexp)
(vimpulse-map (kbd "C-f l") 'lazy-search-menu)
(vimpulse-map (kbd "C-f p") 'find-grep-dired)
(vimpulse-map (kbd "C-f n") 'find-name-dired)
(vimpulse-map (kbd "C-f o") 'dired-do-moccur)
(vimpulse-map (kbd "C-f i") 'ibuffer-do-occur)
(vimpulse-map (kbd "C-f c") 'file-cache-refresh)
(vimpulse-map (kbd "C-f j") 'file-cache-ido-find-file)
(vimpulse-map (kbd "C-f k") 'recentf-ido-find-file)

;; search functions
(vimpulse-imap (kbd "C-f d") 'moccur-grep)
(vimpulse-imap (kbd "C-f h") 'moccur-grep-find)
(vimpulse-imap (kbd "C-f g") 'dmoccur)
(vimpulse-imap (kbd "C-f s") 'ack-same)
(vimpulse-imap (kbd "C-f a") 'ack)
(vimpulse-imap (kbd "C-f f") 'find-file-in-project)
(vimpulse-imap (kbd "C-f r") 'replace-regexp)
(vimpulse-imap (kbd "C-f l") 'lazy-search-menu)
(vimpulse-imap (kbd "C-f p") 'find-grep-dired)
(vimpulse-imap (kbd "C-f n") 'find-name-dired)
(vimpulse-imap (kbd "C-f o") 'dired-do-moccur)
(vimpulse-imap (kbd "C-f i") 'ibuffer-do-occur)

(define-key vimpulse-visual-basic-map "v" 'end-of-line)

(vimpulse-define-text-object vimpulse-sexp (arg)
  "Select a S-expression."
  :keys '("ae" "ie")
  (vimpulse-inner-object-range
   arg
   'backward-sexp
   'forward-sexp))

(eval-after-load "menu-bar" '(require 'menu-bar+))

;; Ediff

(setq ediff-split-window-function (lambda (&optional arg)
                                    (if (> (frame-width) 150)
                                      (split-window-horizontally arg)
                                      (split-window-vertically arg))))
(setq debug-on-error t)

;(remove-hook 'minibuffer-setup-hook 'viper-minibuffer-setup-sentinel)
;(defadvice viper-set-minibuffer-overlay (around vimpulse activate) nil)
;(defadvice viper-has-face-support-p (around vimpulse activate) nil)
;(define-key minibuffer-local-map (kbd "ESC") 'abort-recursive-edit)

;;; change cursor to bar in minibuffer
;(add-hook 'minibuffer-setup-hook '(lambda () (setq cursor-type 'bar)))

(setq ensime-jvm-args "-server -verbose:class -verbosegc -Xloggc:/tmp/ensime_gc.log -XX:+PrintGCDetails -XX:+PrintGCTimeStamps -XX:+PrintGCDateStamps -XX:+PrintTenuringDistribution -XX:+PrintHeapAtGC -XX:+UseConcMarkSweepGC -XX:+UseParNewGC -XX:+UseAdaptiveSizePolicy -Xms256M -Xmx2048M -Dfile.encoding=UTF-8")
(setenv "ENSIME_JVM_ARGS" ensime-jvm-args)

; Fullscreen
(global-set-key (kbd "<s-return>") 'ns-toggle-fullscreen)

(add-hook 'ido-setup-hook '(lambda ()
                             (define-key ido-completion-map (kbd "C-c p") 'ido-toggle-prefix)
                             (define-key ido-completion-map (kbd "C-c c") 'ido-toggle-case)
                             (define-key ido-completion-map (kbd "C-c t") 'ido-toggle-regexp)
                             (define-key ido-completion-map (kbd "C-c e") 'ido-edit-input)
                             (define-key ido-completion-map (kbd "C-k") 'ido-next-match)
                             (define-key ido-completion-map (kbd "C-d") 'ido-kill-buffer-at-head)
                             (define-key ido-completion-map (kbd "C-c o") 'ido-copy-current-file-name)
                             (define-key ido-completion-map [remap viper-intercept-ESC-key] 'abort-recursive-edit)))

;(add-hook 'scala-mode-hook 'viper-mode)

 (defun my-func-switch-to-erc-buffer nil
       "Switch to ERC buffer using IDO to choose which one, or start ERC if not already started."
       (interactive)
       (let (final-list (list ))
         (dolist (buf (buffer-list) final-list)
           (if (equal 'erc-mode (with-current-buffer buf major-mode))
     	  (setq final-list (append (list (buffer-name buf)) final-list))))
         (if final-list
     	(switch-to-buffer (ido-completing-read "Buffer: " final-list))
           (call-interactively 'erc))))

(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

(defun delete-file-and-buffer ()
   "Deletes the current file and buffer"
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p "Really delete this file?")
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun word-count ()
  "Count words in buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

;(global-set-key (kbd "C-c k") 'ecb-toggle-ecb-windows)
(global-set-key (kbd "C-c k") 'delete-file-and-buffer)
(global-set-key (kbd "C-c l") 'revert-buffer)

;(global-set-key (kbd "M-w") 'subword-forward)
; Use up m-W
;(global-set-key (kbd "C-c ;") 'ensime-ecb)
;


(autoload 'formfeed-hline-mode "formfeed-hline" nil t)
(formfeed-hline-mode 1)
