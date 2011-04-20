(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq user-dir (concat dotfiles-dir user-login-name))

(add-to-list 'load-path (concat user-dir "/elisp"))
(add-to-list 'load-path (concat user-dir "/apel-10.8"))
(add-to-list 'load-path (concat user-dir "/yasnippet-read-only"))
(add-to-list 'load-path (concat user-dir "/ensime_2.8.1-0.5.0/elisp"))
(add-to-list 'load-path (concat user-dir "/vimpulse"))
(add-to-list 'load-path (concat user-dir "/vimpulse-surround"))
(add-to-list 'load-path (concat user-dir "/vimpulse-plugins"))
(add-to-list 'load-path (concat user-dir "/viper-in-more-modes"))
(add-to-list 'load-path (concat user-dir "/ecb"))
(add-to-list 'load-path (concat user-dir "/scala"))
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

;(add-to-list 'load-path (concat user-dir "/scamacs/scamacs"))
;(add-to-list 'load-path (concat user-dir "/scamacs/ecb"))

(setq exec-path (append exec-path '("/usr/local/bin")))

;; twittering-mode
(require 'twittering-mode)
(setq twittering-use-master-password t)
(setq twittering-initial-timeline-spec-string
      '(":home"
        ":replies"
        ":favorites"
        ":direct_messages"))

(add-hook 'twittering-mode-hook
   (lambda ()
     (mapc (lambda (pair)
             (let ((key (car pair))
                   (func (cdr pair)))
               (define-key twittering-mode-map
                 (read-kbd-macro key) func)))
           '(("F" . twittering-friends-timeline)
             ("R" . twittering-replies-timeline)
             ("U" . twittering-user-timeline)
             (";" . twittering-home-timeline)
             ("W" . twittering-update-status-interactive)))))

(setq twittering-icon-mode t)                ; Show icons
(setq twittering-timer-interval 300)         ; Update your timeline each 300 seconds (5 minutes)
(setq twittering-url-show-status nil)        ; Keeps the echo area from showing all the http processes

(add-hook 'twittering-new-tweets-hook (lambda ()
(let ((n twittering-new-tweets-count))
(start-process "twittering-notify" nil "notify-send"
            "-i" "/usr/share/pixmaps/gnome-emacs.png"
            "New tweets"
            (format "You have %d new tweet%s"
                    n (if (> n 1) "s" ""))))))

(add-hook 'twittering-edit-mode-hook (lambda () (ispell-minor-mode) (flyspell-mode)))

(autoload 'twittering-numbering "twittering-numbering" nil t)
(add-hook 'twittering-mode-hook 'twittering-numbering)

;; M-x IDO
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'mouse+)
(require 'google-search)

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

;--------------------------------------------------------------------------
;; popwin.el
;;--------------------------------------------------------------------------
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq special-display-function 'popwin:special-display-popup-window)

(push '("*Shell Command Output*" :height 20) popwin:special-display-config)
;(push '("*Shell Command Output*" :height 20 :position top) popwin:special-display-config)

(setq anything-samewindow nil)
(push '("*anything*" :height 20) popwin:special-display-config)
(push '("*anything for files*" :height 20) popwin:special-display-config)

(push '(".*ensime-sbt.*" :regexp t :height 30 :position bottom) popwin:special-display-config)
(push '("*pianobar*" :width 60 :position right) popwin:special-display-config)
(push '("*ENSIME-Compilation-Result*" :height 50 :position bottom) popwin:special-display-config)
(push '("*ensime-inferior-scala*" :width 60 :position right) popwin:special-display-config)

(push '("*scratch*") popwin:special-display-config)
(push '("*viper-info*") popwin:special-display-config)
(push '("*magit: macaw") popwin:special-display-config)
(push '("*Messages*") popwin:special-display-config)
(push '("svnlog.txt") popwin:special-display-config)
(push '("journal.txt" :regexp t) popwin:special-display-config)
(push '("*grep*" :height 50) popwin:special-display-config)
(push '("*Kill Ring*" :height 30) popwin:special-display-config)
(push '("*Inspector*" :width 60 :position right) popwin:special-display-config)
(push '(dired-mode :position right :width 70) popwin:special-display-config) ; dired-jump-other-window (C-x 4 C-j)
(push '("*Warnings*") popwin:special-display-config)
(push '("*Help*" :height 30 :position bottom) popwin:special-display-config)
(push '("*Completions*" :height 30 :position bottom) popwin:special-display-config)

;(push '("*ack*" :height 40 :position bottom) popwin:special-display-config)
;(push '("*Moccur*" :height 20 :width 80 :position right) popwin:special-display-config)

;; save a list of open files in ~/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 100)
                (file-name-history        . 100)
                (grep-history             . 100)
                (compile-history          . 100)
                (minibuffer-history       . 100)
                (query-replace-history    . 100)
                (read-expression-history  . 100)
                (regexp-history           . 100)
                (regexp-search-ring       . 100)
                (search-ring              . 100)
                (shell-command-history    . 100)
                tags-file-name
                register-alist)))

;; auto save desktop on emacs idle
(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))

(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

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
  (define-key dired-mode-map "c" 'dired-do-copy))


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
(defcustom semantic-ectag-program "/opt/local/bin/ctags"
  "The Exuberent CTags program to use."
  :group 'semantic
  :type 'program)

(semantic-mode 1)

(global-ede-mode t)

(if (boundp 'semantic-load-enable-excessive-code-helpers)
    ; Add-on CEDET
    (progn
      (semantic-load-enable-excessive-code-helpers)
      ; TODO: should already be enabled by previous line
      (global-semantic-idle-completions-mode)
      (global-semantic-tag-folding-mode))
   ; Integrated CEDET
  (setq semantic-default-submodes
        '(global-semanticdb-minor-mode
          global-semantic-idle-scheduler-mode
          global-semantic-idle-summary-mode
          global-semantic-idle-completions-mode
          global-semantic-decoration-mode
          global-semantic-highlight-func-mode
          global-semantic-stickyfunc-mode)))

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

(vimpulse-map ";" 'viper-ex)
(vimpulse-vmap ";" 'vimpulse-visual-ex)
(vimpulse-map (kbd "SPC") 'hs-toggle-hiding)
(vimpulse-map "?" 'describe-bindings)
(define-key vimpulse-visual-basic-map "v" 'end-of-line)
;(define-key vimpulse-visual-basic-map "-" 'comment-dwim)

(vimpulse-define-text-object vimpulse-sexp (arg)
  "Select a S-expression."
  :keys '("ae" "ie")
  (vimpulse-inner-object-range
   arg
   'backward-sexp
   'forward-sexp))

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
(require 'vimpulse-relative-linum)
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

;; toggles
(add-hook 'ido-minibuffer-setup-hook
 (lambda ()
   (local-set-key (kbd "C-c p") 'ido-toggle-prefix) ;; same as in isearch
))

;;; SCALA

(require 'scala-mode-auto)

(add-hook 'scala-mode-hook
  (lambda ()
    (local-set-key [return] '(lambda () (interactive) (setq last-command nil) (newline-and-indent))))
    (local-set-key (kbd "C-c j") 'ensime-sbt-switch)
  )

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'highlight-80+-mode)
(defun me-turn-off-indent-tabs-mode ()
  (setq indent-tabs-mode nil))
(add-hook 'scala-mode-hook 'me-turn-off-indent-tabs-mode)
(add-hook 'scala-mode-hook 'hs-minor-mode)
;(add-hook 'scala-mode-hook 'camelCase-mode)
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
))
;; reclaim some binding used by shell mode and shell-command.
;; the shell mode and associated mode and commands use keys in comint-mode-map.
(add-hook 'comint-mode-hook
 (lambda ()
   (define-key comint-mode-map (kbd "M-l") 'recenter) ; was comint-previous-input. Use Ctrl+↑ or f11
;;   (define-key comint-mode-map (kbd "M-n") 'nil) ; was comint-next-input. Use Ctrl+↓ or f12

   ;; rebind displaced commands that i still want a key
   (define-key comint-mode-map (kbd "TAB") 'comint-dynamic-complete)
   (define-key comint-mode-map (kbd "§ <up") 'comint-previous-matching-input)
   (define-key comint-mode-map (kbd "§ <down>") 'comint-next-matching-input)
))
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

; Window Spliting
(global-set-key (kbd "M-6") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-7") 'other-window) ; was center-line
(global-set-key (kbd "M-8") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-9") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-other-windows) ; was digit-argument

; Fullscreen
(global-set-key (kbd "<s-return>") 'maximize-frame)
(maximize-frame)

; open file
(global-set-key [(super o)] 'find-file)

; use full-ack for Find
(global-set-key [(super F)] 'ack)

; buffer switching
(global-set-key [(super k)] 'previous-buffer)
(global-set-key [(super j)] 'next-buffer)

; close window
(global-set-key [(super w)]
  (lambda ()
      (interactive)
          (kill-buffer (current-buffer))))

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
(global-set-key [f5] 'call-last-kbd-macro)

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
(global-set-key (kbd "<f6>"    ) 'elscreen-create)
(global-set-key (kbd "S-<f6>"  ) 'elscreen-kill)

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
;(define-key (current-global-map) [remap vimpulse-operator-forward-word] 'forward-word)
;(define-key (current-global-map) [remap viper-backward-word] 'backward-word)
;(define-key (current-global-map) [remap vimpulse-operator-backward-word] 'backward-word)

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

(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-process-directory (expand-file-name "~/"))

;;;;;;;;;;;;;;;;;; ERC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'erc)
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
  '((".*\\.freenode.net" "#emacs" "#twitterapi" "#scala")))
;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun djcb-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?

    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "hjzhu" :full-name "Justin"))))
;; switch to ERC with Ctrl+c e
(global-set-key (kbd "C-c e") 'djcb-erc-start-or-switch) ;; ERC

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

(global-set-key [f2] 'jao-toggle-selective-display)


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
(define-key my-keys-minor-mode-map (kbd "C-w =") 'balance-windows)
(define-key my-keys-minor-mode-map (kbd "C-w h") 'windmove-left)
(define-key my-keys-minor-mode-map (kbd "C-w l") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "C-w k") 'windmove-up)
(define-key my-keys-minor-mode-map (kbd "C-w j") 'windmove-down)
(define-key my-keys-minor-mode-map (kbd "C-w C-h") 'windmove-left)
(define-key my-keys-minor-mode-map (kbd "C-w C-l") 'windmove-right)
(define-key my-keys-minor-mode-map (kbd "C-w C-k") 'windmove-up)
(define-key my-keys-minor-mode-map (kbd "C-w C-j") 'windmove-down)
(define-key my-keys-minor-mode-map (kbd "<C-tab>") 'other-frame)

(define-key my-keys-minor-mode-map (kbd "C-c o") 'rename-file-and-buffer)
(define-key my-keys-minor-mode-map (kbd "C-c g") 'customize-group)
(define-key my-keys-minor-mode-map (kbd "C-x C-j") 'dired-jump-other-window)
(define-key my-keys-minor-mode-map (kbd "C-l") 'dired-jump)
(define-key my-keys-minor-mode-map (kbd "C-b") 'ido-switch-buffer)
(define-key my-keys-minor-mode-map (kbd "C-x b") 'display-buffer)

;; searching
(define-key my-keys-minor-mode-map (kbd "C-f g") 'moccur-grep-find)
(define-key my-keys-minor-mode-map (kbd "C-f d") 'dmoccur)
(define-key my-keys-minor-mode-map (kbd "C-f s") 'ack-same)
(define-key my-keys-minor-mode-map (kbd "C-f a") 'ack)
(define-key my-keys-minor-mode-map (kbd "C-f f") 'ack-find-file)
(define-key my-keys-minor-mode-map (kbd "C-f p") 'replace-regexp)
(define-key my-keys-minor-mode-map (kbd "C-e") 'other-window)
(define-key my-keys-minor-mode-map (kbd "C-c d") 'ediff-revision)

(define-key my-keys-minor-mode-map (kbd "M-i") 'google-search-selection)
(define-key my-keys-minor-mode-map (kbd "s-i") 'google-it)
(define-key my-keys-minor-mode-map (kbd "C-f p") 'replace-regexp)

(vimpulse-map (kbd "C-f g") 'moccur-grep-find)
(vimpulse-map (kbd "C-f d") 'dmoccur)
(vimpulse-map (kbd "C-f s") 'ack-same)
(vimpulse-map (kbd "C-f a") 'ack)
(vimpulse-map (kbd "C-f f") 'ack-find-file)
(vimpulse-map (kbd "C-f p") 'replace-regexp)
(vimpulse-map (kbd "C-b") 'ido-switch-buffer)
(vimpulse-map (kbd "C-e") 'other-window)
(vimpulse-map (kbd "Y") 'kill-line)
(vimpulse-vmap (kbd "TAB") 'vimpulse-shift-right)
(vimpulse-vmap (kbd "<S-tab>") 'vimpulse-shift-left)
;; TODO unbind C-y, C-e

(global-set-key (kbd "C-c k") 'ecb-toggle-ecb-windows)
(global-set-key (kbd "C-c l") 'ensime) ;; replace lambda
;(global-set-key (kbd "C-c ;") 'ensime-ecb)

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
  (eval-after-load "camelCase"
    '(diminish 'camelCase-mode "C"))
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
(setq ac-delete-dups nil)
(setq ac-use-fuzzy t)
(setq ac-auto-show-menu 0.8)

(add-to-list 'ac-modes 'scala-mode)

(define-key ac-completing-map [return] 'ac-complete)
(define-key ac-completing-map (kbd "<S-tab>") 'ac-previous)
(define-key ac-completing-map (kbd "ESC") 'ac-stop)

(ac-set-trigger-key "TAB")

;; Change the default hippie-expand order and add yasnippet to the front.
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

;; Enables tab completion in the `eval-expression` minibuffer
(define-key read-expression-map [(tab)] 'hippie-expand)
(define-key read-expression-map [(shift tab)] 'unexpand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet

(require 'ensime)
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat user-dir "/yasnippet-read-only/snippets"))

(setq yas/trigger-key (kbd "C-c <kp-multiply>"))

;; BUG DONT COMPILE YASNIPPET!!!!!!!!!!
; Replace yasnippets's TAB
(add-hook 'yas/minor-mode-hook
          (lambda () (define-key yas/minor-mode-map
                       (kbd "TAB") 'auto-complete))) ; was yas/expand

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
