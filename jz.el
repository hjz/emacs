(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq user-dir (concat dotfiles-dir user-login-name))

(add-to-list 'load-path (concat user-dir "/elisp"))
(add-to-list 'load-path (concat user-dir "/apel-10.8"))
(add-to-list 'load-path (concat user-dir "/yasnippet"))
(add-to-list 'load-path (concat user-dir "/ensime/elisp"))
(add-to-list 'load-path (concat user-dir "/vimpulse"))
(add-to-list 'load-path (concat user-dir "/vimpulse-surround.el"))
(add-to-list 'load-path (concat user-dir "/vimpulse-plugins"))
(add-to-list 'load-path (concat user-dir "/scamacs/ecb"))
(add-to-list 'load-path (concat user-dir "/scala"))
(add-to-list 'load-path (concat user-dir "/scamacs/scamacs"))
(add-to-list 'load-path (concat user-dir "/anything-config"))
;(add-to-list 'load-path (concat user-dir "/tabbar"))
(add-to-list 'load-path (concat user-dir "/elscreen-1.4.6"))
(add-to-list 'load-path (concat user-dir "/yaml-mode"))

(setq exec-path (append exec-path '("/Users/jz/bin/")))
(setq exec-path (append exec-path '("/opt/local/bin/")))

(autoload 'css-color-mode "mon-css-color" "" t)
(add-hook 'css-mode-hook  'css-color-turn-on-in-buffer)

(setq ring-bell-function 'ignore)

;; Theme
(require 'color-theme)
(load-theme 'zenburn)
(set-face-foreground 'vertical-border "#282828")

;; PATH
(defun read-system-path ()
  (with-temp-buffer
    (insert-file-contents "/etc/paths")
    (goto-char (point-min))
    (replace-regexp "\n" ":")
    (thing-at-point 'line)))

(setenv "PATH" (concat (read-system-path) "/opt/local/bin/"))

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

(if (boundp 'semantic-ia) (require 'semantic-ia))
(if (boundp 'semantic-gcc) (require 'semantic-gcc))

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
(vimpulse-map ";" 'viper-ex)
(vimpulse-vmap ";" 'vimpulse-visual-ex)
(vimpulse-map (kbd "SPC") 'hs-toggle-hiding)
(vimpulse-map "?" 'describe-bindings)
(vimpulse-map (kbd "ESC") 'keyboard-quit)
(define-key viper-minibuffer-map (kbd "ESC") 'keyboard-quit)

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

;;;;;; Camel Case ;;;;;;;
(autoload 'camelCase-mode "camelCase-mode" nil t)
;; rebind viper fwd bkwd

(require 'ecb)
(setq ecb-tip-of-the-day nil)

(require 'ido)
;; stolen from emacs-fu.blogspot.com
(ido-mode 'both) ;; for buffers and files
(setq 
  ido-everwhere t
  idosave-directory-list-file "~/.emacs.d/cache/ido.last"

  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"

     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t                 ; be case-insensitive

  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point 'guess ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ido-create-new-buffer 'always    ; create buf for no match
  ido-enable-flex-matching nil     ; don't try to be too smart
  ido-max-prospects 8              ; don't spam my minibuffer
  ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; when using ido, the confirmation is rather annoying...
 (setq confirm-nonexistent-file-or-buffer nil)

;; cycle through buffers with Ctrl-Tab (like Firefox)
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;;;;;;;;;;;;;;; Scala ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'scala-mode-auto)

(add-hook 'scala-mode-hook
  (lambda ()
    (local-set-key [return] 'comment-indent-new-line)))
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'highlight-80+-mode)
(defun me-turn-off-indent-tabs-mode ()
  (setq indent-tabs-mode nil))
(add-hook 'scala-mode-hook 'me-turn-off-indent-tabs-mode)
(add-hook 'scala-mode-hook 'hs-minor-mode)
(add-hook 'scala-mode-hook 'camelCase-mode)
(add-hook 'scala-mode-hook 'autopair-mode)

; highlighting for TODO 
(require 'highlight-fixmes-mode)
(add-hook 'scala-mode-hook 'highlight-fixmes-mode)

(require 'ensime)
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat user-dir "/yasnippet/snippets"))
  (add-hook 'scala-mode-hook
            '(lambda ()
               (yas/minor-mode-on)
               ))
(add-hook 'scala-mode-hook 'hl-line-mode)

;; ECB support
(require 'ensime-ecb)
;;;;;;;;;;;;;;; END Scala ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;
(global-set-key "\C-c\C-w" 'backward-kill-word)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(menu-bar-mode 1)

;; Session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;;;;;;;;;;;;;;;;;;; KEY BINDINGS ;;;;;;;;;;;;;;;;;;;;;;

(defun maximize-frame () 
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))

; Window Movement
(winner-mode 1)
(windmove-default-keybindings 'meta)

; Window Spliting
(global-set-key (kbd "M-6") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-7") 'other-window) ; was center-line
(global-set-key (kbd "M-8") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-9") 'split-window-horizontally) ; was digit-argument

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

; window switching
(global-set-key (kbd "s-`") 'other-window)

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

(setq recentf-max-saved-items 200)

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
(global-set-key "\C-cy" '(lambda ()
   (interactive)
   (popup-menu 'yank-menu)))

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

; TODO enable cedet folding
 
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
 
 (eval-after-load "elscreen"
   '(add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update))

(load "elscreen" "ElScreen" t)
(require 'elscreen-buffer-list)

;; show column #
(column-number-mode t)
(line-number-mode nil)

;; surround        
(require 'vimpulse-surround)

(require 'multi-term)
(setq multi-term-program "/bin/bash")
(setq term-default-bg-color "#1f1f1f")   
(setq term-default-fg-color "#dcdccc")
;; only needed if you use autopair
(add-hook 'term-mode-hook
  #'(lambda () (setq autopair-dont-activate t)))


(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

; Cleanup mode line
;; Minor modes
(when (require 'diminish nil 'noerror)
  (eval-after-load "Undo-Tree"
      '(diminish 'undo-tree-mode "U"))
  (eval-after-load "pair"
    '(diminish 'autopair-mode "P"))
  (eval-after-load "camelCase"
    '(diminish 'camelCase-mode "cC"))
  (eval-after-load "yasnippet"
    '(diminish 'yas/minor-mode "Y")))
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
(define-key (current-global-map) [remap viper-forward-word] 'forward-word)
(define-key (current-global-map) [remap vimpulse-operator-forward-word] 'forward-word)
(define-key (current-global-map) [remap viper-backward-word] 'backward-word)
(define-key (current-global-map) [remap vimpulse-operator-backward-word] 'backward-word)

(setq campfire-room-name "API")
(setq campfire-room-id "188551")

; Spelling
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)
(add-hook 'scala-mode-hook 'flyspell-prog-mode)

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

;; Redefine the 8 primary terminal colors to look good against black
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

(setq viper-custom-file-name
       (convert-standard-filename "~/.emacs.d/jz/dot-viper.el"))

(defun jao-toggle-selective-display ()
  "Activate selective display based on the column at point"
  (interactive)
  (set-selective-display
    (if selective-display
      nil
      (+ 1 (current-column)))))
(global-set-key [f2] 'jao-toggle-selective-display)

