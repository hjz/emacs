(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq user-dir (concat dotfiles-dir user-login-name))

(add-to-list 'load-path (concat user-dir "/elisp"))
(add-to-list 'load-path (concat user-dir "/apel-10.8"))
(add-to-list 'load-path (concat user-dir "/scala"))
(add-to-list 'load-path (concat user-dir "/yasnippet"))
(add-to-list 'load-path (concat user-dir "/ensime/elisp"))
(add-to-list 'load-path (concat user-dir "/vimpulse"))
(add-to-list 'load-path (concat user-dir "/vimpulse-surround.el"))
(add-to-list 'load-path (concat user-dir "/vimpulse-plugins"))
(add-to-list 'load-path (concat user-dir "/ecb"))
(add-to-list 'load-path (concat user-dir "/anything-config"))
(add-to-list 'load-path (concat user-dir "/tabbar"))
(add-to-list 'load-path (concat user-dir "/elscreen-1.4.6"))

(setq exec-path (append exec-path '("/Users/jz/bin/")))
(setq exec-path (append exec-path '("/opt/local/bin/")))

(autoload 'css-color-mode "mon-css-color" "" t)
(add-hook 'css-mode-hook  'css-color-turn-on-in-buffer)

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

;(load "jz/cedet-1.0pre7/common/cedet.el")

;(load "jz/cedet-1.0pre7/contrib/semantic-ectag-scala.el")

;(global-ede-mode 1)

;(semantic-load-enable-minimum-features)      ; Enable prototype help and smart completion 
;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;(semantic-load-enable-primary-exuberent-ctags-support)
;(defun my-semantic-hook ()
;  (imenu-add-to-menubar "TAGS"))
;(add-hook 'semantic-init-hooks 'my-semantic-hook)

;;;;;;;;;;;;;;;; CEDET END ;;;;;;;;;;;;;;;;;;

(require 'anything-config)

(define-key global-map [(alt j)] 'tabbar-backward)
(define-key global-map [(alt k)] 'tabbar-forward)

(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)
(require 'undo-tree)
(global-undo-tree-mode)

;;;;;;;;;;;;;;;; VIM END ;;;;;;;;;;;;;;;;;;

(require 'vimpulse)
(vimpulse-map ";" 'viper-ex)
;(vimpulse-map "SPC" 'hs-toggle-hiding)
(vimpulse-vmap ";" 'vimpulse-visual-ex)

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
    (local-set-key [return] 'reindent-then-newline-and-indent)))
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'highlight-80+-mode)
(setq ring-bell-function 'ignore)
(defun me-turn-off-indent-tabs-mode ()
  (setq indent-tabs-mode nil))
(add-hook 'scala-mode-hook 'me-turn-off-indent-tabs-mode)
(add-hook 'scala-mode-hook 'hs-minor-mode)
(add-hook 'scala-mode-hook 'camelCase-mode)
(require 'ensime)
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat user-dir "/yasnippet/snippets"))
  (add-hook 'scala-mode-hook
            '(lambda ()
               (yas/minor-mode-on)
               ))
(add-hook 'scala-mode-hook 'hl-line-mode)

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

(setq recentf-max-saved-items 100)

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
(load "elscreen" "ElScreen" t)

;; F9 creates a new elscreen, shift-F9 kills it
(global-set-key (kbd "<f6>"    ) 'elscreen-create)
(global-set-key (kbd "S-<f6>"  ) 'elscreen-kill)  

;; Windowskey+PgUP/PgDown switches between elscreens
(global-set-key (kbd "<s-prior>") 'elscreen-previous) 
(global-set-key (kbd "<s-next>")  'elscreen-next) 

;; show column #
(column-number-mode t)

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
;(setq ispell-process-directory (expand-file-name "~/"))
;


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


; highlighting for TODO 
(require 'fic-mode)
(defun add-something-to-mode-hooks (mode-list something)
  "helper function to add a callback to multiple hooks"
  (dolist (mode mode-list)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))

(add-something-to-mode-hooks '(c++ scala emacs-lisp) 'turn-on-fic-mode)

; TODO img for campfire

