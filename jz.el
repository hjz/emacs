(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq user-dir (concat dotfiles-dir user-login-name))

(add-to-list 'load-path (concat user-dir "/apel-10.8"))
(add-to-list 'load-path (concat user-dir "/scala"))
(add-to-list 'load-path (concat user-dir "/yasnippet"))
(add-to-list 'load-path (concat user-dir "/ensime/elisp"))
(add-to-list 'load-path (concat user-dir "/vimpulse"))
(add-to-list 'load-path (concat user-dir "/ecb"))
(add-to-list 'load-path (concat user-dir "/anything-config"))
(add-to-list 'load-path (concat user-dir "/tabbar"))
(add-to-list 'load-path (concat user-dir "/elscreen-1.4.6"))
(setq exec-path (append exec-path '("/Users/jz/bin/")))
(setq exec-path (append exec-path '("/opt/local/bin/")))

;;;;;;;;;;;;;;;;; CEDET ;;;;;;;;;;;;;;;;;;;;;;
(defcustom semantic-ectag-program "/opt/local/bin/ctags" 
  "The Exuberent CTags program to use."
  :group 'semantic
  :type 'program)

(load "jz/cedet-1.0pre7/common/cedet.el")
(load "jz/cedet-1.0pre7/contrib/semantic-ectag-scala.el")
(global-ede-mode 1)
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu
(semantic-load-enable-primary-exuberent-ctags-support)
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

;;;;;;;;;;;;;;;; CEDET END ;;;;;;;;;;;;;;;;;;

(require 'anything-config)

(define-key global-map [(alt j)] 'tabbar-backward)
(define-key global-map [(alt k)] 'tabbar-forward)

(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)
(require 'undo-tree)
(global-undo-tree-mode)
(require 'zenburn)

(require 'vimpulse)
(blink-cursor-mode 1)

;;;;;; Camel Case ;;;;;;;
(autoload 'camelCase-mode "camelCase-mode" nil t)
;; rebind viper fwd bkwd

(require 'ecb)
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

;; Scala
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
(require 'ensime)
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat user-dir "/yasnippet/snippets"))
  (add-hook 'scala-mode-hook
            '(lambda ()
               (yas/minor-mode-on)
               ))

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

;;:w (winner-mode 1)
(global-set-key [M-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer wind

; Fullscreen
(global-set-key (kbd "<s-return>") 'maximize-frame)

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

(vimpulse-map ";" 'viper-ex)

;; os x cust
; use default Mac browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

; delete files by moving them to the OS X trash
(setq delete-by-moving-to-trash t)

; run macro
(global-set-key [f5] 'call-last-kbd-macro)

(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
         (if (string-match dir "\\(?:/\\|\\\\)$")
         (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

 (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
 (progn         (copy-file filename newname 1)  (delete-file filename)  (set-visited-file-name newname)         (set-buffer-modified-p nil)))))
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

; automatically clean up old buffers
(require 'midnight)

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

(require 'elscreen-color-theme)
;; F9 creates a new elscreen, shift-F9 kills it
(global-set-key (kbd "<f6>"    ) 'elscreen-create)
(global-set-key (kbd "S-<f6>"  ) 'elscreen-kill)  

;; Windowskey+PgUP/PgDown switches between elscreens
(global-set-key (kbd "<s-prior>") 'elscreen-previous) 
(global-set-key (kbd "<s-next>")  'elscreen-next) 
