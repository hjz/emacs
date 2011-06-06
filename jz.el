(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq user-dir (concat dotfiles-dir user-login-name))

;; no graphic dialog
(setq use-dialog-box nil)

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
(add-to-list 'load-path (concat user-dir "/browse-kill-ring"))
(add-to-list 'load-path (concat user-dir "/one-key-menus"))
(add-to-list 'load-path (concat user-dir "/magit-1.0.0"))
(add-to-list 'load-path (concat user-dir "/confluence-mode"))
(add-to-list 'load-path (concat user-dir "/minimap"))
(add-to-list 'load-path (concat user-dir "/google-weather"))
(require 'minimap)

(require 'command-frequency)
(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)

;;;;;;;;;;;;;;;;;; ERC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'erc)
;; check channels
(erc-track-mode t)
(defun djcb-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "localhost:6667") ;; ERC already active?

    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "localhost" :port 6667 :nick "jz" :full-name "Justin"))))
;; switch to ERC with Ctrl+c e
(global-set-key (kbd "C-c e") 'djcb-erc-start-or-switch) ;; ERC

(load "~/.ercpass")
(add-hook 'erc-join-hook 'bitlbee-identify)
(defun bitlbee-identify ()
  "If we're on the bitlbee server, send the identify command to the
  &bitlbee channel."
   (when (and (string= "localhost" erc-session-server)
              (string= "&bitlbee" (buffer-name)))
     (erc-message "PRIVMSG" (format "%s identify %s"
                                    (erc-default-target)
                                    bitlbee-password)))
   )

;(require 'erc-services)
;(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)

;; bitlbee specific
(defun erc-ignore-unimportant (msg)
  (if (or (string-match "*** localhost has changed mode for &bitlbee to" msg)
          (string-match "Account already online" msg)
          (string-match "You're already logged in." msg)
          (string-match "Trying to get all accounts connected" msg)
          (string-match "Unknown error while loading configuration" msg))
      (setq erc-insert-this nil)))
(add-hook 'erc-insert-pre-hook 'erc-ignore-unimportant)

(setq erc-keywords '((".*Online.*" (:foreground "#8fb28f"))
                     (".*Busy$" (:foreground "#bc8383"))
                     (".*Away$" (:foreground "#7cb8bb"))
                     (".*Away (.*)" (:foreground "#7cb8bb"))
                     ("available" (:foreground "#8fb28f"))
                     ("away" (:foreground "#7cb8bb"))
                     ("offline" (:foreground "#bc8383"))
                     (".*Idle$" (:foreground "#d0bf8f"))
                     (".*Disturb$" (:foreground "#bc8383"))
                     ))
                     
(setq erc-auto-query 'bury)
(if (get-buffer "localhost:6667") ;; ERC already active?
  (erc-track-switch-buffer 1) ;; yes: switch to last active
(erc :server "localhost" :port "6667" :nick "jz" :password bitlbee-password))

; VIA: http://hg.quodlibetor.com/emacs.d/raw-file/6634ae6dcbee/customize/chat.el
(setq erc-modules '(netsplit fill track completion ring button autojoin smiley
                 services match stamp page log replace
                 scrolltobottom move-to-prompt irccontrols spelling)
      erc-autojoin-channels-alist '(("localhost" "&bitlbee")); "#Emacs" "#ScalaFolks" "#API" "#test" ))
;      erc-pals '("forever" "alone")
;      erc-fools '()
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")

      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "NAMES"
                                "324" "329" "332" "333" "353" "477")

      erc-fill-function 'erc-fill-static
      erc-fill-static-center 6
      ;; logging! ... requires the `log' module
      ;; do it line-by-line instead of on quit
      erc-log-channels-directory (expand-file-name "~/Dropbox/logs/")
      erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(setq erc-replace-alist 
  '(
    ("\*\*\* Topic for.*" . "")
    (".*topic set by root!root@localhost.*" . "")
    (".*modes: \+t" . "")
    )
  )

;; allow some channels to not auto-delay messages. This can
;; get you kicked from sane channels, so don't use it.
;(add-hook 'erc-mode-hook
      ;(lambda ()
        ;(let ((floodable-buffers
           ;'(;; every channel in this list is floodable:
             ;"#bugfunk"
             ;)))
          ;(when (member (buffer-name) floodable-buffers)
        ;(make-local-variable 'erc-server-flood-penalty)
        ;(setq erc-server-flood-penalty 0)))))

(defun erc-quit-bitlbee-maybe (process)
  (when (and (get-buffer-process bitlbee-buffer-name)
         (equal (get-buffer-process bitlbee-buffer-name)
            (get-process "bitlbee")))
    (bitlbee-stop)
    (kill-buffer bitlbee-buffer-name)))
(add-hook 'erc-quit-hook
      'erc-quit-bitlbee-maybe)
(add-hook 'erc-quit-hook
      (lambda (process)
        (message "%s" process)))

(defun bwm-make-buffer-floodable ()
  (make-local-variable 'erc-server-flood-penalty)
  (setq erc-server-flood-penalty 0))

;; fancy prompt with channel name, or ERC if nil
;; http://www.emacswiki.org/emacs/ErcConfiguration#toc5
(setq erc-prompt (lambda ()
           (if (and (boundp 'erc-default-recipients)
                (erc-default-target))
               (erc-propertize (concat (erc-default-target) "❯")
                       'read-only t
                       'rear-nonsticky t
                       'front-nonsticky t)
             (erc-propertize (concat "❯")
                     'read-only t
                     'rear-nonsticky t
                     'front-nonsticky t))))

;; Interpret mIRC-style color commands in IRC chats
;; seems to only work correctly when the irccontrols module is enabled
(setq erc-interpret-mirc-color t
      erc-interpret-controls-p t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bitlbee stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
    (cond ((erc-server-process-alive) 'erc-header-line)
          (t 'erc-header-line-disconnected))))
(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)

;; TODO use growl notify"
(defun erc-notify-on-msg (msg)
  "Send a message via notify-send if a message specifically to me"
  (if (or (string-match "jz:" msg)
          (and (string-match "justin" msg)
           (not (string-match "\<root\>" msg)))
          (and (string-match "zhu" msg)
           (not (string-match "\<root\>" msg)))
          (string-match "Message from unknown handle" msg)
      (and (string= "localhost" erc-session-server)
           (not (string-match "\\*\\*\\*" msg))
           (not (string-match "\<root\>" msg))))
      (let ((nameless-msg (replace-regexp-in-string "^\<.*?\>" "" msg)))
    (start-process-shell-command "message recv" nil "afplay ~/Dropbox/Message_Received.wav")
    (growl (buffer-name) nameless-msg)
)))
(add-hook 'erc-insert-pre-hook 'erc-notify-on-msg)

;; The following are commented out by default, but users of other
;; non-Emacs IRC clients might find them useful.

;; these can all be accomplished by
;;     M-x ibuffer RET * M erc-mode RET D RET

;; Kill buffers for channels after /part
;; (setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
;; (setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook 
          '(lambda ()
             (save-excursion
               (walk-windows
                 (lambda (w)
                   (let ((buffer (window-buffer w)))
                     (set-buffer buffer)
                     (when (eq major-mode 'erc-mode)
                       (setq erc-fill-column (- (window-width w) 2)))))))))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(defun erc-cmd-FORTUNE ()
  "show some information about my system"
  (let ((str (shell-command-to-string "fortune | cowsay -f tux")))
    (when str (erc-send-message str))))

 (make-variable-buffer-local 'erc-fill-column)
 (add-hook 'window-configuration-change-hook 
           '(lambda ()
              (save-excursion
                (walk-windows
                 (lambda (w)
                   (let ((buffer (window-buffer w)))
                     (set-buffer buffer)
                     (when (eq major-mode 'erc-mode)
                       (setq erc-fill-column (- (window-width w) 2)))))))))

;; auto fill last chatted user
(defadvice erc-display-prompt (after conversation-erc-display-prompt activate)
  "Insert last recipient after prompt."
  (let ((previous 
         (save-excursion 
           (if (and (search-backward-regexp (concat "^[^<]*<" erc-nick ">") nil t)
                    (search-forward-regexp (concat "^[^<]*<" erc-nick ">" 
                                                   " *\\([^:]*: ?\\)") nil t))
               (match-string 1)))))
    ;; when we got something, and it was in the last 3 mins, put it in
    (when (and 
           previous 
           (> 180 (time-to-seconds 
                   (time-since (get-text-property 0 'timestamp previous)))))
      (set-text-properties 0 (length previous) nil previous)
      (insert previous))))

 ;; Ensure that ERC comes up in Insert mode. TODO for MAGIT
 ;(add-to-list 'viper-insert-state-mode-list 'erc-mode)
 ;(defun ted-viper-erc-hook ()
   ;"Make RET DTRT when you use Viper and ERC together."
   ;(viper-add-local-keys 'insert-state
                         ;`((,(kbd "RET") . erc-send-current-line)))
   ;(viper-add-local-keys 'vi-state
                         ;`((,(kbd "RET") . erc-send-current-line))))
 ;(add-hook 'erc-mode-hook 'ted-viper-erc-hook)
 (setq erc-button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")

