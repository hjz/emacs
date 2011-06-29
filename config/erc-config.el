(require 'erc)
(require 'erc-nicklist)
(require 'vimpulse)
;; check channels
(erc-track-mode t)

(load "~/.ercpass")
(add-hook 'erc-join-hook 'bitlbee-identify)
(defun bitlbee-identify ()
  "If we're on the bitlbee server, send the identify command to the
  &bitlbee channel."
   (when (and (string= "localhost" erc-session-server)
              (string= "&bitlbee" (buffer-name)))
     (erc-message "PRIVMSG" (format "%s identify %s"
                                    (erc-default-target)
                                    bitlbee-password))
)
   )

;(require 'erc-services)
;(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)

;; bitlbee specific
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

(require 'random-quote)

; VIA: http://hg.quodlibetor.com/emacs.d/raw-file/6634ae6dcbee/customize/chat.el
(setq erc-modules '(netsplit fill track completion ring button autojoin smiley
                 services match stamp page log replace autoaway highlight-nicknames keep-place
                 move-to-prompt irccontrols spelling)
      erc-autojoin-channels-alist '(("localhost" "&bitlbee" "#Emacs" "#ScalaFolks" "#API"))
;      erc-pals '("forever" "alone")
;      erc-fools '()
;      erc-input-line-position -2
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE")
      erc-autoaway-idle-seconds 600
      erc-autoaway-message (concat "Away (" (pick-random-quote)  ")")
      erc-auto-discard-away t
      erc-auto-set-away t

      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "NAMES"
                                "324" "329" "332" "333" "353" "477")

      erc-fill-function 'erc-fill-static
      erc-fill-static-center 10
      erc-log-channels-directory (expand-file-name "~/Dropbox/logs/")
      erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(defun my-concat-lines (lines str count)
(if (and lines (> count 0))
   (concat (my-concat-lines (cdr lines) str (- count 1)) "\n" (car lines))
   str))

(defun my-get-file-lines (filename count)
  (with-temp-buffer
    (ignore-errors (insert-file-contents filename))
    (my-concat-lines (split-string (buffer-string) "\n" t) nil count)
))

;; Insert log contents
(add-hook 'erc-join-hook '(lambda ()
                            (save-excursion
                              (goto-char 0)
                              (insert (substring (my-get-file-lines (erc-current-logfile) 16) 1))
                            )))
                            ;(add-hook 'erc'join-hook (goto-char (point-max)))

(setq erc-replace-alist '(("\</?FONT>" . "")))

(defun erc-ignore-unimportant (msg)
  (if (or (string-match "*** localhost has changed mode for &bitlbee to" msg)
          (string-match "Account already online" msg)
          (string-match "You're already logged in." msg)
          (string-match "Trying to get all accounts connected" msg)
          (string-match "Unknown error while loading configuration" msg)
          (string-match "topic set by root!root@localhost" msg)
          (string-match "now away: Away" msg)
          (string-match "modes:.*t" msg)
          (string-match "Setting automatically away after .*" msg)
          (string-match "Topic for.*BitlBee groupchat" msg)
          (string-match "*** Welcome back" msg))
      (setq erc-insert-this nil)))
(add-hook 'erc-insert-pre-hook 'erc-ignore-unimportant)

;; modify nickname highlighting
(defvar is-notice-property) ;; just a symbol for use as text prop name
(defadvice erc-highlight-notice (after note-notice-on-highlight activate)
  "Annotate notices with is-notice-property"
  (put-text-property 0 (length s) 'is-notice-property 't s))

;; unactivated modification to erc-get-server-user to reject self.
(defadvice erc-get-server-user (around erc-get-server-user-notself)
  (if (not (string-equal nick (erc-current-nick)))
    ad-do-it))

(defadvice erc-highlight-nicknames (around disable-nick-highlight-for-notice activate)
  "only allow nick highlighting when not a notice, and disable
   highlighting of own nick"
  (re-search-forward "\\w+" nil t 2) ;; make sure we skip leading timestamp
  (unless (get-text-property (point) 'is-notice-property)
    ;; don't re-highlight self, as it does nothing but break erc-track
    (ad-activate-regexp "erc-get-server-user-notself")
    ad-do-it
    (ad-deactivate-regexp "erc-get-server-user-notself")))

;; todo use something better
;; (define-key erc-mode-map (kbd "C-c C-q")
            ;; (lambda (nick)
              ;; (interactive (list (completing-read "Nick: " channel-members)))
              ;; (erc-cmd-QUERY nick)))

;; allow some channels to not auto-delay messages. This can
;; get you kicked from sane channels, so don't use it.
(add-hook 'erc-mode-hook
      (lambda ()
        (let ((floodable-buffers
           '(;; every channel in this list is floodable:
             "#bugfunk"
             )))
          (when (member (buffer-name) floodable-buffers)
        (make-local-variable 'erc-server-flood-penalty)
        (setq erc-server-flood-penalty 0)))))

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
;(setq erc-prompt (lambda ()|#
           ;(if (and (boundp 'erc-default-recipients)
                ;(erc-default-target))
               ;(erc-propertize (concat (erc-default-target) ">")
                       ;'read-only t
                       ;'rear-nonsticky t
                       ;'front-nonsticky t)
             ;(erc-propertize (concat ">")
                     ;'read-only t
                     ;'rear-nonsticky t
                     ;'front-nonsticky t))))

(setq erc-prompt "❯❯")

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

(defun important-msg ()
  (or (string-match "jz:" msg)
      (string-match "justin" msg)
      (string-match "zhu" msg)
      (string-match "Message from unknown handle" msg)))

;; TODO use growl notify"
(defun erc-notify-on-msg (msg)
  "Send a message via notify-send if a message specifically to me"
  (when (and (important-msg)
           (not (string-match "\\*\\*\\*" msg))
           (not (string-match "\<root\>" msg))
           (string-match "^#.*" (buffer-name)))
      (let ((nameless-msg (replace-regexp-in-string "^\<.*?\>" "" msg)))
        (start-process-shell-command "message recv" nil "afplay ~/Dropbox/Message_Received.wav")
        (growl (buffer-name) nameless-msg))))

(defun my-erc-page-me-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg)))
               ;(my-erc-page-allowed nick))
               (start-process-shell-command "message recv" nil "afplay ~/Dropbox/Message_Received.wav")
               (growl nick msg)
      nil)))

(add-hook 'erc-server-PRIVMSG-functions 'my-erc-page-me-PRIVMSG)
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

;; dont need fill, but need it for formatting nicks
(setq erc-fill-column 1000)

(defun erc-cmd-FORTUNE ()
  "show some information about my system"
  (let ((str (shell-command-to-string "fortune | cowsay -f tux")))
    (when str (erc-send-message str))))

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
(add-to-list 'viper-insert-state-mode-list 'erc-mode)
(add-to-list 'viper-insert-state-mode-list 'magit-log-edit-mode)
(add-to-list 'viper-insert-state-mode-list 'comint-mode)
 ;(defun ted-viper-erc-hook ()
   ;"Make RET DTRT when you use Viper and ERC together."
   ;(viper-add-local-keys 'insert-state
                         ;`((,(kbd "RET") . erc-send-current-line)))
   ;(viper-add-local-keys 'vi-state
                         ;`((,(kbd "RET") . erc-send-current-line))))
 ;(add-hook 'erc-mode-hook 'ted-viper-erc-hook)
 (setq erc-button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")

(defun start-irc ()
  (interactive)
  (unless (get-buffer "localhost:6667") ;; ERC already active?
    (select-frame (make-frame '((name . "Emacs IRC")
                                (minibuffer . t))))
    (erc :server "localhost" :port "6667" :nick "jz" :password bitlbee-password))
)

(defun djcb-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "localhost:6667") ;; ERC already active?

    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "localhost" :port 6667 :nick "jz" :full-name "Justin"))))
;; switch to ERC with Ctrl+c e
(global-set-key (kbd "C-c e") 'start-irc)


;; timestamps
(make-variable-buffer-local
 (defvar erc-last-datestamp nil))

(defun ks-timestamp (string)
  (erc-insert-timestamp-left string)
  (let ((datestamp (erc-format-timestamp (current-time) erc-datestamp-format)))
    (unless (string= datestamp erc-last-datestamp)
      (erc-insert-timestamp-left datestamp)
      (setq erc-last-datestamp datestamp))))

(setq erc-timestamp-only-if-changed-flag t
      erc-timestamp-format "%H:%M "
      erc-datestamp-format " === [%Y-%m-%d %a] ===\n" ; mandatory ascii art
      erc-fill-prefix "      "
      erc-insert-timestamp-function 'ks-timestamp)

(setq erc-auto-query 'bury)

(add-hook 'erc-mode-hook '(lambda ()
                           (make-local-variable 'ido-enable-replace-completing-read)
                           (setq ido-enable-replace-completing-read nil)
                           (define-key erc-mode-map (kbd "<C-up>") 'erc-previous-command)
                           (define-key erc-mode-map (kbd "<C-down>") 'erc-next-command)
                          ))

;; show number of members in chan
(define-minor-mode ncm-mode "" nil
  (:eval
   (let ((ops 0)
         (voices 0)
         (members 0))
     (maphash (lambda (key value)
                (when (erc-channel-user-op-p key)
                  (setq ops (1+ ops)))
                (when (erc-channel-user-voice-p key)
                  (setq voices (1+ voices)))
                (setq members (1+ members)))
              erc-channel-users)
     (format " %S/%S/%S" ops voices members))))

;; show bar after unread msgs
(eval-after-load 'erc-track
  '(progn
     (defun erc-bar-move-back (n)
       "Moves back n message lines. Ignores wrapping, and server messages."
       (interactive "nHow many lines ? ")
       (re-search-backward "^.*<.*>" nil t n))

     (defun erc-bar-update-overlay ()
       "Update the overlay for current buffer, based on the content of
erc-modified-channels-alist. Should be executed on window change."
       (interactive)
       (let* ((info (assq (current-buffer) erc-modified-channels-alist))
	      (count (cadr info)))
	 (if (and info (> count erc-bar-threshold))
	     (save-excursion
	       (end-of-buffer)
	       (when (erc-bar-move-back count)
		 (let ((inhibit-field-text-motion t))
		   (move-overlay erc-bar-overlay
				 (line-beginning-position)
				 (line-end-position)
				 (current-buffer)))))
	   (delete-overlay erc-bar-overlay))))

     (defvar erc-bar-threshold 1
       "Display bar when there are more than erc-bar-threshold unread messages.")
     (defvar erc-bar-overlay nil
       "Overlay used to set bar")
     (setq erc-bar-overlay (make-overlay 0 0))
     (overlay-put erc-bar-overlay 'face '(:underline "black"))
     ;;put the hook before erc-modified-channels-update
     (defadvice erc-track-mode (after erc-bar-setup-hook
				      (&rest args) activate)
       ;;remove and add, so we know it's in the first place
       (remove-hook 'window-configuration-change-hook 'erc-bar-update-overlay)
       (add-hook 'window-configuration-change-hook 'erc-bar-update-overlay))
     (add-hook 'erc-send-completed-hook (lambda (str)
					  (erc-bar-update-overlay)))))
