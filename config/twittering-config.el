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

