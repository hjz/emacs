;; twittering-mode
(require 'twittering-mode)
(setq twittering-use-master-password t)
(setq twittering-initial-timeline-spec-string
      '(":replies" ":home"))

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
(setq twittering-timer-interval 120)         ; Update your timeline each 120 seconds
(setq twittering-url-show-status nil)        ; Keeps the echo area from showing all the http processes
(setq twittering-number-of-tweets-on-retrieval 30)

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
(add-hook 'twittering-mode-hook 'hl-line-mode)
