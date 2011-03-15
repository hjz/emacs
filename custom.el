(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#1f1f1f" "#dca3a3" "#7f9f7f" "#f0dfaf" "#7cb8bb" "#7f3194" "#93e0e3" "#dcdccc"])
 '(column-number-mode t)
 '(comment-auto-fill-only-comments t)
 '(comment-multi-line t)
 '(ecb-analyse-face (quote font-lock-warning-face))
 '(ecb-analyse-general-face (quote ecb-analyse-general-face))
 '(ecb-auto-activate t)
 '(ecb-directories-general-face (quote ecb-default-general-face))
 '(ecb-directory-face (quote font-lock-warning-face))
 '(ecb-eshell-auto-activate nil)
 '(ecb-history-bucket-node-dir-soure-path-face (quote ecb-history-bucket-node-face))
 '(ecb-history-face (quote font-lock-warning-face))
 '(ecb-history-general-face (quote ecb-default-general-face))
 '(ecb-layout-name "leftright-package")
 '(ecb-method-face (quote font-lock-warning-face))
 '(ecb-methods-general-face (quote ecb-default-general-face))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-face (quote font-lock-warning-face))
 '(ecb-source-path (quote (("/" "/") (#("/Users/jz/ps/swiftlet/config" 0 28 (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu")) "sconfig") (#("/Users/jz/ps/macaw" 0 18 (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu")) "macaw") (#("/Users/jz/ps/macaw/macaw-core/src/main/scala/com/twitter/macaw/twitter" 0 70 (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu")) "mcore") (#("/Users/jz/workspace/twitter/lib" 0 31 (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu")) "twlib") (#("/Users/jz/ps/macaw/macaw-twitter/src/main/scala/com/twitter/macaw/twitter" 0 73 (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu")) "macaw-twtr") (#("/Users/jz/ps/macaw/macaw-twitter/src/test/scala/com/twitter/macaw/twitter" 0 73 (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu")) "t-macaw-twtr"))))
 '(ecb-sources-general-face (quote ecb-default-general-face))
 '(ecb-tag-header-face (quote font-lock-warning-face))
 '(ecb-use-speedbar-instead-native-tree-buffer nil)
 '(ecb-vc-enable-support t)
 '(ecb-windows-height 0.2)
 '(ecb-windows-width 0.17)
 '(elscreen-display-tab nil)
 '(elscreen-tab-display-control nil)
 '(ensime-auto-connect (quote never))
 '(ensime-graphical-tooltips t)
 '(ensime-tooltip-type-hints t)
 '(fringe-mode (quote (0 . 0)) nil (fringe))
 '(global-hl-line-mode nil)
 '(highlight-80+-columns 100)
 '(ido-enable-flex-matching t)
 '(ido-enable-regexp t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "*.ido.last")))
 '(indicate-empty-lines t)
 '(magit-process-popup-time 30)
 '(magit-repo-dirs (quote ("~/ps/" "~/workspace/")))
 '(save-place t nil (saveplace))
 '(scala-mode-feature:electric-on-per-default nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(ssl-certificate-directory "~/.w3/certs/")
 '(ssl-program-arguments (quote ("s_client" "-quiet" "-host" host "-port" service "-verify" (int-to-string ssl-certificate-verification-policy) "-CApath" ssl-certificate-directory)))
 '(standard-indent 2)
 '(tab-always-indent (quote complete))
 '(tls-program (quote ("openssl s_client -connect %h:%p -no_ssl2 -ign_eof")))
 '(tool-bar-mode nil)
 '(transient-mark-mode nil)
 '(vimpulse-fold-level 5)
 '(vimpulse-operator-comment-key "Q")
 '(vimpulse-want-C-u-like-Vim t)
 '(viper-ESC-moves-cursor-back t)
 '(viper-auto-indent t)
 '(viper-ex-style-editing nil)
 '(viper-insert-state-cursor-color nil)
 '(viper-parse-sexp-ignore-comments nil)
 '(viper-shift-width 2)
 '(viper-syntax-preference (quote extended))
 '(viper-want-ctl-h-help t)
 '(yas/also-auto-indent-first-line nil)
 '(yas/indent-line (quote fixed))
 '(yas/next-field-key (quote ("SPC" "<tab>")))
 '(yas/prev-field-key (quote ("<backtab>" "<S-tab>")))
 '(yas/prompt-functions (quote (yas/dropdown-prompt)))
 '(yas/skip-and-clear-key (quote ("C-d" "<delete>" "<deletechar>"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1f1f1f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 134 :width normal :foundry "apple" :family "Consolas"))))
 '(campfire-my-message-face ((t (:background "#2b2b2b" :foreground "white"))))
 '(campfire-my-user-face ((t (:foreground "white" :box (:line-width 2 :color "blue" :style released-button)))))
 '(campfire-other-user-face ((t (:background "#2f2f2f" :foreground "#9fc59f"))))
 '(cursor ((t (:weight bold))))
 '(ecb-default-general-face ((((class color) (background dark)) nil)))
 '(elscreen-tab-background-face ((((class color)) (:background "#282828"))))
 '(elscreen-tab-control-face ((((class color)) (:background "#1f1f1f" :foreground "white"))))
 '(elscreen-tab-current-screen-face ((((class color)) (:background "#1f1f1f" :foreground "white"))))
 '(elscreen-tab-other-screen-face ((((class color)) (:background "#3D3D3D" :foreground "white"))))
 '(flyspell-incorrect ((t (:underline "#cc9393" :weight bold))))
 '(highlight-80+ ((((background dark)) (:background "#3f3f3f" :foreground "#afd8af" :weight bold))))
 '(highlight-80+-line ((t nil)) t)
 '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:background "#284f28" :foreground "#ffffe0"))))
 '(linum ((t (:background "#282828" :foreground "#b6b6b6"))))
 '(magit-item-highlight ((((class color) (background dark)) (:background "#2f2f2f"))))
 '(show-paren-mismatch ((t (:background "red" :foreground "white")))))
