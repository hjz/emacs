(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-directory "~/Dropbox/org")
;; http://orgmode.org/manual/Setting-up-the-staging-area.html#Setting-up-the-staging-area
(setq org-mobile-directory "~/Dropbox/morg")
(setq org-mobile-inbox-for-pull (concat org-directory "/mobileorg.org"))
(setq org-agenda-files '("~/Dropbox/org"))



(require 'remember)
(require 'org-remember)
(setq remember-annotation-functions '(org-remember-annotation))

(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(setq org-default-notes-file (concat org-directory "/gtd.org"))
(define-key global-map "\C-cr" 'org-remember)

(setq org-remember-templates
      '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\n  Added: %U" "~/Dropbox/org/gtd.org" "Tasks")
        ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/Dropbox/org/journal.org")
        ("Idea" ?i "* %^{Title}\n  %i\n  %a" "~/Dropbox/org/maybe.org" "Ideas")))

(setq org-refile-targets (quote (("gtd.org" :maxlevel . 1)
                              ("someday.org" :level . 2))))

;; jekyll
(require 'org-jekyll)
(setq org-publish-project-alist
      '(
        ("org-jz"
         ;; Path to your org files.
         :base-directory "~/hjz.github.com/org"
         :base-extension "org"

         ;; Path to your Jekyll project.
         :publishing-directory "~/hjz.github.com"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t ;; Only export section between <body> </body>

;         :todo-keywords nil ; Skip todo keywords
;         :exclude "draft*" ; TODO fix

         :inline-images t
         :table-of-contents nil
         :drawers nil
         :section-numbers nil
         :auto-preamble nil
         :auto-postamble nil
         )


        ("org-static-jz"
         :base-directory "~/hjz.github.com/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "~/hjz.github.com/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("jz" :components ("org-jz" "org-static-jz"))
        ))

(defun my-org-convert-incoming-items ()
  (interactive)
  (with-current-buffer (find-file-noselect org-mobile-inbox-for-pull)
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t)
      (goto-char (match-beginning 0))
      (insert ?*)
      (forward-char 2)
      (insert "TODO ")
      (goto-char (line-beginning-position))
      (forward-line)
      (insert
       (format
        " SCHEDULED: %s
:PROPERTIES:
:ID: %s :END:
"
        (with-temp-buffer (org-insert-time-stamp (current-time)))
        (shell-command-to-string "uuidgen"))))
    (let ((tasks (buffer-string)))
      (erase-buffer)
      (save-buffer)
      (kill-buffer (current-buffer))
      (with-current-buffer (find-file-noselect (concat org-directory "/todo.org")
        (save-excursion
          (goto-char (point-min))
          (search-forward "* CEG")
          (goto-char (match-beginning 0))
          (insert tasks)))))))

(add-hook 'org-mobile-post-pull-hook 'my-org-convert-incoming-items)
