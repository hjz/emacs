((ace-jump-mode status "installed" recipe
                (:name ace-jump-mode :website "https://github.com/winterTTr/ace-jump-mode/wiki" :description "A quick cursor location minor mode for emacs" :type github :pkgname "winterTTr/ace-jump-mode" :features ace-jump-mode))
 (ack status "removed" recipe nil)
 (el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :features el-get :info "." :load "el-get.el"))
 (evil status "removed" recipe nil)
 (full-ack status "installed" recipe
           (:name full-ack :description "A front-end for ack" :type github :pkgname "nschum/full-ack" :prepare
                  (progn
                    (autoload 'ack "full-ack" nil t)
                    (autoload 'ack-find-file "full-ack" nil t)
                    (autoload 'ack-find-same-file "full-ack" nil t)
                    (autoload 'ack-same "full-ack" nil t))))
 (pos-tip status "installed" recipe
          (:name pos-tip :description "Show tooltip at point" :type emacswiki))
 (scala-mode status "installed" recipe
             (:name scala-mode :description "Major mode for editing Scala code." :type svn :url "http://lampsvn.epfl.ch/svn-repos/scala/scala-tool-support/trunk/src/emacs/" :build
                    ("make")
                    :load-path
                    (".")
                    :features scala-mode-auto))
 (undo-tree status "installed" recipe
            (:name undo-tree :description "Treat undo history as a tree" :type git :url "http://www.dr-qubit.org/git/undo-tree.git" :prepare
                   (progn
                     (autoload 'undo-tree-mode "undo-tree.el" "Undo tree mode; see undo-tree.el for details" t)
                     (autoload 'global-undo-tree-mode "undo-tree.el" "Global undo tree mode" t)))))
