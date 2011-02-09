;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package is a minor mode to highlight the current line in buffer.
;;
;; highline was inspired on:
;;
;;    linemenu.el		  Bill Brodie <wbrodie@panix.com>
;;	 Hook function to highlight current line in buffer.
;;
;;    hl-line.el		  Dave Love <fx@gnu.org>
;;	 Highlight the current line.
;;
;;    highlight-current-line.el	  Christoph Conrad <christoph.conrad@gmx.de>
;;	 Highlight line where the cursor is.
;;
;; To use highline, insert in your ~/.emacs:
;;
;;    (require 'highline)
;;
;; For good performance, be sure to byte-compile highline.el, e.g.
;;
;;    M-x byte-compile-file <give the path to highline.el when prompted>
;;
;; This will generate highline.elc, which will be loaded instead of
;; highline.el.
;;
;; highline was tested with GNU Emacs 20.4.1.
;;
;;
;; Using highline
;; --------------
;;
;; * LOCAL highline (see NOTE 1 below):
;;    + To activate highline locally, type:
;;	    M-x highline-on RET
;;	 Or:
;;	    C-u 1 M-x highline-local-mode RET
;;
;;    + To deactivate highline locally, type:
;;	    M-x highline-off RET
;;	 Or:
;;	    C-u 0 M-x highline-local-mode RET
;;
;;    + To toggle highline locally, type:
;;	    M-x highline-local-mode RET
;;
;; * GLOBAL highline (see NOTE 1 below):
;;    + To activate highline globally, type:
;;	    M-x highline-mode-on RET
;;	 Or:
;;	    C-u 1 M-x highline-mode RET
;;
;;    + To deactivate highline globally, type:
;;	    M-x highline-mode-off RET
;;	 Or:
;;	    C-u 0 M-x highline-mode RET
;;
;;    + To toggle highline globally, type:
;;	    M-x highline-mode RET
;;
;; * INDIRECT highline (see NOTE 2 below):
;;    + To activate indirect highline, type:
;;	    M-x highline-view-on RET
;;	 Or:
;;	    C-u 1 M-x highline-view-mode RET
;;
;;    + To deactivate indirect highline, type:
;;	    M-x highline-view-off RET
;;	 Or:
;;	    C-u 0 M-x highline-view-mode RET
;;
;;    + To toggle indirect highline, type:
;;	    M-x highline-view-mode RET
;;
;; * To customize highline, type:
;;	 M-x highline-customize RET
;;
;; You can also bind `highline-local-mode', `highline-mode', `highline-on',
;; `highline-off', `highline-mode-on', `highline-mode-off',
;; `highline-customize', `highline-view-on', `highline-view-off' and
;; `highline-view-mode' to some key, like:
;;
;;    (global-set-key "\C-c\C-a"     'highline-on)
;;    (global-set-key "\C-c\C-b"     'highline-off)
;;    (global-set-key "\C-c\C-l"     'highline-local-mode)
;;    (global-set-key "\C-c\C-d"     'highline-mode-on)
;;    (global-set-key "\C-c\C-e"     'highline-mode-off)
;;    (global-set-key "\C-c\C-g"     'highline-mode)
;;    (global-set-key "\C-c\C-c"     'highline-customize)
;;    (global-set-key "\C-c\C-v\C-n" 'highline-view-on)
;;    (global-set-key "\C-c\C-v\C-f" 'highline-view-off)
;;    (global-set-key "\C-c\C-v\C-t" 'highline-view-mode)
;;
;; NOTE 1: There is no problem if you mix local and global minor mode usage.
;;
;; NOTE 2: Indirect highline (`highline-view-on', `highline-view-off' and
;;	   `highline-view-mode') is useful when you wish to have various
;;	   "visions" of the same buffer.
;;	   Indirect highline uses an indirect buffer to get the "vision" of the
;;	   buffer.  So, if you kill an indirect buffer, the base buffer is not
;;	   affected; if you kill the base buffer, all indirect buffer related
;;	   with the base buffer is automagicaly killed.  Also, any text
;;	   insertion/deletion in any indirect or base buffer is updated in all
;;	   related buffers.
;;
;;
;; Example
;; -------
;;
;; As an example, try to insert this in your .emacs file:
;;
;;  (require 'highline)
;;  ;; Turn on local highlighting for Dired (C-x d)
;;  (add-hook 'dired-after-readin-hook 'highline-on)
;;  ;; Turn on local highlighting for list-buffers (C-x C-b)
;;  (defadvice list-buffers (after highlight-line activate)
;;    (save-excursion
;;      (set-buffer "*Buffer List*")
;;      (highline-on)))
;;
;;
;; Hooks
;; -----
;;
;; highline has the following hook variables:
;;
;; `highline-hook'
;;    It is evaluated always when highline is turned on globally.
;;
;; `highline-local-hook'
;;    It is evaluated always when highline is turned on locally.
;;
;; `highline-view-hook'
;;    It is evaluated always when indirect highline is turned on.
;;
;; `highline-load-hook'
;;    It is evaluated after highline package is loaded.
;;
;;
;; Options
;; -------
;;
;; Below it's shown a brief description of highline options, please, see the
;; options declaration in the code for a long documentation.
;;
;; `highline-face'			Specify face used to highlight the
;;					current line.
;;
;; `highline-vertical-face'		Specify face used to highlight other
;;					than current line.
;;
;; `highline-line'			Specify which part of line should be
;;					highlighted.
;;
;; `highline-vertical'			Specify how many vertical lines should
;;					be highlighted.
;;
;; `highline-verbose'			Non-nil means generate messages.
;;
;; `highline-ignore-regexp'		Specify regexp for buffers to ignore.
;;
;; `highline-priority'			Specify highline overlay priority.
;;
;; `highline-selected-window'		Non-nil means highlight current line on
;;					current window.
;;
;; To set the above options you may:
;;
;; a) insert the code in your ~/.emacs, like:
;;
;;	 (setq highline-face 'highlight)
;;
;;    This way always keep your default settings when you enter a new Emacs
;;    session.
;;
;; b) or use `set-variable' in your Emacs session, like:
;;
;;	 M-x set-variable RET highline-face RET highlight RET
;;
;;    This way keep your settings only during the current Emacs session.
;;
;; c) or use customization, for example:
;;	 click on menu-bar *Help* option,
;;	 then click on *Customize*,
;;	 then click on *Browse Customization Groups*,
;;	 expand *Editing* group,
;;	 expand *Highline* group
;;	 and then customize highline options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; d) or see the option value:
;;
;;	 C-h v highline-face RET
;;
;;    and click the *customize* hypertext button.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;; e) or invoke:
;;
;;	 M-x highline-customize RET
;;
;;    and then customize highline options.
;;    Through this way, you may choose if the settings are kept or not when
;;    you leave out the current Emacs session.
;;
;;
;; Acknowledgements
;; ----------------
;;
;; Thanks to Sandip Chitale <sandip.chitale@brokat.com> for byte-compilation
;; tests.
;;
;; Thanks to Stephan Engelke <engelke@gmx.ne> for XEmacs tests.
;;
;; Thanks to Roman Belenov <roman@nstl.nnov.ru> for `pre-command-hook'
;; suggestion.
;;
;; Thanks to Trey Jackson <bigfaceworm@hotmail.com> for `highline-line'
;; enhancements.
;;
;; Thanks to Fredrik Sundstroem <fresun-7@sm.luth.se> for permanent-local
;; overlay property indication.
;;
;; Thanks to:
;;    Bill Brodie <wbrodie@panix.com>		   linemenu.el
;;    Dave Love <fx@gnu.org>			   hl-line.el
;;    Christoph Conrad <christoph.conrad@gmx.de>   highlight-current-line.el
;; And to all people who contributed with them.
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

