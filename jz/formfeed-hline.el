;;; formfeed-hline.el --- display formfeed with horizontal line

;; Copyright 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 4
;; Keywords: frames
;; URL: http://user42.tuxfamily.org/formfeed-hline/index.html

;; formfeed-hline.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; formfeed-hline.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `M-x formfeed-hline-mode' global minor mode displays control-L formfeeds
;; as a horizontal line like
;;
;;     ^L-------------------------------------
;;
;; The dashes follow the window width and the face is `escape-glyph' like
;; the default ^L display.  The ^L is still shown so it's familiar and
;; hopefully can't be confused with a literal line of dashes.  See the
;; docstring in `formfeed-hline-mode' below for more.

;;; Emacsen:

;; Designed for Emacs 21 and up, works in XEmacs 21.

;;; Install:

;; Put formfeed-hline.el somewhere in one of your `load-path' directories
;; and to your .emacs
;;
;;     (autoload 'formfeed-hline-mode "formfeed-hline" nil t)
;;
;; Then `M-x formfeed-hline-mode' to turn it on or off as desired, or make
;; it permanent with
;;
;;     (formfeed-hline-mode 1)
;;
;; There's an autoload cookie below for the function, if you know how to use
;; `update-file-autoloads' and friends.

;;; History:

;; Version 1 - the first version
;; Version 2 - defang xemacs defadvice for unload-feature
;; Version 3 - copy standard-display-table to gain other settings
;;           - unset window-display-table if standard, to help enriched-mode
;; Version 4 - correction to "Usual Display" info node link

;;; Code:


;; Cribs:
;;
;; Emacs uses only a single window/buffer/global display table, and doesn't
;; inherit from char-table-parent, so a new window table is created as a
;; copy of the standard-display-table to at least keep current settings from
;; there, though not future changes.  XEmacs doesn't need that as it looks
;; through all relevant window/frame/global display tables for each
;; character, so its new window-display-table can be just a
;; make-display-table.
;;

;;-----------------------------------------------------------------------------
;; xemacs21 incompatibilities

(eval-and-compile
  (if (eval-when-compile (fboundp 'window-display-table))
      ;; emacs21 up
      (defalias 'formfeed-hline--window-display-table
        'window-display-table)
    ;; xemacs
    (defun formfeed-hline--window-display-table (window)
      "Return the display table for WINDOW."
      (specifier-instance current-display-table window))))

(eval-and-compile
  (if (eval-when-compile (fboundp 'set-window-display-table))
      ;; emacs21 up
      (defalias 'formfeed-hline--set-window-display-table
        'set-window-display-table)
    ;; xemacs
    (defun formfeed-hline--set-window-display-table (window table)
      "Set the display table for WINDOW to DISPLAY-TABLE."
      (if table
          (add-spec-to-specifier current-display-table table window)
        (remove-specifier current-display-table window))
      table)))


;;-----------------------------------------------------------------------------
;; emacs22 new stuff

(eval-and-compile
  (cond ((eval-when-compile (fboundp 'make-glyph-code))
         ;; emacs22 up
         (defalias 'formfeed-hline--make-glyph-code 'make-glyph-code))

        ((eval-when-compile (fboundp 'face-id))
         ;; emacs21
         (defun formfeed-hline--make-glyph-code (char &optional face)
           "Return a glyph code (an integer) for CHAR with FACE.
The same as `make-glyph-code' in Emacs 22."
           (logior char (* 524288 (if face (face-id face) 0)))))

        ;; nothing for xemacs
        ))

;;-----------------------------------------------------------------------------
;; generic

(defun formfeed-hline-char-table-equal (table1 table2)
  "Return non-nil if char tables TABLE1 and TABLE2 are equal.
Doesn't work and not used in xemacs21."
  (and (char-table-p table1) ;; nil not equal
       (char-table-p table2)
       (eq (char-table-subtype table1)
           (char-table-subtype table2))
       (catch 'done
         (dotimes (i (or (get (char-table-subtype table1)
                              'char-table-extra-slots)
                         0))
           (unless (equal (char-table-extra-slot table1 i)
                          (char-table-extra-slot table2 i))
             (throw 'done nil)))
         (let (lst)
           (map-char-table (lambda (chars value)
                             (if value
                                 (push (cons chars value) lst)))
                           table1)
           (setq lst (nreverse lst))
           (map-char-table (lambda (chars value)
                             (and value
                                  (not (equal (pop lst) (cons chars value)))
                                  (throw 'done nil)))
                           table2)
           (null lst)))))

(defun formfeed-hline-display-table-string-face (str face)
  "Return STR with FACE applied suitable for use in a display table.
This means a vector of `make-glyph-code' in Emacs.
In XEmacs currently STR itself is returned (should it be some
sort of `make-glyph' to apply a face?)."
  (when (eval-when-compile (fboundp 'formfeed-hline--make-glyph-code)) ;; emacs
    (setq str (vconcat (mapcar (lambda (c)
                                 (formfeed-hline--make-glyph-code c face))
                               str))))
  str)

;;-----------------------------------------------------------------------------

(defun formfeed-hline-window-size-change (frame)
  "Update formfeed display line width for window size changes.
This function is put into `window-size-change-functions' by
`formfeed-hline-mode'.  Some or all of the windows in FRAME have
a new width and/or height."

  ;; Most of the time only the height changes, eg. when the minibuffer goes
  ;; multi-line or splits horizontally.  The existing dashes width is
  ;; checked in the display table to see if an update is actually required.
  ;;
  (dolist (window (window-list frame t))
    (unless (or (window-minibuffer-p window)
                (zerop (window-width window))) ;; zero when window killed
      (let* ((table      (formfeed-hline--window-display-table window))
             (old-dashes (if table
                             ;; -3 for "^L\n" added to dashes
                             (- (length (aref table ?\f)) 3)
                           0))
             ;; -3 for ^L and blank at right
             (dashes     (max 0 (- (window-width window) 3))))
        (when (/= dashes old-dashes)
          ;; (message "change dashes %S to %S on %s" old-dashes dashes window)

          (if (zerop dashes)
              ;; unset when window too narrow
              (when table
                (aset table ?\f nil))

            ;; window can fit dashes, install string

            ;; Emacs doesn't inherit anything in a window-display-table, so
            ;; start as a copy of standard-display-table.  XEmacs looks
            ;; through all per-character, so start as empty.
            (unless table
              (require 'disp-table) ;; initialize `standard-display-table'
              (setq table
                    (if (eval-when-compile (boundp 'standard-display-table))
                        (copy-sequence standard-display-table) ;; emacs
                      (make-display-table))) ;; xemacs
              (formfeed-hline--set-window-display-table window table))

            (aset table
                  ?\f
                  (formfeed-hline-display-table-string-face
                   (concat "^L" (make-string dashes ?-) ) ;; "\n"
                   (and (facep 'escape-glyph) ;; not in emacs21,xemacs21
                        'escape-glyph))))

          ;; xemacs only notices a change in the table when it's set into
          ;; the specifier
          (if (eval-when-compile (featurep 'xemacs))
              (formfeed-hline--set-window-display-table window table)))))))

;;;###autoload
(define-minor-mode formfeed-hline-mode
  "Display form-feeds as ^L and a row of dashes.

    ^L-----------------------------------------------------

The dashes follow the window width and use `escape-glyph' face
the same as other control characters.  It's applied to ordinary
windows, not the minibuffer window.

A line is good if the default ^L is not enough visual indication.
\(See Info node `(elisp)Usual Display' on the default.)

-----
`ctl-arrow' is ignored, you get \"^L----\" even if ctl-arrow is
set to have octal for other control characters.  Perhaps this
will change in the future.

`escape-glyph' face is new in Emacs 22.  If you create that face
yourself in Emacs 21 then `formfeed-hline-mode' will use it.  (No
faces are applied in xemacs21 currently.)

XEmacs 21.4.22 has some dodginess in the display of multiple
consecutive ^L's.  Only every second one displays (or something
like that) when using the formfeed-hline row of dashes.

`enriched-mode' has its own display table setup for formfeed as a
line of dashes.  `formfeed-hline-mode' ends up overriding that.

The formfeed-hline.el home page is
URL `http://user42.tuxfamily.org/formfeed-hline/index.html'"

  :global t

  (if formfeed-hline-mode
      ;; enable
      (progn
        (add-to-list 'window-size-change-functions
                     'formfeed-hline-window-size-change)
        ;; initial setups
        (mapc 'formfeed-hline-window-size-change (frame-list)))

    ;; disable
    (setq window-size-change-functions
          (remq 'formfeed-hline-window-size-change
                window-size-change-functions))
    (walk-windows
     (lambda (window)
       (let ((table (formfeed-hline--window-display-table window)))
         (when table
           (if (eval-when-compile (boundp 'standard-display-table))
               ;; emacs
               (progn
                 (aset table ?\f (aref standard-display-table ?\f))
                 ;; discard window table if now standard; this lets a
                 ;; buffer-display-table show, eg. in enriched-mode
                 (and (boundp 'standard-display-table)
                      (formfeed-hline-char-table-equal table
                                                       standard-display-table)
                      (formfeed-hline--set-window-display-table window nil)))
             ;; xemacs
             (aset table ?\f nil)
             ;; display only updates when specifier set
             (formfeed-hline--set-window-display-table window table)))))
     nil  ;; not minibuffer
     t))) ;; all frames

;; Could have been cute to put a `formfeed-hline-enable' as a customize
;; option on one of after-init-hook, emacs-startup-hook, term-setup-hook or
;; similar.  But as of Emacs 23 they're all just defvar not defcustom.

;; emacs22 up can disable on `unload-feature', but emacs21,xemacs21 must
;; manually disable or will leave an undefined func in
;; window-size-change-functions
;;
(defun formfeed-hline-unload-function ()
  "Undo display table changes on `unload-feature'."
  (formfeed-hline-mode 0)
  nil) ;; and normal unload-feature actions


;;-----------------------------------------------------------------------------
;; xemacs workarounds

;; XEmacs 21.4.22 has the `window-size-change-functions' variable, but it's
;; not implemented, ie. its functions are not called.  Notice size changes
;; instead by hooking onto functions which might change sizes or create new
;; windows.  Not sure if this is everything, but user-level window creation
;; goes through `split-window' at least.
;;
(when (eval-when-compile (featurep 'xemacs))
  ;; unload-feature doesn't run an -unload-hook or -unload-function in
  ;; xemacs 21.4.22, so just defang with a boundp check against unloaded
  (defadvice split-window (after formfeed-hline activate)
    "Notice window size change for `formfeed-hline-mode'."
    (if (and (boundp 'formfeed-hline-mode)
             formfeed-hline-mode)
        (formfeed-hline-window-size-change (window-frame window))))
  (defadvice set-window-configuration (after formfeed-hline activate)
    "Notice window size change for `formfeed-hline-mode'."
    (if (and (boundp 'formfeed-hline-mode)
             formfeed-hline-mode)
        (mapc 'formfeed-hline-window-size-change (frame-list)))))


(provide 'formfeed-hline)

;;; formfeed-hline.el ends here
