;;; Commentary:

;; This minor mode will recognize various commands in shell mode and
;; take one of two actions.  For "build" commands (currently make,
;; valgrind, and ant), it will enable compilation-shell-minor-mode.
;; For "gdb", it will invoke Emacs' built-in gdb rather than running
;; it in the shell buffer.  This is especially handy if you use
;; "gdb --args".

