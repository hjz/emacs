(defconst groc-script "/Users/jz/sc/groc/bin/groc")

;; XXX
;; doesn't handle escaped angle brackets which throw off columns
;; maybe try to get a more suitable format out of opengrok
;(add-to-list 'grep-regexp-alist '("^\(.+\)\:\([0-9]+\) .*" 2 1))

(defun groc-fulltext (text)
  "Run OpenGrok fulltext search."
  (interactive "sfulltext: ")
  (grep (concat groc-script " -f " text)))

(defun groc-definition (text)
  "Run OpenGrok definition search."
  (interactive "sdefinition: ")
  (grep (concat groc-script " -d " text)))

(defun groc-reference (text)
  "Run OpenGrok reference search."
  (interactive "sreference: ")
  (grep (concat groc-script " -r " text)))
