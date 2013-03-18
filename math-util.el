
;; Append to list
;;
(defun math-append-to-list (list elem)
  "Appends elem to the end of list."
  (setcdr (last list) (cons elem nil)))

;; Creating and manipulating instances of the math-token class.
;;
(defconst math-token-nud-left-bp-index 0 "The slot for nud-left-bp")
(defconst math-token-nud-fn-index 1 "The slot for nud-fn")
(defconst math-token-led-left-bp-index 2 "The slot index for led-left-bp")
(defconst math-token-led-fn-index 3 "The slot for led-fn")
(defconst math-token-id-index 4 "The slot for id")
(defconst math-token-src-index 5 "The slot for src")
(defconst math-token-last-index 6 "The number of slots")

(defun math-token-make-instance (pair)
  "Create a token instance from the pair (type . value)."
  (let ((new-obj (make-vector math-token-last-index nil)))
    (aset new-obj math-token-id-index (if (equal (car pair) :operator) (cdr pair) (car pair)))
    (aset new-obj math-token-src-index (cdr pair))
    new-obj))

;; Getting the left-bp, right-bp, nud-parse, led-parse, id and src attributes for a token.
;;
(defun math-token-nud-left-bp (token)
  "The nud left binding power for token."
  (aref token math-token-nud-left-bp-index))

(defun math-token-nud-fn (token)
  "The nud parsing function for token."
  (aref token math-token-nud-fn-index))

(defun math-token-led-left-bp (token)
  "The led left binding power for token."
  (aref token math-token-led-left-bp-index))

(defun math-token-led-fn (token)
  "The led parsing function for token."
  (aref token math-token-led-fn-index))

(defun math-token-id (token)
  "The identifier for token."
  (aref token math-token-id-index))

(defun math-token-src (token)
  "The verbatim source code from which token was derived."
  (aref token math-token-src-index))

;; Setting the left-bp, right-bp, nud-parse, led-parse attributes for a token.
;;
(defun math-token-set-nud-left-bp (token bp)
  "Set nud left binding power for token."
  (aset token math-token-nud-left-bp-index bp))

(defun math-token-set-nud-fn (token fn)
  "Set nud parsing function for token."
  (aset token math-token-nud-fn-index fn))

(defun math-token-set-led-left-bp (token bp)
  "Set led left binding power for token."
  (aset token math-token-led-left-bp-index bp))

(defun math-token-set-led-fn (token fn)
  "Set led parsing function for token."
  (aset token math-token-led-fn-index fn))

(provide 'math-util)
