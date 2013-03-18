
;; Append to list
;;
(defun math-append-to-list (list elem)
  "Appends elem to the end of list."
  (setcdr (last list) (cons elem nil)))

;; Creating and manipulating instances of the math-token class.
;;
(defconst math-token-prefix-left-bp-index 0 "The slot for prefix-left-bp")
(defconst math-token-prefix-fn-index 1 "The slot for prefix-fn")
(defconst math-token-infix-left-bp-index 2 "The slot index for infix-left-bp")
(defconst math-token-infix-fn-index 3 "The slot for infix-fn")
(defconst math-token-id-index 4 "The slot for id")
(defconst math-token-src-index 5 "The slot for src")
(defconst math-token-last-index 6 "The number of slots")

(defun math-token-make-instance (pair)
  "Create a token instance from the pair (type . value)."
  (let ((new-obj (make-vector math-token-last-index nil)))
    (aset new-obj math-token-id-index (if (equal (car pair) :operator) (cdr pair) (car pair)))
    (aset new-obj math-token-src-index (cdr pair))
    new-obj))

;; Getting the left-bp, right-bp, prefix-parse, infix-parse, id and src attributes for a token.
;;
(defun math-token-prefix-left-bp (token)
  "The prefix left binding power for token."
  (aref token math-token-prefix-left-bp-index))

(defun math-token-prefix-fn (token)
  "The prefix parsing function for token."
  (aref token math-token-prefix-fn-index))

(defun math-token-infix-left-bp (token)
  "The infix left binding power for token."
  (aref token math-token-infix-left-bp-index))

(defun math-token-infix-fn (token)
  "The infix parsing function for token."
  (aref token math-token-infix-fn-index))

(defun math-token-id (token)
  "The identifier for token."
  (aref token math-token-id-index))

(defun math-token-src (token)
  "The verbatim source code from which token was derived."
  (aref token math-token-src-index))

;; Setting the left-bp, right-bp, prefix-parse, infix-parse attributes for a token.
;;
(defun math-token-set-prefix-left-bp (token bp)
  "Set prefix left binding power for token."
  (aset token math-token-prefix-left-bp-index bp))

(defun math-token-set-prefix-fn (token fn)
  "Set prefix parsing function for token."
  (aset token math-token-prefix-fn-index fn))

(defun math-token-set-infix-left-bp (token bp)
  "Set infix left binding power for token."
  (aset token math-token-infix-left-bp-index bp))

(defun math-token-set-infix-fn (token fn)
  "Set infix parsing function for token."
  (aset token math-token-infix-fn-index fn))

(provide 'math-util)
