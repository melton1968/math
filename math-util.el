
;; Creating and manipulating instances of the math-token class.
;;
(defconst math-token-left-bp-index 0 "The object index for id left-bp token")
(defconst math-token-right-bp-index 1 "The object index for id right-bp token")
(defconst math-token-prefix-parse-index 2 "The object index for prefix-parse in token")
(defconst math-token-infix-parse-index 3 "The object index for infix-parse in token")
(defconst math-token-id-index 4 "The object index for id in token")
(defconst math-token-src-index 5 "The object index for src in token")

(defun math-token-make-instance (pair)
  "Create a token instance from the pair (type . value)."
  (let ((new-obj (make-vector 6 nil)))
    (aset new-obj math-token-id-index (if (equal (car pair) :operator) (cdr pair) (car pair)))
    (aset new-obj math-token-src-index (cdr pair))
    new-obj))

;; Getting the left-bp, right-bp, prefix-parse, infix-parse, id and src attributes for a token.
;;
(defun math-token-left-bp (token)
  "The left binding power for token."
  (aref token math-token-left-bp-index))

(defun math-token-right-bp (token)
  "The right binding power for token."
  (aref token math-token-right-bp-index))

(defun math-token-prefix-parse (token)
  "The prefix parsing function for token."
  (aref token math-token-prefix-parse-index))

(defun math-token-infix-parse (token)
  "The infix parsing function for token."
  (aref token math-token-infix-parse-index))

(defun math-token-id (token)
  "The identifier for token."
  (aref token math-token-id-index))

(defun math-token-src (token)
  "The verbatim source code from which token was derived."
  (aref token math-token-src-index))

;; Setting the left-bp, right-bp, prefix-parse, infix-parse attributes for a token.
;;
(defun math-token-set-left-bp (token bp)
  "Set left binding power for token."
  (aset token math-token-left-bp-index bp))

(defun math-token-set-right-bp (token bp)
  "Set right binding power for token."
  (aset token math-token-right-bp-index bp))

(defun math-token-set-prefix-parse (token fn)
  "Set prefix parsing function for token."
  (aset token math-token-prefix-parse-index fn))

(defun math-token-set-infix-parse (token fn)
  "Set infix parsing function for token."
  (aset token math-token-infix-parse-index fn))

(provide 'math-util)
