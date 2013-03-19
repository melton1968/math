
;; Append to list
;;
(defun math-append-to-list (list elem)
  "Appends elem to the end of list."
  (setcdr (last list) (cons elem nil)))

;; Creating and manipulating instances of the math-token class.
;;
(defconst math-token-nud-left-bp-index 0 "The nud-left-bp")
(defconst math-token-nud-fn-index 1 "The nud-fn")
(defconst math-token-led-left-bp-index 2 "The led-left-bp")
(defconst math-token-led-fn-index 3 "The led-fn")
(defconst math-token-id-index 4 "The token id")
(defconst math-token-src-index 5 "The verbatim source code.")
(defconst math-token-file-index 6 "The source code file name.")
(defconst math-token-line-index 7 "The source code line number.")
(defconst math-token-begin-index 8 "The start column of the token in the source code.")
(defconst math-token-end-index 9 "The end column of the token in the source code.")
(defconst math-token-level-index 10 "The matchfix depth immediately following token.")
(defconst math-token-last-index 11 "The number of slots")

(defun math-token-make-instance (type src)
  "Create a token instance."
  (let ((new-obj (make-vector math-token-last-index nil)))
    (aset new-obj math-token-id-index (if (equal type :operator) src type))
    (aset new-obj math-token-src-index src)
    (aset new-obj math-token-file-index (buffer-name))
    (aset new-obj math-token-line-index (line-number-at-pos (point)))
    (aset new-obj math-token-begin-index nil)
    (aset new-obj math-token-end-index nil)
    (aset new-obj math-token-level-index (nth 0 (syntax-ppss)))
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

(defun math-token-file (token)
  "The source file from which token was derived."
  (aref token math-token-file-index))

(defun math-token-line (token)
  "The source file line number from which token was derived."
  (aref token math-token-line-index))

(defun math-token-begin (token)
  "The source file column number of the start of the token."
  (aref token math-token-begin-index))

(defun math-token-end (token)
  "The source file column number of the end of the token."
  (aref token math-token-end-index))

(defun math-token-level (token)
  "The matchfix depth level for the point just after this token."
  (aref token math-token-level-index))

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
