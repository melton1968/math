
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
(defconst math-token-class-index 4 "The token class.")
(defconst math-token-source-index 5 "The verbatim source code.")
(defconst math-token-begin-index 6 "The point just before token.")
(defconst math-token-end-index 7 "The point just after token.")
(defconst math-token-last-index 8 "The number of slots")

(defun math-token-make-instance (class source)
  "Create a token instance."
  (let ((new-obj (make-vector math-token-last-index nil)))
    (aset new-obj math-token-class-index class)
    (aset new-obj math-token-source-index source)
    (aset new-obj math-token-begin-index (- (point) (if (stringp source) (length source) 1)))
    (aset new-obj math-token-end-index (point))
    new-obj))

;; Getting the attributes.
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

(defun math-token-class (token)
  "The token class: `:eof' `:eol' `:identifier' `:string' `:number' `:operator'."
  (aref token math-token-class-index))

(defun math-token-id (token)
  "The identifier: Same as class except :operator is replaced by the actual operator string."
  (let ((class (aref token math-token-class-index)))
    (if (equal class :operator) (aref token math-token-source-index) class)))

(defun math-token-source (token)
  "The verbatim source code from which token was derived."
  (aref token math-token-source-index))

(defun math-token-file (token)
  "The source file from which token was derived."
  (current-buffer-name))

(defun math-token-line (token)
  "The source file line number from which token was derived."
  (line-number-at-pos (aref token math-token-begin-index)))

(defun math-token-begin (token)
  "The source file column number of the start of the token."
  (aref token math-token-begin-index))

(defun math-token-end (token)
  "The source file column number of the end of the token."
  (aref token math-token-end-index))

(defun math-token-level (token)
  "The matchfix depth level for the point just after this token."
  (nth 0 (syntax-ppss (aref token math-token-end-index))))

;; Setting the left-bp, right-bp, nud-parse, led-parse attributes.
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
