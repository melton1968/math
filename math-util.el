
;; Append to list
;;
(defun math-append-to-list (list elem)
  "Appends elem to the end of list."
  (setcdr (last list) (cons elem nil)))

;; math-attributes class
;;
(defconst math-attributes-bp-index 0 "The binding power.")
(defconst math-attributes-fn-index 1 "The parsing function.")
(defconst math-attributes-name-index 2 "The name.")
(defconst math-attributes-last-index 3 "The number of slots.")

(defun math-attributes-make-instance (bp fn name)
  "Create a attributes instance."
  (let ((new-obj (make-vector math-attributes-last-index nil)))
    (aset new-obj math-attributes-bp-index bp)
    (aset new-obj math-attributes-fn-index fn)
    (aset new-obj math-attributes-name-index name)
    new-obj))

(defun math-attributes-bp (attrs)
  "The binding power attribute."
  (aref attrs math-attributes-bp-index))

(defun math-attributes-fn (attrs)
  "The parsing function attribute."
  (aref attrs math-attributes-fn-index))

(defun math-attributes-name (attrs)
  "The name attribute."
  (aref attrs math-attributes-name-index))

;; Getting and setting attributes in the parser tables.
;;
(defun math-get-attributes (key table)
  (let ((attrs (gethash key table)))
    (unless attrs (error "No attributes for `%s' in parser table `%s'" key table))
    attrs))

(defun math-set-attributes (key attrs table)
  (let ((attrs (gethash key table)))
    (if attrs (error "Attributes already set for `%s' in parser table `%s'" key table)))
  (puthash key attrs table))

;; math-token class
;;
(defconst math-token-class-index 0 "The token class.")
(defconst math-token-source-index 1 "The verbatim source code.")
(defconst math-token-begin-index 2 "The point just before token.")
(defconst math-token-end-index 3 "The point just after token.")
(defconst math-token-last-index 4 "The number of slots")

(defun math-token-make-instance (class source)
  "Create a token instance."
  (let ((new-obj (make-vector math-token-last-index nil)))
    (aset new-obj math-token-class-index class)
    (aset new-obj math-token-source-index source)
    (aset new-obj math-token-begin-index (- (point) (if (stringp source) (length source) 1)))
    (aset new-obj math-token-end-index (point))
    new-obj))

;; Getting the token attributes.
;;
(defun math-token-nud-name (token)
  "The nud name for token."
  (math-attributes-name (math-get-attributes (math-token-id token) math-nud-table)))

(defun math-token-nud-bp (token)
  "The nud binding power for token."
  (math-attributes-bp (math-get-attributes (math-token-id token) math-nud-table)))

(defun math-token-nud-fn (token)
  "The nud parsing function for token."
  (math-attributes-fn (math-get-attributes (math-token-id token) math-nud-table)))

(defun math-token-led-name (token)
  "The led name for token."
  (math-attributes-name (math-get-attributes (math-token-id token) math-led-table)))

(defun math-token-led-bp (token)
  "The led binding power for token."
  (math-attributes-bp (math-get-attributes (math-token-id token) math-led-table)))

(defun math-token-led-fn (token)
  "The led parsing function for token."
  (math-attributes-fn (math-get-attributes (math-token-id token) math-led-table)))

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
  (buffer-name))

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

(provide 'math-util)
