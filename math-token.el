
(require 'math-attrs)

;; Token class
;;
;; External names: math-token-*
;; Internal names: math-tok--*
;;

(defconst math-tok--class-index 0 "The token class.")
(defconst math-tok--source-index 1 "The verbatim source code.")
(defconst math-tok--begin-index 2 "The point just before token.")
(defconst math-tok--end-index 3 "The point just after token.")
(defconst math-tok--last-index 4 "The number of slots")

(defun math-token-make-instance (class source)
  "Create a token instance."
  (let ((new-obj (make-vector math-tok--last-index nil)))
    (aset new-obj math-tok--class-index class)
    (aset new-obj math-tok--source-index source)
    (aset new-obj math-tok--begin-index (- (point) (if (stringp source) (length source) 1)))
    (aset new-obj math-tok--end-index (point))
    new-obj))

;; Getting the token attributes.
;;
(defun math-token-nud-name (token)
  "The nud name for token."
  (let ((attrs (gethash (math-token-id token) math-nud-table)))
    (if attrs (math-attributes-name attrs) nil)))

(defun math-token-nud-bp (token)
  "The nud binding power for token."
  (let ((attrs (gethash (math-token-id token) math-nud-table)))
    (if attrs (math-attributes-bp attrs) nil)))

(defun math-token-nud-fn (token)
  "The nud parsing function for token."
  (let ((attrs (gethash (math-token-id token) math-nud-table)))
    (if attrs (math-attributes-fn attrs) nil)))

(defun math-token-led-name (token)
  "The led name for token."
  (let ((attrs (gethash (math-token-id token) math-led-table)))
    (if attrs (math-attributes-name attrs) nil)))

(defun math-token-led-bp (token)
  "The led binding power for token."
  (let ((attrs (gethash (math-token-id token) math-led-table)))
    (if attrs (math-attributes-bp attrs))))

(defun math-token-led-fn (token)
  "The led parsing function for token."
  (let ((attrs (gethash (math-token-id token) math-led-table)))
    (if attrs (math-attributes-fn attrs) nil)))

(defun math-token-class (token)
  "The token class: `:eof' `:eol' `:identifier' `:string' `:number' `:operator'."
  (aref token math-tok--class-index))

(defun math-token-id (token)
  "The identifier: Same as class except :operator is replaced by the actual operator string."
  (let ((class (aref token math-tok--class-index)))
    (if (equal class :operator) (aref token math-tok--source-index) class)))

(defun math-token-source (token)
  "The verbatim source code from which token was derived."
  (aref token math-tok--source-index))

(defun math-token-file (token)
  "The source file from which token was derived."
  (buffer-name))

(defun math-token-line (token)
  "The source file line number from which token was derived."
  (line-number-at-pos (aref token math-tok--begin-index)))

(defun math-token-begin (token)
  "The source file point of the start of the token."
  (aref token math-tok--begin-index))

(defun math-token-end (token)
  "The source file point of the end of the token."
  (aref token math-tok--end-index))

(defun math-token-level (token)
  "The matchfix depth level for the point just after this token."
  (nth 0 (syntax-ppss (aref token math-tok--end-index))))

(provide 'math-token)
