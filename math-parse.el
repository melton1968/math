
(require 'math-token)

(defconst math-parser-table (make-hash-table :test 'equal)
  "The parser symbol table.

Each entry in the parser table is an alist with the
`:parse-prefix' and `:parse-index' functions (or nil) for the
given symbol.
")

(defconst math-parser-token nil
  "The current parser token.")

(defun math-parser-table-put (identifier &optional new-properties)
  "Add new-properties to identifier's properties, adding
identifier to the parser-table if necessary."
  ;; Get the current properties or the empty list
  (let ((properties (or (gethash identifier math-parser-table) (list))))
    ;; Check that property is not already set in the property table.
    (dolist (new-property new-properties)
      (let ((value (assoc new-property properties)))
	(if value
	    (error "Identifier %s already has value %s associated with property %s"
		   identifier value new-property))))
    ;; Combine the new properties with the existing properties and
    ;; update the parser-table.
    (let ((updated-properties (append new-properties properties)))
      (puthash identifier updated-properties math-parser-table))))
      
(defun math-parser-table-property (identifier property)
  "Return the value of property for indentifier from the parser-table."
  (cdr (assoc property (gethash identifier math-parser-table))))

(defun math-parser-default-parse-literal (token)
  `(,(cdr (assoc :sub-class token)) ,(cdr (assoc :value token))))

(defun math-parser-literal (identifier &optional parse-prefix)
  (math-parser-table-put identifier '((:parse-prefix . math-parser-default-parse-literal))))

(defun math-parser-default-infix-left (left-expression token)
  (list 
   (cdr (assoc :value token))
   left-expression 
   (math-parser-parse (math-parser-table-property :left-binding-power (assoc :identifier token)))))

(defun math-parser-infix-left (identifier left-binding-power &optional parse-infix)
  (let ((lbp-pair (list (cons :left-binding-power left-binding-power)))
	(pi-pair (list (cons :parse-infix (or parse-infix 'math-parser-default-infix-left)))))
    (math-parser-table-put identifier lbp-pair)
    (math-parser-table-put identifier pi-pair)))

(defun math-parser-token-identifier (token)
  (let ((class (cdr (assoc :class token)))
	 (sub-class (cdr (assoc :sub-class token)))
	 (value (cdr (assoc :value token))))
    (cond
     ((equal sub-class :number) "number")
     ((equal sub-class :string) "string")
     ((equal class :name) "name")
     ((equal class :operator) value)
     ((equal class :eof) "eof")
     ((equal class :unknown) "unknown"))))
	
(defun math-parser-next-token ()
  (let* ((token (math-forward-token))
	 (identifier (math-parser-token-identifier token)))
      (setq math-parser-token (append (list (cons :identifier identifier)) token))))

(defun math-parser-parse (right-binding-power)
  (math-parser-next-token)
  (message "parse(pre-prefix) token: %s" math-parser-token)

  (let ((identifier (cdr (assoc :identifier math-parser-token))))
    (let ((parse-prefix (math-parser-table-property identifier :parse-prefix)))
      (unless parse-prefix 
	(error "No parse-prefix associated with identifier %s for token %s" 
	       identifier math-parser-token))
      (let ((left-expression (funcall parse-prefix math-parser-token)))

	(message "parse(after-prefix) left-expr: %s" left-expression)

	(math-parser-next-token)
	(message "parse(pre-infix) token: %s" math-parser-token)

	(let ((identifier (cdr (assoc :identifier math-parser-token))))
	  (let ((parse-infix (math-parser-table-property identifier :parse-infix)))

	    (if parse-infix
		(let ((new-expression (funcall parse-infix left-expression math-parser-token)))
		      (message "parse(after-infix/t) left-expr: %s" new-expression)
		      new-expression)
	      (message "parse(after-infix/nil) left-expr: %s" left-expression)
	      left-expression)))))))

(defun math-parse ()
  (save-excursion
    (goto-char (point-min))
    (math-parser-parse 0)))

(defun math-parse-command ()
  (interactive)
  (message "math-parse: %s" (math-parse)))

(math-parser-literal "number")
(math-parser-literal "string")
;;(math-parser-name "name")

;;(math-parser-prefix "+")
;;(math-parser-prefix "-")

(math-parser-infix-left "+" 20)
;;(math-parser-infix-left "-" 20)
;;(math-parser-infix-left "*" 30)
;;(math-parser-infix-left "/" 30)
;;(math-parser-infix-left "^" 40)

(provide 'math-parse)
