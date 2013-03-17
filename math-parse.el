
(require 'math-token)

(defconst math-parser-token nil
  "The current parser token.")

(defconst math-parser-table (make-hash-table :test 'equal)
  "The parser property table. 

The property table has an entry for each possible token
identifier, i.e. `(number)', `(string)', `(name)', `(eof)', 
`(unknown)', and each operator.

Each entry contains an association list for that
identifier with the identifier's `:left-binding-power', `:prefix'
parse function and `:infix' parse function.
")

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

;;;; The default parse methods for literal, infix-left, prefix.
;;;;
(defun math-parser-default-parse-literal (token)
  "Parse a literal token into `(token-type value)'"
  `(,(cdr (assoc :identifier token)) ,(cdr (assoc :value token))))

(defun math-parser-default-parse-infix-left (left-expression token)
  "Parse a left associaive infix operator into `(operator (sub-expr) (sub-expr))'"
  `(,(cdr (assoc :value token))
    ,left-expression
    ,(math-parser-parse (cdr (assoc :left-binding-power token)))))

;;;; Methods for adding identifiers for literals, names, and operators to the parse table.
;;;;
(defun math-parser-id-literal (identifier &optional prefix)
  "Add the given literal identifier to the parse table."
  (math-parser-table-put 
   identifier 
   `((:prefix             . ,(or prefix 'math-parser-default-parse-literal))
     (:left-binding-power . 0))))

(defun math-parser-id-infix-left (identifier left-binding-power &optional infix)
  "Add the given infix identifier to the parse table."
  (math-parser-table-put 
   identifier
   `((:left-binding-power . ,left-binding-power)
     (:infix              . ,(or infix 'math-parser-default-parse-infix-left)))))


;;;; The core parsing methods.
;;;;	
(defun math-parser-token-property (property)
  (cdr (assoc property math-parser-token)))

(defun math-parser-next-token ()
  "Get the next token and set its shared properties based on the parser table."
  (let* ((token (math-next-token))
	 (shared-properties (gethash (cdr (assoc :identifier token)) math-parser-table)))
    (setcdr (last token) shared-properties)
    (setq math-parser-token token)))

(defun math-parser-peek-token-property (property)
  (let* ((token (math-peek-token))
	 (shared-properties (gethash (cdr (assoc :identifier token)) math-parser-table)))
    (cdr (assoc property shared-properties))))
  

(defun math-parser-parse (right-binding-power)
  ;; Get the first token of the current expression.
  (math-parser-next-token)

  ;; Get the prefix function for parsing the current token. Since this
  ;; the first token, there must be a prefix function.
  (let ((prefix (math-parser-token-property :prefix)))
      (unless prefix 
	(error "No prefix function for token %s" math-parser-token))

      ;; Apply the prefix function to get the parsed left sub-expression.
      (let ((left-expression (funcall prefix math-parser-token)))

	;; 
	(message "token %s" (math-peek-token))
	(message "right-binding-power %d" right-binding-power)
	(message "left-binding-power  %d" (math-parser-peek-token-property :left-binding-power))
	(while (< right-binding-power (math-parser-peek-token-property :left-binding-power))

	  ;; Get the next token, which must be an infix operator since
	  ;; we just parsed the left-subexpression.
	  (math-parser-next-token)

	  ;; Gett the infix function for parsing the current
	  ;; token. Since we just parsed the left sub-expression, there
	  ;; must be an infix function (unless we have reached a special
	  ;; end of expression operator which has a null infix function).
	  (let ((infix (math-parser-token-property :infix)))
	    (unless infix
	      (error "No infix function for token %s" math-parser-token))

	    (setq left-expression (funcall infix left-expression math-parser-token)))
	  (message "token %s" (math-peek-token))
	  (message "left-binding-power  %d" (math-parser-peek-token-property :left-binding-power))
	  )

	left-expression)))
	    

(defun math-parse ()
  (save-excursion
    (goto-char (point-min))
    (math-parser-parse 0)))

(defun math-parse-command ()
  (interactive)
  (message "math-parse: %s" (math-parse)))

(math-parser-id-literal math-token-number)
(math-parser-id-literal math-token-string)
(math-parser-id-literal math-token-eof)

;;(math-parser-name math-token-name)

;;(math-parser-prefix "+")
;;(math-parser-prefix "-")

(math-parser-id-infix-left "+" 20)
(math-parser-id-infix-left "-" 20)
(math-parser-id-infix-left "*" 30)
(math-parser-id-infix-left "/" 30)
;;(math-parser-id-infix-right "^" 40)

(provide 'math-parse)
