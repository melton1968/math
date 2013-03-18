
(require 'math-token)
(require 'math-util)

(defconst math-cur-tok nil
  "The current parser token.")

(defconst math-prefix-left-bp-table (make-hash-table :test 'equal)
  "Maps a token identifier to the identifier's prefix left binding power.")

(defconst math-prefix-fn-table (make-hash-table :test 'equal)
  "Maps a token identifier to the identifier's prefix parse function.")

(defconst math-infix-left-bp-table (make-hash-table :test 'equal)
  "Maps a token identifier to the identifier's infix left binding power.")

(defconst math-infix-fn-table (make-hash-table :test 'equal)
  "Maps a token identifier to the identifier's infix parse function.")

(defun math-put-table (key value table)
  (if (gethash key table)
      (error "Identifier %s already has an entry in table %s" key table)
    (puthash key value table)))

(defun math-get-table (key table)      
  (gethash key table))

;; Parse methods for literal, infix-left, prefix.
;;
(defun math-parse-prefix-literal (token)
  "Parse a literal token into `(token-type value)'"
  (math-token-src token))

(defun math-parse-prefix (token)
  "Parse a left associaive infix operator into `(operator expr)'"
  `(,(math-token-id token) ,(math-parse-expression (math-token-prefix-left-bp token))))

(defun math-parse-infix-left (left-expression token)
  "Parse a left associaive infix operator into `(operator (sub-expr) (sub-expr))'"
  `(,(math-token-id token)
    ,left-expression
    ,(math-parse-expression (math-token-infix-left-bp token))))

(defun math-parse-infix-right (left-expression token)
  "Parse a left associaive infix operator into `(operator (sub-expr) (sub-expr))'"
  `(,(math-token-id token)
    ,left-expression
    ,(math-parse-expression (- (math-token-infix-left-bp token) 1))))

(defun math-parse-open-paren (left-expression token)
  (let (subexpr (math-parse-expression 0))
    (math-token-advance ")")
    `(,subexpr)))

;; Methods for registering identifiers in the parser tables.
;;
(defun math-register-symbol (identifier)
  (math-put-table identifier 0 math-prefix-left-bp-table)
  (math-put-table identifier 0 math-infix-left-bp-table))

(defun math-register-symbol-prefix (identifier bp fn)
  (math-put-table identifier bp math-prefix-left-bp-table)
  (math-put-table identifier fn math-prefix-fn-table))

(defun math-register-symbol-infix (identifier bp fn)
  (math-put-table identifier bp math-infix-left-bp-table)
  (math-put-table identifier fn math-infix-fn-table))

(defun math-register-literal (identifier &optional prefix)
  "Add the given literal identifier to the parser tables."
  (math-put-table identifier (or prefix 'math-parse-prefix-literal) math-prefix-fn-table)
  (math-put-table identifier 0 math-prefix-left-bp-table))

(defun math-register-prefix (identifier left-bp &optional prefix)
  "Add the given prefix identifier to the parser tables."
  (math-put-table identifier (or prefix 'math-parse-prefix) math-prefix-fn-table)
  (math-put-table identifier left-bp math-prefix-left-bp-table))

(defun math-register-infix-left (identifier left-bp &optional infix)
  "Add the given infix identifier to the parser tables."
  (math-put-table identifier (or infix 'math-parse-infix-left) math-infix-fn-table)
  (math-put-table identifier left-bp math-infix-left-bp-table))

(defun math-register-infix-right (identifier left-bp &optional infix)
  "Add the given infix identifier to the parser tables."
  (math-put-table identifier (or infix 'math-parse-infix-right) math-infix-fn-table)
  (math-put-table identifier left-bp math-infix-left-bp-table))

;; The core parsing methods.
;;	
(defun math-parser-advance-token ()
  "Get the next token and set its properties based on the parser tables."
  (let* ((token (math-token-make-instance (math-next-token)))
	 (identifier (math-token-id token)))
    (math-token-set-prefix-left-bp token (math-get-table identifier math-prefix-left-bp-table))
    (math-token-set-prefix-fn token (math-get-table identifier math-prefix-fn-table))
    (math-token-set-infix-left-bp token (math-get-table identifier math-infix-left-bp-table))
    (math-token-set-infix-fn token (math-get-table identifier math-infix-fn-table))
    (setq math-cur-tok token)))

(defun math-parser-peek-infix-left-bp ()
  "The left binding power of the next token to be read."
  (let ((pair (math-peek-token)))
    (math-get-table (math-token-id (math-token-make-instance pair)) math-infix-left-bp-table)))
  
(defun math-parse-expression (right-bp)
  "Parse an expression."
  ;; Get the first token of the expression.
  (math-parser-advance-token)

  ;; Get the prefix function for parsing the current token.
  (let ((prefix (math-token-prefix-fn math-cur-tok)))
    (unless prefix 
      (error "No prefix function for token %s" math-cur-tok))

    ;; Apply the prefix function to get the parsed left sub-expression.
    (let ((subexpr (funcall prefix math-cur-tok)))

      ;; As long as the next token's binding power is higher than the
      ;; current right-bp, keep processing infix expressions.
      (while (< right-bp (math-parser-peek-infix-left-bp))

	  ;; Get the next token.
	  (math-parser-advance-token)

	  ;; Gett the infix function for parsing the current token.
	  (let ((infix (math-token-infix-fn math-cur-tok)))
	    (unless infix
	      (error "No infix function for token %s" math-cur-tok))
	    
	    (setq subexpr (funcall infix subexpr math-cur-tok))))
      subexpr)))
	    

(defun math-parse-buffer ()
  (save-excursion
    (goto-char (point-min))
    (math-parse-expression 0)))

(math-register-literal math-token-number)
(math-register-literal math-token-string)

(math-register-symbol math-token-eof)

;;(math-parser-name math-token-name)

(math-register-prefix "+" 70)
(math-register-prefix "-" 70)

(math-register-infix-left "+" 20)
(math-register-infix-left "-" 20)
(math-register-infix-left "*" 30)
(math-register-infix-left "/" 30)
(math-register-infix-right "^" 40)

(math-register-symbol-prefix "(" 100 (lambda (left-expression token) (math-parse-expression 0)))

(defun math-parse-buffer-command ()
  (interactive)
  (message "math-parse: %s" (math-parse-buffer)))

(provide 'math-parse)
