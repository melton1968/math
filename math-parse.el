

;;;; 0. Parser ;;;;
;;
;; The parser uses a top-down operator precedence (TDOP) parsing
;; methodology. For background see the following:
;;
;; http://javascript.crockford.com/tdop/tdop.html
;; http://effbot.org/zone/simple-top-down-parsing.htm
;; http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
;; http://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing/
;;
;;;; 1. Terminology ;;;;
;;
;; Consider the following simple expression grammar.
;;
;; expr := number                 // a literal number
;; expr := '+' expr               // '+' is a prefix operator
;; expr := '(' expr ')'           // '(' and ')' are matchfix operators
;; expr := expr '++'              // '++' is a postfix operator
;; expr := expr '+' expr          // '+' is an infix operator
;;
;; TDOP divides the operators in the above operators into three
;; catergories: null denominator, left expression denominator and
;; `closer'. In much of the literature, the denominator terms are
;; abbreviated as `nud' and `led' respectively.
;;
;; The prefix '+' and the matchfix '(' are considered null denominator
;; operators because there is no 'left hand side' for the operator in
;; that context.
;;
;; The postfix '++' and infix '+' are considered left expression
;; denominator operators because there is a 'left hand side' to which
;; the operator applies.
;;
;; Even thought the matchfix ')' has an expression on the left, it is
;; considered a closer since it follows the last non-terminal in the production.
;;
;; Only the `nud' and `led' operators have associated parse function
;; that are displatched when they are read as tokens. The `closer'
;; operators are simply required to be read at the correct time but do
;; not have any associated parse function.
;;
;; Sometimes `ned' and `led' are refered to as prefix and infix, but that
;; is really a misnomer and can lead to confusion. The terms prefix,
;; infix and postfix describe a different dimension about an operator
;; than the terms null denominator and left expression denominator.
;;
;; The other important TDOP term is binding power which is related to
;; the precedence and associativity of an operator. Each operator has
;; both a left binding power and a right binding power. 
;;
;; The left binding power is how strongly the operator binds to the
;; preceeding term and the right binding power is how strongly the
;; operator binds to the succeeding term. The binding power is used to
;; control both operator precedence and associativity.
;; 
;;;; 2. Algorithm ;;;;
;;
;; The following pseudo-code describes the basic parsing
;; algorithm.
;;
;; parse-expression (right-binding-power = 0)
;;     Read `token'
;;     expression = Result of the `nud' parse method for `token' (nud token)
;;     while right-binding-power < left-binding-power(next token)
;;         Read `token'
;;         expression = Result of the `led' parse method for `token' (led expression token)
;;     return expression
;;
;; For each token read, either the `nud' or `led' parse method is
;; dispatched. Let's consider this in the context of the simple
;; grammar from above.
;;
;; The nud for number just returns the number.
;; The nud for the prefix '+' calls parse expression to get it's right expression.
;; The nud for '(' calls parse expression and then expects the closer ')'.
;; The led for postfix '++' simply applies its operation to the left expression.
;; The led for infix '+' calls parse expression to get its right expression.
;;


(require 'math-parse-util)
(require 'math-token)

(defconst math-cur-tok nil
  "The current parser token.")

;; The core parsing methods.
;;	
(defun math-parser-advance-token ()
  "Get the next token and set its properties based on the parser tables."
  (let* ((token (math-token-make-instance (math-next-token)))
	 (identifier (math-token-id token)))
    (math-token-set-nud-left-bp token (math-get-table identifier math-nud-left-bp-table))
    (math-token-set-nud-fn token (math-get-table identifier math-nud-fn-table))
    (math-token-set-led-left-bp token (math-get-table identifier math-led-left-bp-table))
    (math-token-set-led-fn token (math-get-table identifier math-led-fn-table))
    (setq math-cur-tok token)))

(defun math-parser-peek-led-left-bp ()
  "The left binding power of the next token to be read."
  (let* ((pair (math-peek-token))
	 (id (math-token-id (math-token-make-instance pair)))
	 (bp (math-get-table id math-led-left-bp-table)))
    (if bp bp (error "error: Unknown operator %s." id)))) 

(defun math-parser-peek-led-id ()
  "The id of next token to be read."
  (let ((pair (math-peek-token)))
    (math-token-id (math-token-make-instance pair))))

(defun math-parser-expect-closer (closer)
  (let ((token (math-next-token)))
    (unless (and (equal (car token) :operator) (equal (cdr token) closer))
      (error "Exepcted matching %s but read %s instead" closer (cdr token)))))

(defun math-parser-expect-separator (closer)
  (let ((token (math-next-token)))
    (unless (and (equal (car token) :operator) (equal (cdr token) closer))
      (error "Exepcted %s but read %s instead" closer (cdr token)))))
  
(defun math-parse-expression (right-bp)
  "Parse an expression."
  ;; Get the first token of the expression.
  (math-parser-advance-token)

  ;; Get the nud function for parsing the current token.
  (let ((nud (math-token-nud-fn math-cur-tok)))
    (unless nud 
      (error "No nud function for token %s" math-cur-tok))

    ;; Apply the nud function to get the parsed left sub-expression.
    (let ((subexpr (funcall nud math-cur-tok)))

      ;; As long as the next token's binding power is higher than the
      ;; current right-bp, keep processing led expressions.
      (while (< right-bp (math-parser-peek-led-left-bp))

	  ;; Get the next token.
	  (math-parser-advance-token)

	  ;; Gett the led function for parsing the current token.
	  (let ((led (math-token-led-fn math-cur-tok)))
	    (unless led
	      (error "No led function for token %s" math-cur-tok))
	    
	    (setq subexpr (funcall led subexpr math-cur-tok))))
      subexpr)))
	    

(defun math-parse-buffer ()
  (save-excursion
    (goto-char (point-min))
    (math-parse-expression 0)))

;; Operator Definitions.
;;

;; Literals
;;
(math-register-literal math-token-number)
(math-register-literal math-token-string)

;;(math-parser-name math-token-name)

;; name[expr1,expr2,...]
;;
(math-register-symbol-led "[" 745 'math-parse-led-sequence)

;; Unary mathematical operators
;;
(math-register-nud "+" 490)
(math-register-nud "-" 490)

;; Binary mathematical operators
;;
(math-register-led-flat "+" 330)
(math-register-led-flat "-" 330)
(math-register-led-left "*" 410)
(math-register-led-left "/" 480)
(math-register-led-right "^" 590)

;; Grouping operator.
;;
(math-register-symbol-nud "(" 100 'math-parse-open-paren)

;; expr>>filename      --> Put[expr,"filename"]
;; expr>>>filename     --> PutAppend[expr,"filename"]
(math-register-led-left ">>" 30)
(math-register-led-left ">>>" 30)

;; expr1;expr2;expr3   --> CompoundExpression[expr1,expr2,expr3]
;; expr1;expr2;        --> CompoundExpression[expr1,expr2,Null]
;;
;; TODO - add lookahead in tokenizer for ws to eol so that the eos
;; parser can check for the second version and add the Null marker.
(math-register-led-flat ";" 20)

;; expr1 \` expr2      --> FormBoxp[expr2,expr1]
(math-register-led-right "\\`" 10)

;; Separators and Closers need to be registered, but do not have any
;; associated precedence or parsing funtions.
(math-register-symbol math-token-eof)
(math-register-symbol ",")
(math-register-symbol "]")
(math-register-symbol ")")

(defun math-parse-buffer-command ()
  (interactive)
  (message "math-parse: %s" (math-parse-buffer)))

(provide 'math-parse)
