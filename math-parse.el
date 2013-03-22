

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
;; catergories: null denotation, left expression denotation and
;; `closer'. In much of the literature, the denotation terms are
;; abbreviated as `nud' and `led' respectively.
;;
;; The prefix '+' and the matchfix '(' are considered null denominator
;; operators because there is no 'left hand side' for the operator in
;; that context.
;;
;; The postfix '++' and infix '+' are considered left expression
;; denotation operators because there is a 'left hand side' to which
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
;; than the terms null denonation and left expression denotation.
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


(require 'math-parse-defs)

(defconst math-p--tok nil
  "The current parser token.")

(defconst math-p--next-tok nil
  "The next parser token.")

(defun math-p--error (msg token)
  "Dispatch an error for token with the given message."
  (error "%s:%d: error: %s" 
	 (math-token-file token)
	 (math-token-line token)
	 msg))

;; The core parsing methods.
;;
(defun math-p--init ()
  (setq math-p--tok nil)
  (setq math-p--next-tok nil)
  (math-p--advance-token))

(defun math-p--advance-token ()
  "Get the next token and set its properties based on the parser tables."
  (setq math-p--tok math-p--next-tok)
  (setq math-p--next-tok (math-tok-next-token)))

(defun math-p--peek-led-bp ()
  "The left binding power of the next token."
  (let ((bp (math-token-led-bp math-p--next-tok)))
    (if bp bp (math-p--error 
	       (format "No left binding power for operator `%s'." id) 
	       math-p--next-tok))))

(defun math-p--peek-led-id ()
  "The id of next token to be read."
  (math-token-id math-p--next-tok))

(defun math-p--expect-closer (closer)
  (math-p--advance-token)
  (let ((id (math-token-id math-p--tok)))
    (unless (equal id closer)
      (math-p--error 
       (format "Exepcted matching %s but read %s instead" closer id) 
       math-p--tok))))

(defun math-p--expect-separator (closer)
  (math-p--advance-token)
  (let ((id (math-token-id math-p--tok)))
    (unless (equal id closer)
      (math-p--error (format "Exepcted %s but read %s instead" closer id) math-p--tok))))
  
(defun math-p--consume-blank-lines ()
  (while (equal (math-token-class math-p--next-tok) :eol)
    (math-p--advance-token)))

(defun math-p--parse-expression (right-bp)
  "Parse an expression."
  ;; Incomplete Mathematica expressions can be continued on the next
  ;; line. Since we are trying to parse an expression, skip over the
  ;; eol markers.
  ;;(math-p--consume-blank-lines)

  ;; Get the first token of the expression.
  (math-p--advance-token)

  (while (equal (math-token-class math-p--tok) :eol)
    (math-p--advance-token))

  ;; If we see an eof marker here, then this expression is incomplete.
  (if (equal (math-token-class math-p--tok) :eof)
      (math-p--error "Incomplete expression" token))

  ;; Get the nud function for parsing the current token.
  (let ((nud (math-token-nud-fn math-p--tok)))
    (unless nud 
      (math-p--error 
       (format "No nud function for `%s'" (math-token-source math-p--tok)) 
       math-p--tok))

    ;; Apply the nud function to get the parsed left sub-expression.
    (let ((subexpr (funcall nud math-p--tok)))

      ;; As long as the next token's binding power is higher than the
      ;; current right-bp, keep processing led expressions.
      (while (< right-bp (math-p--peek-led-bp))

	  ;; Get the next token.
	  (math-p--advance-token)

	  ;; Gett the led function for parsing the current token.
	  (let ((led (math-token-led-fn math-p--tok)))
	    (unless led
	      (math-p--error
	       (format "No led function for `%s'" (math-token-source math-p--tok))
	       math-p--tok))
	    
	    (setq subexpr (funcall led subexpr math-p--tok))))
      subexpr)))

(defun math-p--parse-statement ()
  "Parse a Mathematica statement."

  ;; Discard blank lines.
  (while (equal (math-token-class math-p--next-tok) :eol)
    (math-p--advance-token))

  ;; Read expressions until we find an eol or eof terminated expression.
  (let ((expressions (list 'expressions)))
    (while (and (not (equal (math-token-class math-p--next-tok) :eol))
		(not (equal (math-token-class math-p--next-tok) :eof)))
      (math-append-to-list expressions (math-p--parse-expression 0))
      ;; The expression must be terminated by one of: `;' `eol' `eof'.
      (let ((id (math-token-id math-p--next-tok)))
	(cond
	 ;; Consume the `;'
	 ((equal id ";") 
	  (math-p--advance-token))
	 ;; The eol marker will cause the while loop to terminate.
	 ((equal id :eol)
	  t)
	 ;; The eof marker will cause the while loop to terminate.
	 ((equal id :eof)
	  t)
	 ;; Otherwise, there is a syntax error.
	 (t
	  (math-p--error 
	   (format "Expected a `;' but read `%s' instead." id)
	   math-p--next-tok)))))

    ;; Discard blank lines.
    (while (equal (math-token-class math-p--next-tok) :eol)
      (math-p--advance-token))

    expressions))

(defun math-p--parse-program ()
  "Parse a Mathematica program."
  
  (math-p--init)
  ;; Read statements until we see the `eof' token.
  (let ((statements (list 'statements)))
    (while (not (equal (math-token-class math-p--next-tok) :eof))
      (math-append-to-list statements (math-p--parse-statement)))
    statements))

(defun math-parse-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (with-output-to-temp-buffer "*math-p--output*"
      (pp (math-p--parse-program)))))

(defun math-parse-region (begin end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (let ((input (buffer-string)))
	(with-output-to-temp-buffer "*math-p--output*"
	  (princ "Input:\n\n")
	  (princ input)
	  (princ "\n\n")
	  (princ "Parse:\n\n")
	  (pp (math-p--parse-program)))))))

(provide 'math-parse)
