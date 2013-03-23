

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
(require 'math-tokenize)

;; Parser Token
;;
;; `math-p--tok' is the token currently being parsed. It should be
;; treated as read-only. It updated by math-p--advance-token.
(defconst math-p--tok nil)

;; The only function that changes `math-p--tok'.
(defun math-p--advance-token ()
  "Get the next token."
  (setq math-p--tok (math-tokenize-next)))

;; Parser State
;;
;; `math-p--parsing' is the state of the parser: t means parsing is in
;; progress, nil means parsing has stopped.
(defconst math-p--parsing nil)

;; Initialization
;;
;; The parser must be initialized before each attempt to parse a
;; region or buffer. The initialization queues up the first token and
;; sets the parser state to 'go.
(defun math-p--init ()
  (math-p--advance-token)
  (setq math-p--parsing t))

;; Errors
;;
;; Return an error message of the form '(Error "message") with the
;; appropriate line number of other information and set the parser
;; state to stopped. `token' caused an error described by `msg'.
(defun math-p--error (token msg)
  (setq math-p--parsing nil)
  `(Error ,msg))

;; Append the error returned by `math-p--error' to the given `expr'.
(defun math-p--error-append (expr token msg)
  (math-append-to-list expr (math-p--error token msg)))

;; Parsing Helpers
;;

;; Consume blank lines.
(defun math-p--consume-blank-lines ()
  (while (equal (math-token-class math-p--tok) :eol)
    (math-p--advance-token)))

;; Return true if id matches an element of list or is equal to symbol.
(defun math-p--match (id list-or-symbol)
  (cond
   ((and (listp list-or-symbol) (member id list-or-symbol)) t)
   ((and (not (listp list-or-symbol)) (equal id list-or-symbol)) t)
   (t nil)))

;; Return true if we are parsing the current token does not match
;; any of the `closers' (a string or list of strings).
(defun math-p--continue-until (closers)
  (let ((id (math-token-id math-p--tok)))
    (and math-p--parsing (not (math-p--match id closers)))))

;; Expecting a closing token.
;;
;; 1) If we are not parsing, return nil
;; 2) If the current token matches `closers', consume the token and return nil.
;; 3) Otherwise, return the error list.
;; 
(defun math-p--closers (closers)
  (let ((id (math-token-id math-p--tok)))
    (cond
     ((not math-p--parsing) nil)
     ((math-p--match id closers) (math-p--advance-token) nil)
     (t (math-p--error math-p--tok (format "expected `%s' but read `%s' instead" closers id))))))

;; Expecting either a separator or a closing token.
;;
;; 1) If we are not parsing, return nil.
;; 2) If the current token matches `separators', consume the token and return nil.
;; 3) If the current token matches `closers', return nil.
;; 4) Otherwise, return the errro list.
(defun math-p--separators-or-closers (separators closers)
  (let ((id (math-token-id math-p--tok)))
    (cond
     ((not math-p--parsing) nil)
     ((math-p--match id separators) (math-p--advance-token) nil)
     ((math-p--match id closers) nil)
     (t 
      (math-p--error 
       math-p--tok 
       (format "expected one of %s or a %s but read %s" separators closers id))))))

;; Core Parsing Functions
;;
(defun math-p--parse-expression (right-bp)
  (math-p--consume-blank-lines)

  ;; Since this is the first token in the current expression, we need
  ;; to get its nud parsing function.
  (let* ((token math-p--tok)
	 (nud (math-token-nud-fn token)))
      
    ;; If the current token does not have a nud function, then we
    ;; cannot parse an expression.
    (if (null nud)
	(math-p--error token (format "expected expression but read '%s'." (math-token-id token)))
	
      ;; Apply the nud function to get the parsed expression.
      (math-p--advance-token)
      (let ((expression (funcall nud token)))
	(math-p--parse-expression-led expression right-bp)))))

(defun math-p--parse-expression-led (left-expr right-bp)

  (let ((left-bp (math-token-led-bp math-p--tok)))
    (if (null left-bp)
	(math-p--error 
	 math-p--tok 
	 (format "expected an operator but read '%s'." (math-token-id token)))

      (if (< right-bp left-bp)

	  ;; Get the led function for parsing the current token.
	  (let* ((token math-p--tok)
		 (led (math-token-led-fn token)))

	    (if (null led)
		(math-p-error token (format "no led function for '%s'."))

	      ;; Get the next token.
	      (math-p--advance-token)
      
	      ;; Apply the led function to get the parsed sub-expression.
	      (let ((expression (funcall led left-expr token)))
		(math-p--parse-expression-led expression right-bp))))

	left-expr))))

;; Parse a Mathematica statement."
(defun math-p--parse-statement ()
  ;; Discard blank lines.
  (math-p--consume-blank-lines)

  ;; Read expressions until we find an eol or eof terminated expression.
  (let ((expressions (list 'expressions)))
    (while (math-p--continue-until '(:eol :eof))
      (math-append-to-list expressions (math-p--parse-expression 0))
      (math-append-to-list expressions (math-p--separators-or-closers ";" '(:eol :eof))))
    (math-append-to-list expressions (math-p--closers '(:eol :eof)))))

;; Parse a Mathematica program."
(defun math-p--parse-program ()
  (math-p--init)
  ;; Read statements until we see the `eof' token.
  (let ((statements (list 'statements)))
    (while (math-p--continue-until :eof)
      (math-append-to-list statements (math-p--parse-statement))
      (math-p--consume-blank-lines))
    (math-append-to-list statements (math-p--closers :eof))))

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
