
(require 'math-util)

;; `led' parse methods.
;;
;; A symbol's `led' parse methhod is applied for all symbols read by
;; parse expression except the first symbol. This should only occur
;; when symbol is the second or later symbol in a production for
;; expression.
;;
;; The `led' method is passed the left expression and the current
;; token and returns an expressions.

;; Parse `expr1 operator expr2' --> (operator expr1 expr2)
;; token: operator (left associative)
(defun math-parse-led-left (l-expr token)
  (let* ((name (math-token-led-name token))
	 (head (if name name (math-token-source token)))
	 (r-expr (math-parse-expression (math-token-led-bp token))))
  `(,head
    ,l-expr
    ,r-expr)))

;; Parse `expr1 operator expr2' --> (operator expr1 expr2)
;; token: operator (right associative)
(defun math-parse-led-right (l-expr token)
  (let* ((name (math-token-led-name token))
	 (head (if name name (math-token-source token)))
	 (r-expr (math-parse-expression (- (math-token-led-bp token) 1))))
  `(,head
    ,l-expr
    ,r-expr)))

;; Parse `name[expr1,expr2,...]' --> (name expr1 expr2 ...).
;; token: `['
;; requirement: left expression should be a name.
(defun math-parse-led-sequence (l-expr token)
  (let ((sequence `(,l-expr)))
    (while (not (equal (math-parse-peek-led-id) "]"))
      (let ((expression (math-parse-expression 0)))
	(math-append-to-list sequence expression))
      (if (equal (math-parse-peek-led-id) ",")
	  (math-parse-expect-separator ",")))
    (math-parse-expect-closer "]")
    sequence))

;; Parse `expr1 operator expr2 ...' --> (operator expr1 expr2 ...)
;; token: operator
;;
;; This is used to parse flat operators, i.e. operators that have a
;; variable number of expressions.
(defun math-parse-led-flat (l-expr token)
  (let ((r-expr (math-parse-expression (math-token-led-bp token))))
    (let ((expressions `(,l-expr ,r-expr)))
      (while (equal (math-parse-peek-led-id) (math-token-id token))
	(math-parse-advance-token)
	(math-append-to-list expressions (math-parse-expression (math-token-led-bp token))))
      (cons (math-token-id token) expressions))))

;; Parse `expr1 operator' --> (operator expr1)
;; token: operator
;;
;; This is used to parse a postfix operator.
(defun math-parse-led-postfix (l-expr token)
  (let* ((name (math-token-led-name token))
	 (head (if name name (math-token-source token))))
  `(,head ,l-expr)))

;; Parse `symb /: expr2 = or := or =. expr3' --> (Tag
;; token: operator (right associative)
;; (defun math-parse-led-tag (symbol token)
;;   (let ((l-expr (math-parse-expression (- (math-token-led-bp token) 1))))
;;     (let ((set-tok-id (math-token-id math--next-tok)))
;;     (cond
;;      ((equal set-tok-id "=") 


;;   `(,(math-token-id token)
;;     ,l-expr
;;     ,(math-parse-expression (- (math-token-led-bp token) 1))))


(provide 'math-led)
