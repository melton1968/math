
(require 'math-token)

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
(defun math-parse-led-left (left-expression token)
  `(,(math-token-id token)
    ,left-expression
    ,(math-parse-expression (math-token-led-left-bp token))))

;; Parse `expr1 operator expr2' --> (operator expr1 expr2)
;; token: operator (right associative)
(defun math-parse-led-right (left-expression token)
  `(,(math-token-id token)
    ,left-expression
    ,(math-parse-expression (- (math-token-led-left-bp token) 1))))

;; Parse `name[expr1,expr2,...]' --> (name expr1 expr2 ...).
;; token: `['
;; requirement: left expression should be a name.
(defun math-parse-led-sequence (left-expression token)
  (let ((sequence `(,left-expression)))
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
(defun math-parse-led-flat (left-expression token)
  (let ((right-expression (math-parse-expression (math-token-led-left-bp token))))
    (let ((expressions `(,left-expression ,right-expression)))
      (while (equal (math-parse-peek-led-id) (math-token-id token))
	(math-parse-advance-token)
	(math-append-to-list expressions (math-parse-expression (math-token-led-left-bp token))))
      (cons (math-token-id token) expressions))))

(provide 'math-led)
