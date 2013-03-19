
(require 'math-token)

;; `nud' parse methods.
;;
;; A symbol's `nud' parse method is applied if it is the first token
;; read by parse-expression. This should only occur when the symbol is
;; the first symbol in a production for expression.
;;
;; The `nud' method is passed the current token and returns an
;; expression.

;; Parse `operator expression' --> (operator expression)
;; token: operator
(defun math-parse-nud-prefix (token)
  `(,(math-token-id token) ,(math-parse-expression (math-token-nud-left-bp token))))

;; Parse `literal' --> value
;; token: literal
(defun math-parse-nud-literal (token)
  (math-token-source token))

;; Parse `(expression)' --> expression
;; token: `('
(defun math-parse-nud-paren (token)
  (let ((expression (math-parse-expression 0)))
    (math-parse-expect-closer ")")
    expression))

;; Parse eof --> nil
;; token: eof
(defun math-parse-nud-eof (token)
  nil)

;; Parse eol --> skip the eol token and try to parse an expression
;; again.
;; token: eol
;;;;;;;;;;;;;;; TODO: is a binding power of 0 correct here?
(defun math-parse-nud-eol (token)
  (math-parse-expression 0))

(provide 'math-nud)
