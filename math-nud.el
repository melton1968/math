
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
  `(,(math-token-id token) ,(math-parse-expression (math-token-prefix-left-bp token))))

;; Parse `literal' --> value
;; token: literal
(defun math-parse-nud-literal (token)
  (math-token-src token))

;; Parse `(expression)' --> expression
;; token: `('
(defun math-parse-nud-paren (token)
  (let ((expression (math-parse-expression 0)))
    (math-parser-expect-closer ")")
    expression))

;; Parse eof --> nil
;; token: eof
(defun math-parse-nud-eof (token)
  nil)

(provide 'math-nud)
