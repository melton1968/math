
(require 'math-util)

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
  (let* ((name (math-token-nud-name token))
	 (head (if name name (math-token-id token)))
	 (expr (math-parse-expression (math-token-nud-bp token))))
    `(,head ,expr)))

;; Parse `literal' --> value
;; token: literal
(defun math-parse-nud-literal (token)
  (let* ((name (math-token-nud-name token))
	 (head (if name name (math-token-source token))))
    head))

;; Parse `(expression)' --> expression
;; token: `('
(defun math-parse-nud-paren (token)
  (let ((expr (math-parse-expression 0)))
    (math-parse-expect-closer ")")
    expr))

;; Parse `{expr1,expr2,...}' --> (:List expr1 expr2 ...).
;; token: `{'
(defun math-parse-nud-sequence (token)
  (let ((sequence `(,'List)))
    (while (not (equal (math-token-id math--next-tok) "}"))
      (math-append-to-list sequence (math-parse-expression 0))
      (if (equal (math-token-id math--next-tok) ",")
	  (math-parse-expect-separator ",")))
    (math-parse-expect-closer "}")
    sequence))

(provide 'math-nud)
