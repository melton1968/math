
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
	 (expr (math-p--parse-expression (math-token-nud-bp token))))
    `(,head ,expr)))

;; Parse `_' --> (_)
;; Parse `_expr1' --> (_ expr1)
;; Token: `_'
;;;;;;;; TODO - This doesn't handle the first case.
(defun math-parse-nud-blank (token)
  (let* ((name (math-token-nud-name token))
	 (head (if name name (math-token-id token)))
	 (expr (math-p--parse-expression (math-token-nud-bp token))))
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
  (let ((expr (math-p--parse-expression 0)))
    (math-append-to-list expr (math-p--closers ")"))))

;; Parse `{expr1,expr2,...}' --> (:List expr1 expr2 ...).
;; token: `{'
(defun math-parse-nud-sequence (token)
  (let ((sequence `(,'List)))
    (while (math-p--continue-until "}")
      (math-append-to-list sequence (math-p--parse-expression 0))
      (math-append-to-list sequence (math-p--separators-or-closers '("," ";") "}")))
    (math-append-to-list sequence (math-p--closers "}"))))

(provide 'math-nud)
