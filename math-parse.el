
(require 'math-token)

(defconst math-parser-symbol-table (make-hash-table :test 'equal)
  "The parser symbol table.

Each entry in the symbol table corresponds to a type of token
that can be returned from the tokenizer.
")

(defconst math-parser-next-token-function nil
  "The parser calls this function to get a token.")

(defconst math-parser-token nil
  "The current parser token.")

(defun math-symbol (id &optional bp nud led)
  (let ((symbol (gethash id math-symbol-table)))
    (unless symbol
      (puthash id (list id bp nud led) math-symbol-table))
    symbol))

(defun math-symbol-infix (id bp &optional led)
  (math-symbol id bp (if led led
		       (lambda (token left-expr)
			 (list (token left-expr (expression bp)))))))

(defun math-expression (right-bp)
  (let ((last-token token))
    (let ((token (math-next-token)))
      (let ((left (funcall (nth 2 (math-symbol last-token)))))
	(while (< right-bp (nth 1 (math-symbol token)))
	  (setq last-token token)
	  (setq token (math-next-token))
	  (setq left (funcall (nth 3 (math-symbol last-token)) left)))
	left))))

(defun math-next-token ()

(math-symbol "number")
(math-symbol "string")
(math-symbol-infix "+" 20)
(math-symbol-infix "-" 20)
(math-symbol-infix "*" 30)
(math-symbol-infix "/" 30)
(math-symbol-infix "^" 40)

(math-symbol "+")
(math-symbol "-")




(defun expression (&optional right-precedence)
  (let ((next-token (math-forward-token))
	(left = (nud token)))
    (while (< right-precedence (left-precedence token))
      token = next-token
      (setq next-token (math-forward-token))
      (setq left (led token left)))))
