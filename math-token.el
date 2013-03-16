
(require 'math-defs)

(defun math-forward-string (&optional backward)
  "Move point to the end of the string. If backward is not nil,
move to the beginning of he string.

Assumes point is within a string.
"
  ;; Move to the beginning of the string.
  (goto-char (nth 8 (syntax-ppss)))

  ;; If we are not moving to the beginning of the string, move to the
  ;; end of the string.
  (unless backward
    (forward-sexp)))

(defun math-forward-comment (&optional backward)
  "Move point to the end of the comment. If backward is not nil,
move to the beginning of he comment.

Assumes point is within a comment.
"
  ;; Move to the beginning of the comment.
  (goto-char (nth 8 (syntax-ppss)))

  ;; If we are not moving to the beginning of the comment, move to the
  ;; end of the comment.
  (unless backward
    (forward-comment 1))
)

(defun math-forward-token (&optional count)
  "Move forward across COUNT lexical tokens. Argument -COUNT
means move backwards across COUNT lexical tokens.

Return an alist of the form:

`((:class . token-class) (:sub-class token-subclass) (:value token-value))' 

where token-class is one of:

`literal'  - a literal number or string value.
`name'     - a variable or function name.
`operator' - an operator.
`eof'      - the end of buffer / file.
`unknown'  - an unknown token.

and sub-class is one of:

`number' - a number `literal'.
`string' - a string `literal'.
`user'   - a user name (lower case `name').
`system' - a system name (upper case `name').
`group'  - a grouping `operator'.
`symbol' - a syntax character `operator'
`base'   - a base `operator'.
`eof'      - the end of buffer / file.
`unknown'  - an unknown token.

and token-value is the verbatim source text.

The point is assumed not to be within a comment or string.
"
  ;; If case-fold search is true, then upper and lower cases are not
  ;; distinguishable in regular expressions. We set it to false
  ;; locally so that upper and lower cases can be distinguished. This
  ;; does not have any impact outside of this function.
  (let ((case-fold-search nil)) 
    ;; Move past any white space.
    (forward-comment (if (> count 0) (point-max) (-(point-max))))
    
    ;; The point is just before a token. Match the token to one of
    ;; the token classes described in the math-token-re-alist.
    (catch 're-match
      (let ((looking (if (> count 0) 'looking-at (lambda (re) (looking-back re (point-min) t))))
	    (match (if (> count 0) 'match-end 'match-beginning)))
	(dolist (pair math-token-re-alist)
	  (if (funcall looking (cdr pair))
	      (progn
		(goto-char (funcall match 0))
		;; FIXME - Need to return an association list here.
		(throw 're-match 
		       (list (cons :class (caar pair))
			     (cons :sub-class (cadar pair))
			     (cons :value (match-string-no-properties 0))))))))

      ;; If we are at the end of the buffer, return the end token.
      (if (= (point) (point-max))
	  '((:class     . :eof)
	    (:sub-class . :eof)
	    (:value     . "(eof)"))
	;; Otherwise, we did not recognize the token. Move forward one
	;; character so we do not get stuck and then return the
	;; unrecognized token.
	(forward-char 1)
	`((:class     . :unknown)
	  (:sub-class . :unknown)
	  (:value     . ,(string (char-before (point)))))))))

(defun math-forward-token-command ()
  (interactive)
  (let ((pair (math-forward-token 1)))
    (message "%s" pair)))

(defun math-backward-token-command ()
  (interactive)
  (let ((pair (math-forward-token -1)))
    (message "%s" pair)))

(defun math-tokenize-buffer (buffer-name)
  (interactive "BOutput buffer: ")
  (save-excursion
    (let ((buffer (get-buffer-create buffer-name)))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(pp (math-forward-token 1) buffer)
	(terpri buffer))))
  (switch-to-buffer buffer-name)
  (goto-char (point-min)))
    
(provide 'math-token)
