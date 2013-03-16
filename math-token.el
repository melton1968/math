
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

Return the cons (token-class . value) where token-class is one
of: `:comment-start', `:comment-end', `:symbol', `:number', `:string',
`:user-ident', `:sys-ident', `:operator', `:open', `:close'.

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
		(throw 're-match (cons (car pair) (match-string-no-properties 0))))))
	(cons :unknown "?")))))

(defun math-forward-token-command ()
  (interactive)
  (let ((pair (math-forward-token 1)))
    (message "%s" pair)))

(defun math-backward-token-command ()
  (interactive)
  (let ((pair (math-forward-token -1)))
    (message "%s" pair)))

