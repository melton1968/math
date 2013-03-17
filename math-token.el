
(require 'math-defs)

(defun math-start-of-string (&optional backward)
  "If point is inside of a string, move point to the start of the string.
 If backward is not nil, move to the end of the string."

  ;; `(nth 3 (syntax-ppss))' is nil unless point is within a string.
  ;; `(nth 8 (syntax-ppss))' points to the start of the string.

  ;; When point is within a string.
  (when (nth 3 (syntax-ppss))
    ;; Move to the start of the string.
    (goto-char (nth 8 (syntax-ppss)))
    ;; If backward is not nil, move to the other end of the string.
    (if backward
      (forward-sexp))))

(defun math-start-of-comment (&optional backward)
  "If point is inside a comment, move point to the end of the comment. 
If backward is not nil, move to the beginning of the comment."

  ;; `(nth 4 (syntax-ppss)' is nil unless point is within a comment.
  ;; `(nth 8 (syntax-ppss)' points to the start of the comment.

  ;; When point is within a comment.
  (when (nth 4 (syntax-ppss))
    ;; Move to the start of the comment.
    (goto-char (nth 8 (syntax-ppss)))
    ;; If backward is not nil, move to the other end of the comment.
    (if backward
      (forward-comment 1))))

(defun math-next-token (&optional backward)
  "The next lexical token in the buffer starting at `point'. If backward is not nil, 
the previous lexical token.

The returned token is of the form.

`((:identifier . token-id) (:value token-value))' 

where token-id is one of:

`math-token-number'   - a literal number value.
`math-token-string'   - a literal string value.
`math-token-name'     - a program name (variable or function).
operator-string       - a string representation of an operator.
`math-token-unknown'  - an unknown token.

and token-value is the verbatim text from which the token was derived."

  ;; If case-fold search is true, then upper and lower cases are not
  ;; distinguishable in regular expressions. We set it to false
  ;; locally so that upper and lower cases can be distinguished. This
  ;; does not have any impact outside of this function.
  (let ((case-fold-search nil))

    ;; If point is within a string, move to the beginning of the string.
    (math-start-of-string backward)

    ;; Move past any white space.
    (forward-comment (if backward (-(point-max)) (point-max)))
    
    ;; The point is just before a token. Match the token to one of
    ;; the token classes described in the math-token-re-alist.
    (catch 're-match
      (let ((looking (if backward (lambda (re) (looking-back re (point-min) t)) 'looking-at ))
	    (match (if backward 'match-beginning 'match-end)))
	(dolist (pair math-token-re-alist)
	  (if (funcall looking (cdr pair))
	      (progn
		(goto-char (funcall match 0))
		(throw 're-match 
		       (let ((string (match-string-no-properties 0))
			     (use-string-as-id (equal :operator (car pair))))
			 `((:identifier . ,(if use-string-as-id string (car pair)))
			   (:value . ,string))))))))

      ;; If we are at the end of the buffer, return the end token.
      (if (= (point) (point-max))
	  `((:identifier . ,math-token-eof)
	    (:value      . ,math-token-eof))
	;; Otherwise, we did not recognize the token. Move forward one
	;; character so we do not get stuck and then return the
	;; unrecognized token.
	(forward-char 1)
	`((:identifier . ,math-token-unknown)
	  (:value     . ,(string (char-before (point)))))))))

(defun math-peek-token (&optional backward)
  "Same as math-next-token except the token is not consumed."
  (save-excursion
    (math-next-token backward)))

(defun math-next-token-command ()
  (interactive)
  (let ((pair (math-next-token nil)))
    (message "%s" pair)))

(defun math-prev-token-command ()
  (interactive)
  (let ((pair (math-next-token t)))
    (message "%s" pair)))

(defun math-tokenize-buffer (buffer-name)
  (interactive "BOutput buffer: ")
  (save-excursion
    (let ((buffer (get-buffer-create buffer-name)))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(pp (math-next-token nil) buffer)
	(terpri buffer))))
  (switch-to-buffer buffer-name)
  (goto-char (point-min)))
    
(provide 'math-token)
