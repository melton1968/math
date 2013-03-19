
(require 'math-defs)
(require 'math-util)

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
  "Consume and return the next lexical token in the buffer
starting at `point'. If backward is not nil, the previous lexical
token."

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
		(throw 're-match (math-token-make-instance 
				  (car pair) 
				  (match-string-no-properties 0)))))))

      (cond 
       ;; If we are at the end of the buffer, return the end token.
       ((= (point) (point-max)) (math-token-make-instance math-token-eof math-token-eof))
       
       ;; If we are looking at a newline
       ((= (char-after (point)) ?\n)
	(forward-char 1)
	;; If we are at top-level, then return the eol token.
	;; Otherwise, skip the eol and return the next token.
	(if (= (nth 0 (syntax-ppss)) 0) 
	    (math-token-make-instance math-token-eol math-token-eol)
	  (math-next-token)))
	
	;; Otherwise, we did not recognize the token. Move forward one
	;; character so we do not get stuck and then return the
	;; unrecognized token.
       (t 
	(forward-char 1)
	(math-token-make-instance math-token-unknown (string (char-before (point)))))))))

(defun math-peek-token (&optional backward)
  "Same as math-next-token except the token is not consumed."
  (save-excursion
    (math-next-token backward)))

(defun math-next-token-command ()
  (interactive)
  (let ((token (math-next-token nil)))
    (message "%s" token)))

(defun math-prev-token-command ()
  (interactive)
  (let ((token (math-next-token t)))
    (message "%s" token)))

(defun math-tokenize-region (begin end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (math-tokenize-buffer))))

(defun math-tokenize-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (with-output-to-temp-buffer "*math-tokenize-output*"
      (let ((last-line 0))
	(while (< (point) (point-max))
	  (let* ((token (math-next-token nil))
		 (line (math-token-line token)))
	    (if (> line last-line)
		(terpri))
	    (setq last-line line)
	    (princ (math-token-src token))
	    (princ " ")))))))
    
(provide 'math-token)
