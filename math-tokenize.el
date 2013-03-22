
(require 'math-token-defs)
(require 'math-util)

(defun math-tok-goto-start-of-string (&optional backward)
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

(defun math-tok-goto-start-of-comment (&optional backward)
  "If point is inside a comment, move point to the beginning of the comment. 
If backward is not nil, move to the end of the comment."

  ;; `(nth 4 (syntax-ppss)' is nil unless point is within a comment.
  ;; `(nth 8 (syntax-ppss)' points to the start of the comment.

  ;; When point is within a comment.
  (when (nth 4 (syntax-ppss))
    ;; Move to the start of the comment.
    (goto-char (nth 8 (syntax-ppss)))
    ;; If backward is not nil, move to the other end of the comment.
    (if backward
      (forward-comment 1))))

(defun math-tok--token (backward)
  "Consume and return the next lexical token in the buffer
starting at `point'. If backward is not nil, the previous lexical
token."

  ;; If case-fold search is true, then upper and lower cases are not
  ;; distinguishable in regular expressions. We set it to false
  ;; locally so that upper and lower cases can be distinguished. This
  ;; does not have any impact outside of this function.
  (let ((case-fold-search nil))

    ;; If point is within a string, move to the beginning of the string.
    (math-tok-goto-start-of-string backward)

    ;; If point is within a comment, move to the beginning of the
    ;; comment.
    (math-tok-goto-start-of-comment backward)

    ;; Move past any white space.
    (forward-comment (if backward (-(point-max)) (point-max)))
    
    ;; The point is just before a token.
    (let ((looking (if backward (lambda (re) (looking-back re (point-min) t)) 'looking-at ))
	  (match (if backward 'match-beginning 'match-end)))

      (cond 
       ;; If we are at the end of the buffer, return the end token.
       ((= (point) (point-max)) 
	(math-token-make-instance :eof :eof))
       
       ;; If we are looking at a newline
       ((= (char-after (point)) ?\n)
	(forward-char 1)
	;; If we are at top-level, then return the eol token.
	;; Otherwise, skip the eol and return the next token.
	(if (= (nth 0 (syntax-ppss)) 0) 
	    (math-token-make-instance :eol :eol)
	  (math-tok--token backward)))

       ;; Identifiers
       ((funcall looking math-tok-identifier-re)
	(goto-char (funcall match 0))
	(math-token-make-instance :identifier (match-string-no-properties 0)))

       ;; Symbols
       ((funcall looking math-tok-symbol-re)
	(goto-char (funcall match 0))
	(math-token-make-instance :operator (match-string-no-properties 0)))
       
       ;; Strings
       ((funcall looking math-tok-string-re)
	(goto-char (funcall match 0))
	(math-token-make-instance :string (match-string-no-properties 0)))

       ;; Numbers
       ((funcall looking math-tok-number-re)
	(goto-char (funcall match 0))
	(math-token-make-instance :number (match-string-no-properties 0)))

       ;; Slots
       ((funcall looking math-tok-slot-re)
	(goto-char (funcall match 0))
	(math-token-make-instance :slot (match-string-no-properties 0)))

       ;; Out
       ((funcall looking math-tok-out-re)
	(goto-char (funcall match 0))
	(math-token-make-instance :out (match-string-no-properties 0)))

       ;; Operators
       ((funcall looking math-tok-operator-re)
	(goto-char (funcall match 0))
	(math-token-make-instance :operator (match-string-no-properties 0)))

	;; Otherwise, we did not recognize the token. Move forward one
	;; character so we do not get stuck and then return the
	;; unrecognized token.
       (t 
	(forward-char 1)
	(math-token-make-instance :unknown (string (char-before (point)))))))))

(defun math-tok-next-token ()
  (math-tok--token nil))

(defun math-tok-prev-token ()
  (math-tok--token t))

(defun math-tok-peek-next-token ()
  (save-excursion
    (math-tok-next-token)))

(defun math-tok-peek-prev-token ()
  (save-excursion
    (math-tok-prev-token)))

(defun math-tok-print-next-token ()
  (interactive)
  (let ((token (math-tok-next-token)))
    (message "%s" token)))

(defun math-tok-print-prev-token ()
  (interactive)
  (let ((token (math-tok-prev-token)))
    (message "%s" token)))

(defun math-tok-tokenize-region (begin end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (math-tok-tokenize-buffer))))

(defun math-tok-tokenize-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (with-output-to-temp-buffer "*math-tokenize-output*"
      (let ((last-line (math-token-line (math-tok-peek-next-token))))
	(while (< (point) (point-max))
	  (let* ((token (math-tok-next-token))
		 (line (math-token-line token)))
	    (if (> line last-line) (terpri))
	    (princ (format "'%s' " (math-token-source token)))
	    (setq last-line line)))))))
    
(provide 'math-tokenize)
