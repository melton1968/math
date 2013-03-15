(defun math-union-re (regexps)
  "Create a regular expression that is the union of the regular
  expressions in the given list."
  (mapconcat 'copy-sequence regexps " \\| "))

(defconst math-user-id-re
  "[a-zA-Z0-9]+"
  "The regular expression for a Mathmatica system identifier.")

(defconst math-system-id-re
  "[A-Z][a-zA-Z]*"
  "The regular expression for a Mathmatica system identifier.")

(defconst math-operator-re
  (regexp-opt '("=" ":=" "," ";"))
  "The regular expression for matching any Mathmatica operator.")

(defconst math-number-re
  ;; Taken from http://reference.wolfram.com/mathmatica/tutorial/InputSyntax.html#7977
  ;;     digits                - integer
  ;;     digits.digits         - approximate number
  ;;     base^^digits          - integer in base
  ;;     base^^digits.digits   - approximate number in base
  ;;     mantissa*^n           - scientific notation
  ;;     base^^mantissa*^n     - scientific notation in base
  ;;     number`               - number in machine precision
  ;;     number`s              - number in precision s
  ;;     number``s             - number in precision s
  ;;
  (math-union-re '("[+-]?[0-9]*\\.?[0-9]+"))
  "The regular expression for a Mathmatica number.")

(defun math-forward-string (&optional backward)
  "Move point to the end of the string. If backward is not nil,
move to the beginning of he string.

Assumes point is within a string.
"
  ;; Move to the beginning of the string.
  (goto-char (nth 7 (syntax-ppss)))

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
`:userid', `:sysid', `:operator', `:open', `:close'.
"

  ;; If case-fold search is true, then upper and lower cases are not
  ;; distinguishable in regular expressions. We set it to false
  ;; locally so that upper and lower cases can be distinguished. This
  ;; does not have any impact outside of this function.
  (let ((case-fold-search nil)) 
    (let ((ppss (syntax-ppss)))
      (cond 
       
       ;; Point is withing a string. If we are moving forward, back up
       ;; to the beginning of the string so we can return it as a
       ;; token and recurse. Otherwise, move to the end of the string 
       ;; and so we can return it as a token and recurse.
       ((nth 3 ppss) 
	(math-forward-string (> count 0))
	(math-forward-token count))
       
       ;; Point is within a comment. If we are moving forward, move to
       ;; the end of the comment and recurse.  Otherwise, move to the
       ;; beginning of the comment and recurse.
       ((nth 4 ppss) 
	(math-forward-comment (< count 0))
	(math-forwaard-token count))

       ;; Point is not within a string or comment.
       (t 
	;; Move past any white space.
	(forward-comment (if (> count 0) 1 -1))
	
	;; The point is just before a token.
	(cond

	 ;; If point is at the start of a number, then return "number".
	 ((looking-at math-number-re)
	  (goto-char (match-end 0))
	  (cons :number (match-string-no-properties 0)))
	 
	 ;; If point is at the start of a string, then return "string".
	 ((looking-at "\"")
	  (forward-sexp)
	  (cons :string "string"))
	 
	 ;; If point is at the start of a operator, then return the operator.
	 ((looking-at math-operator-re)
	  (goto-char (match-end 0))
	  (cons :operator (match-string-no-properties 0)))
	 
	 ;; If point is at the start of an system identifier, then return "sysid"
	 ((looking-at math-system-id-re)
	  (goto-char (match-end 0))
	  (cons :sysid (match-string-no-properties 0)))
	 
	 ;; If point is at the start of a user identifier, then return "userid"
	 ((looking-at math-user-id-re)
	  (goto-char (match-end 0))
	  (cons :userid (match-string-no-properties 0)))
	 
	 ;; Otherwise, we are at some unkown text or a paren so return nil
	 (t (cons :unknown "?"))))))))


(defun math-context ()
  "The context at point: `:comment-context', `:string-context', `:code-context'"
  (let ((ppss (syntax-ppss)))
    (cond ((nth 3 ppss) :string-context)
	  ((nth 4 ppss) :comment-context)
	  (t :code-context))))

(defun math-indent-line ()
  (interactive)
  (save-excursion
    (forward-line 0)
    (let ((context (math-context)))
      (cond ((eq context :comment-context) (message "comment"))
	    ((eq context :string-context) (message "string"))
	    ((eq context :code-context) (message "code"))))))

(defun scan-region ()
  (save-excursion
    (with-output-to-temp-buffer "*tokens"
      (let ((start (region-beginning))
	    (end (region-end)))
	(goto-char start)
	(set-mark end)
	(if (> (point) (mark))
	    (exchange-point-and-mark))
	(while (< (point) (mark))
	  (setq token (math-forward-token))
	  (if (and token (> (length token) 0))
	      (princ (format "<%s> " token))
	    (let ((start (point)))
	      (forward-char)
	      (princ (format " %s " (buffer-substring-no-properties start (point)))))))))))


(defun math-forward-token-command ()
  (interactive)
  (let ((pair (math-forward-token 1)))
    (message "%s" pair)))

(define-key math-mode-map "\C-c\C-f" 'math-forward-token-command)
(define-key math-mode-map "\C-c\C-r" 'boo)

(provide 'math-indent)

