
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


(defconst math-operator-re
  (regexp-opt '("=" ":=" "," ";"))
  "The regular expression for matching any Mathmatica operator.")

(defun math-union-re (regexps)
  "Create a regular expression that is the union of the regular
  expressions in the given list."
  (mapconcat 'copy-sequence regexps " \\| "))

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

(defconst math-system-id-re
  "[A-Z][a-zA-Z]*"
  "The regular expression for a Mathmatica system identifier.")

(defconst math-user-id-re
  "[a-zA-Z0-9]+"
  "The regular expression for a Mathmatica system identifier.")

(defun math-next-token-command ()
  (interactive)
  (let ((token (math-forward-token)))
    (if (and token (> (length token) 0)) (message "%s" token))))
(define-key math-mode-map "\C-c\C-f" 'math-forward-token-command)

(defun math-next-token (forward)
  ;; If case-fold search is true, then upper and lower cases are not
  ;; distinguishable in regular expressions. We set it to false
  ;; locally so that upper and lower cases can be distinguished. This
  ;; does not have any impact outside of this function.
  (let ((case-fold-search nil)) 
    (forward-comment (point-max))
    (cond
     ;; If point is at the start of a number, then return "number".
     ((looking-at math-number-re)
      (goto-char (match-end 0))
      "number")
     
     ;; If point is at the start of a string, then return "string".
     ((looking-at "\"")
      (forward-sexp)
      "string")
     
     ;; If point is at the start of a operator, then return the operator.
     ((looking-at math-operator-re)
      (goto-char (match-end 0))
      (match-string-no-properties 0))
     
     ;; If point is at the start of an system identifier, then return "sysid"
     ((looking-at math-system-id-re)
      (goto-char (match-end 0))
      "sysid")
     
     ;; If point is at the start of a user identifier, then return "userid"
     ((looking-at math-user-id-re)
      (goto-char (match-end 0))
      "userid")
     
     ;; Otherwise, we are at some unkown text or a paren so return nil
     (t nil))))

(provide 'math-indent)

