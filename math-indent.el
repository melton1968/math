
(require 'math-parse)

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

(provide 'math-indent)

