
(require 'math-parse)
(require 'math-comment)

(defun math-ind-context ()
  "The context at point: `:comment-context', `:string-context', `:code-context'"
  (let ((ppss (syntax-ppss)))
    (cond ((nth 3 ppss) :string-context)
	  ((nth 4 ppss) :comment-context)
	  (t :code-context))))

(defun math-indent-line ()
  (interactive)
  (let ((point 
	 (save-excursion
	   (forward-line 0)
	   (let ((context (math-ind-context)))
	     (cond ((eq context :comment-context) (math-comment-indent))
		   ((eq context :string-context) (message "string"))
		   ((eq context :code-context) (message "code")))))))
    (if (numberp point)
	(goto-char point))))

(defun mant-indent-fill ()
  (interactive)
  (save-excursion
    (let ((context (math-ind-context)))
      (cond ((eq context :comment-context) (math-ind-fill-comment))
	    (t nil)))))

(provide 'math-indent)

