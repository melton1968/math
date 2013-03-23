
(require 'math-comment)
(require 'math-code)

;; Return the context at point: `comment', `string' or `code'.
(defun math-i--context ()
  (let ((ppss (syntax-ppss)))
    (cond ((nth 3 ppss) :string)
	  ((nth 4 ppss) :comment)
	  (t :code))))

(defun math-indent ()
  (interactive)
  (let ((point 
	 (save-excursion
	   (forward-line 0)
	   (let ((context (math-i--context)))
	     (cond ((eq context :comment) 
		    (message "math-indent-string")
		    (math-comment-indent))
		   ((eq context :string) 
		    (message "math-indent-string")
		    nil)
		   ((eq context :code) 
		    (message "math-indent-code")
		    (math-code-indent)))))))
    (if (numberp point)
	(goto-char point))))

(defun math-fill ()
  (interactive)
  (let ((context (math-ind-context)))
    (cond ((eq context :comment) (math-comment-fill))
	  (t nil))))

(provide 'math-indent)

