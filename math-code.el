
(require 'math-parse)

;; Indent the current line based on the parameters in
;; `math-indent-list'. Return the new location for point if line is
;; successfully indented, nil otherwise.
;;
;; Assumptions: 
;;     point is at bol for current line.
;;     point is not inside of a comment or string.
(defun math-code-indent ()
  (let ((pt (math-c--parse-to-point))
	(errors (math-match-tree 'Error pt)))
    
    (cond

     ;; If the parse is clean, the next token must start a new
     ;; statement.
     ((not errors)
      (let* ((link (math-link-lookup :statement))
	     (source (math-link-source link))
	     (offset (math-link-offset link)))
	(cond
	 ((equal :bol source)
	  (math-link-offset link)
	  (indent-line-to offset)
	  (move-to-column offset)
	  (point)))))

     ;; If there is a single :eof error in the parse, then we need to
     ;; analyze the parse.
     ((and (equal 1 (length errors)) (equal :eof (math-token-class (nth 3 errors))))
      nil)

     (t nil))))
     

;; Parse from bob to current point
(defun math-c--parse-to-point ()
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (math-parse-program))))



(defun math-compute-indent (parse-tree indentation-rules)
  ;; 
  )

(provide 'math-code)
