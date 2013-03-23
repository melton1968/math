
(require 'math-parse)

;; Indent the current line based on the parameters in
;; `math-indent-list'. Return the new location for point if line is
;; successfully indented, nil otherwise.
;;
;; Assumptions: 
;;     point is at bol for current line.
;;     point is not inside of a comment or string.
(defun math-code-indent ()
  ;; Parse from bob to current point
  (save-restriction
    (narrow-to-region (point-min) (point))
    (goto-char (point-min))
    (let ((pt (math-parse-program)))
      (message "%s" pt)
      pt)))



(provide 'math-code)
