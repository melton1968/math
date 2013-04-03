
;; Append to list
;;
(defun math-append-to-list (list elem)
  "If elem is not nil, append elem to the end of list and return
list."
  (if elem (setcdr (last list) (cons elem nil)))
  list)

(defun math-apply-tree (function tree)
  (cond
   ((listp tree)
    (let ((result (funcall function tree)))
      (dolist (elem (cdr tree) result)
	(setq result (append (math-apply-tree function elem) result)))))
   (t nil)))

(defun math-match-tree (operator tree)
  (math-apply-tree (lambda (tree) (if (equal operator (car tree)) `(,tree) nil)) tree))

(defun math-invert-tree (tree) tree)

(provide 'math-util)
