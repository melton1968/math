
;; Append to list
;;
(defun math-append-to-list (list elem)
  "If elem is not nil, append elem to the end of list and return
list."
  (if elem (setcdr (last list) (cons elem nil)))
  list)

(provide 'math-util)
