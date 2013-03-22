
;; Append to list
;;
(defun math-append-to-list (list elem)
  "Appends elem to the end of list."
  (setcdr (last list) (cons elem nil)))

(provide 'math-util)
