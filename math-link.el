


(defconst math-l--target-index 0 "The target element to be indented.")
(defconst math-l--source-index 1 "The source element relative to which indent.")
(defconst math-l--offset-index 2 "The relative offset.")
(defconst math-l--start-index 3 "Link to start of source element.")
(defconst math-l--last-index 4 "The number of slots.")

(defun math-link (target source offset &optional start)
  "Create an indentation link for target relative to source."
  (let ((new-obj (make-vector math-l--last-index nil)))
    (aset new-obj math-l--target-index target)
    (aset new-obj math-l--source-index source)
    (aset new-obj math-l--offset-index offset)
    (aset new-obj math-l--start-index start)
    new-obj))

(defun math-link-target (link)
  (aref link math-l--target-index))

(defun math-link-source (link)
  (aref link math-l--source-index))

(defun math-link-offset (link)
  (aref link math-l--offset-index))

(defun math-link-start (link)
  (aref link math-l--start-index))

(provide 'math-link)
