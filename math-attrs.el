
;; Attributes class
;;
;; External names: math-attributes-*
;; Internal names: math-a--*

(defconst math-a--bp-index 0 "The binding power.")
(defconst math-a--fn-index 1 "The parsing function.")
(defconst math-a--name-index 2 "The name.")
(defconst math-a--last-index 3 "The number of slots.")

(defun math-attributes-make-instance (bp fn name)
  "Create a attributes instance."
  (let ((new-obj (make-vector math-a--last-index nil)))
    (aset new-obj math-a--bp-index bp)
    (aset new-obj math-a--fn-index fn)
    (aset new-obj math-a--name-index name)
    new-obj))

(defun math-attributes-bp (attrs)
  "The binding power attribute."
  (aref attrs math-a--bp-index))

(defun math-attributes-fn (attrs)
  "The parsing function attribute."
  (aref attrs math-a--fn-index))

(defun math-attributes-name (attrs)
  "The name attribute."
  (aref attrs math-a--name-index))

;; Getting and setting attributes in the parser tables.
;;
(defun math-attributes-get (key table)
  (let ((attrs (gethash key table)))
    (unless attrs (error "No attributes for `%s' in parser table `%s'" key table))
    attrs))

(defun math-attributes-set (key attrs table)
  (let ((attrs (gethash key table)))
    (if attrs (error "Attributes already set for `%s' in parser table `%s'" key table)))
  (puthash key attrs table))

(provide 'math-attrs)
