
(require 'math-nud)
(require 'math-led)

(defconst math-nud-left-bp-table (make-hash-table :test 'equal)
  "Maps a token identifier to the identifier's nud left binding power.")

(defconst math-nud-fn-table (make-hash-table :test 'equal)
  "Maps a token identifier to the identifier's nud parse function.")

(defconst math-led-left-bp-table (make-hash-table :test 'equal)
  "Maps a token identifier to the identifier's led left binding power.")

(defconst math-led-fn-table (make-hash-table :test 'equal)
  "Maps a token identifier to the identifier's led parse function.")

(defun math-put-table (key value table)
  (if (gethash key table)
      (error "Identifier %s already has an entry in table %s" key table)
    (puthash key value table)))

(defun math-get-table (key table)      
  (gethash key table))

;; Methods for registering identifiers in the parser tables.
;;
(defun math-register-symbol (identifier)
  (math-put-table identifier 0 math-nud-left-bp-table)
  (math-put-table identifier 0 math-led-left-bp-table))

(defun math-register-symbol-nud (identifier bp fn)
  (math-put-table identifier bp math-nud-left-bp-table)
  (math-put-table identifier fn math-nud-fn-table))

(defun math-register-symbol-led (identifier bp fn)
  (math-put-table identifier bp math-led-left-bp-table)
  (math-put-table identifier fn math-led-fn-table))

(defun math-register-literal (identifier &optional nud)
  "Add the given literal identifier to the parser tables."
  (math-put-table identifier (or nud 'math-parse-nud-literal) math-nud-fn-table)
  (math-put-table identifier 0 math-nud-left-bp-table))

(defun math-register-nud (identifier left-bp &optional nud)
  "Add the given nud identifier to the parser tables."
  (math-put-table identifier (or nud 'math-parse-nud) math-nud-fn-table)
  (math-put-table identifier left-bp math-nud-left-bp-table))

(defun math-register-led-flat (identifier left-bp &optional led)
  "Add the given led identifier to the parser tables."
  (math-put-table identifier (or led 'math-parse-led-flat) math-led-fn-table)
  (math-put-table identifier left-bp math-led-left-bp-table))

(defun math-register-led-left (identifier left-bp &optional led)
  "Add the given led identifier to the parser tables."
  (math-put-table identifier (or led 'math-parse-led-left) math-led-fn-table)
  (math-put-table identifier left-bp math-led-left-bp-table))

(defun math-register-led-right (identifier left-bp &optional led)
  "Add the given led identifier to the parser tables."
  (math-put-table identifier (or led 'math-parse-led-right) math-led-fn-table)
  (math-put-table identifier left-bp math-led-left-bp-table))


(provide 'math-parse-util)
