
(defconst math-tok-identifier-re
  "[$a-zA-Z]+[$a-zA-Z0-9_]*"
  "The regular expression for a Mathmatica identifier.")

(defconst math-tok-symbol-re
  "\\\\\\[\\([A-Z][A-Za-z0-9]*\\)]"
  "The regular expresion for a Mathematica syntax character.")

(defconst math-tok-string-re
  "\"[^\"\\\\]*\\(?:\\\\.[^\"\\\\]*\\)*\""
  "The regular expression for a Mathematica string.")

(defconst math-tok-number-re
  "[0-9]*\\.?[0-9]+"
  "The regular expression for a Mathematica number.")

(defconst math-tok-slot-re
  "##?[0-9]*"
  "The regular expression for a Mathematica slot.")

(defconst math-tok-out-re
  "%+[0-9]*"
  "The regular expression for a Mathematica slot.")

(defconst math-tok-operator-list
  '("::" "_" "<<" "\\+" "\\&" "\\%" "\\_" "\\!" "?" "[" "[[" "]" 
    "\\*" "++" "--" "@" "~" "/@" "//" "@@" "@@@" "!" "!!" "*" "'" "<>"
    "^" "\\^" "\\@" "-" "+" "/" "\\/" "*" ";;" "==" "!=" ">" ">=" "<" 
    "<=" "===" "=!=" "&&" "||" ".." "..." "|" ":" "~~" "/;" "->" ":>"
    "/." "//." "+=" "-=" "*=" "/=" "&" "=" ":=" "^=" "^:=" "/:" "=." ">>" 
    ">>>" ";" "\\`" "," "(" ")" "{" "}")
  "The Mathematica operators.")

(defconst math-tok-operator-re
  (regexp-opt math-tok-operator-list))

(provide 'math-token-defs)
