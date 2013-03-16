

(defconst math-operators
  '(
    ("+" 10 :infix-l)
    ("-" 10 :infix-l)
    ("*" 20 :infix-l)
    ("/" 20 :infix-l)
    ("^" 30 :infix-r)
    )
  "The Mathematica operators with precedence information.")

(defconst math-comment-start-re
  "(\*"
  "The regular expression for a Mathematica start comment delimiter.")

(defconst math-comment-end-re
  "\*)"
  "The regular expression for a Mathematica end comment delimiter.")

(defconst math-user-ident-re
  "[a-zA-Z0-9_]+"
  "The regular expression for a Mathmatica user identifier.")

(defconst math-sys-ident-re
  "[A-Z][a-zA-Z]*"
  "The regular expression for a Mathmatica system identifier.")

(defconst math-symbol-re
  "\\\\\\[\\([A-Z][A-Za-z0-9]*\\)]"
  "The regular expresion for a Mathematica syntax character.")

(defconst math-string-re
  "\"[^\"\\\\]*\\(?:\\\\.[^\"\\\\]*\\)*\""
  "The regular expression for the start of a Mathematica string.")

(defconst math-open-brace-re
  "\\[\\|(\\|{"
  "The regular expression for a Mathematica open brace.")

(defconst math-close-brace-re
  "\\]\\|)\\|}"
  "The regular expression for a Mathematica close brace.")

(defconst math-number-re
  ;; Taken from http://reference.wolfram.com/mathmatica/tutorial/InputSyntax.html#7977
  ;;     digits                - integer
  ;;     digits.digits         - approximate number
  ;;     base^^digits          - integer in base
  ;;     base^^digits.digits   - approximate number in base
  ;;     mantissa*^n           - scientific notation
  ;;     base^^mantissa*^n     - scientific notation in base
  ;;     number`               - number in machine precision
  ;;     number`s              - number in precision s
  ;;     number``s             - number in precision s
  ;;
  "[+-]?[0-9]*\\.?[0-9]+"
  "The regular expression for a Mathmatica number.")

(defconst math-operator-re
  (regexp-opt '("=" ":=" "," ";" ":" "::" "|" "-" "+" "*" "/" "^"))
  "The regular expression for a Mathmatica operator.")

(defconst math-token-re-alist
  `((:user-ident . ,math-user-ident-re)
   (:sys-ident . ,math-sys-ident-re)
   (:number . ,math-number-re)
   (:string . ,math-string-re)
   (:open . ,math-open-brace-re)
   (:close . ,math-close-brace-re)
   (:operator . ,math-operator-re))
  "Association list of token-class and regular expressions for
  matching each type of token.")

(provide 'math-defs)
