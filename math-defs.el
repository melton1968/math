

(defconst math-operators
  '(
    ("+" 10 :infix-l)
    ("-" 10 :infix-l)
    ("*" 20 :infix-l)
    ("/" 20 :infix-l)
    ("^" 30 :infix-r)
    )
  "The Mathematica operators with precedence information.")

(defconst math-comment-start
  "(\*"
  "The Mathematica start comment delimiter.")

(defconst math-comment-end
  "\*)"
  "The Mathematica end comment delimiter.")

(defconst math-user-name-re
  "[a-zA-Z0-9_]+"
  "The regular expression for a Mathmatica user identifier.")

(defconst math-system-name-re
  "[A-Z][a-zA-Z]*"
  "The regular expression for a Mathmatica system identifier.")

(defconst math-symbol-re
  "\\\\\\[\\([A-Z][A-Za-z0-9]*\\)]"
  "The regular expresion for a Mathematica syntax character.")

(defconst math-string-re
  "\"[^\"\\\\]*\\(?:\\\\.[^\"\\\\]*\\)*\""
  "The regular expression for the start of a Mathematica string.")

(defconst math-brace-re
  "\\[\\|(\\|{\\|\\]\\|)\\|}"
  "The regular expression for a Mathematica brace.")

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
  "[0-9]*\\.?[0-9]+"
  "The regular expression for a Mathmatica number.")

(defconst math-operator-re
  (regexp-opt '("=" ":=" "," ";" ":" "::" "|" "-" "+" "*" "/" "^"))
  "The regular expression for a Mathmatica operator.")

(defconst math-token-number ":number"
  "The token identifier representing a number.")

(defconst math-token-string ":string"
  "The token identifier representing a string.")

(defconst math-token-name ":name"
  "The token identifier representing a name.")

(defconst math-token-eof ":eof"
  "The token identifier representing the end of the input.")

(defconst math-token-unknown ":unknown"
  "The token identifier representing an unknown token.")

(defconst math-token-re-alist
  `((,math-token-number . ,math-number-re)
    (,math-token-string . ,math-string-re)
    (,math-token-name   . ,math-user-name-re)
    (,math-token-name   . ,math-system-name-re)
    (:operator         . ,math-brace-re)
    (:operator         . ,math-symbol-re)
    (:operator         . ,math-operator-re))
  "Association list of token-id and regular expressions for
  matching each type of token.")

(provide 'math-defs)
