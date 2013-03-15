
;; Define a syntax-table suitable for lexical analysis of
;; Mathematica code.
;;
;; Mapping the comment delimiters.
;; Mapping all the brace constructs (i.e. (),[],{}) to be matching parens.
;; Mapping the legal identifier characters to be of word type.
;; Mapping the operator characters to be of punctuation type.
;;
;; TODO: Check to see which characters actually need to modified from
;; the standard syntax table.
(defconst math-syntax-table
  (let ((table (make-syntax-table)))
    
    ;; For Mathmatica syntax, all of the paren characters function as
    ;; grouping elements.

    ;; '(' -> open '(', match ')', 1 1st comment, n nestable.
    (modify-syntax-entry ?\( "()1n" table)
    
    ;; ')' -> close ')', match '(', 4 4th comment, n nestable.
    (modify-syntax-entry ?\) ")(4n" table)
    
    ;; '*' -> punct ., 2 2nd comment, 3 3rd comment, n nestable.
    (modify-syntax-entry ?\* ". 23n" table)
    
    ;; '[' -> open '(', match ']'
    (modify-syntax-entry ?\[ "(]" table)
    
    ;; ']' -> close ')', match '['
    (modify-syntax-entry ?\] ")[" table)
    
    ;; '{' -> open '(', match '}'
    (modify-syntax-entry ?\{ "(}" table)
    
    ;; '}' -> close ')', match '{'
    (modify-syntax-entry ?\} "){" table)
    
    ;; The following characters are punctuation (i.e. they cannot appear
    ;; in identifiers).
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?\' "\"" table)
    
    ;; '$' -> symbol since $ is allowed in identifier names.
    (modify-syntax-entry ?\$ "w" table)
    
    ;; '_' -> symbol since _ is allowed in identifier names.
    (modify-syntax-entry ?_ "w" table)
    
    table)
  "Syntax table for `math-mode'.")

;; Mathematica syntax uses the backslash character (`\') and the
;; square bracket characters (`[' and `]') in two different contexts.
;;
;; In the `normal' context, the backslash character is an escape
;; character and the square bracket characters are opening and closing
;; paren characters. This is how they are marked in the `math-syntax-table'.
;;
;; However, both the backslash character and the square bracket
;; characters are used in Mathematica Syntax Characters (e.g. \[Pi],
;; \[Infinity], etc.).
;;
;; In order to recognize a Mathematica Syntax Character as a keyword,
;; the character class marked in the `math-syntax-table' must be
;; overridden in certain contexts. The
;; `math-syntax-propetize-function' variable points to a function that
;; provides the override logic. This function is installed in the
;; startup code for `math-mode'.
;;
(defconst math-syntax-propertize-function
  (syntax-propertize-rules
   ("\\\\\\[\\([A-Z][A-Za-z]*\\)]" (0 "_"))))

;; The syntax highlighting configuration.
;;
(defconst math-mode-font-lock-defaults 
  (list
   ;; Mathmatica function calls (i.e. functions that start with a
   ;; uppercase letter). E.g., Plus[1,2]
   ;;
   ;; <UppercaseIdentifier>[
   ;;
   '("\\<\\([A-Z][A-Za-z0-9]*\\)\\>" 1 font-lock-type-face)
   
   ;; User defined function calls (i.e. functions that start with a
   ;; lowercase letter). E.g., myPlus[1,2]
   ;;
   ;; <LowercaseIdentifier>[
   ;;
   '("\\<\\([A-Za-z][A-Za-z0-9]*\\)\\>\\[" 1 font-lock-function-name-face)
   
   ;; Mathmatica so called Syntax Characters. E.g., \[Pi]. Since
   ;; '\' is an escape character, a syntax-table-function must be
   ;; used to modifiy the character properties for these forms.
   ;;
   ;; \[Identifier]
   ;;
   '("\\\\\\[\\([A-Z][A-Za-z0-9]*\\)]" 0 font-lock-constant-face)
   
   ;; Mathmatica variable or function names (i.e. variables or
   ;; functions that start with an upperase letter).
   ;;
   ;; <UppercaseIdentifier>
   ;;
   '("\\<\\([A-Z][A-Za-z0-9]*\\)\\>" 1 font-lock-keyword-face)
   
   ;; Pattern variable (i.e. a variable containing an underscore.
   ;;
   ;; <LowercaseIdentifier_>
   ;;
   '("\\<\\([a-z][A-Za-z0-9]*[_]\\)\\>" 1 font-lock-variable-name-face)
   )
  "Font lock defaults for `math-mode'.")

(provide 'math-syntax)
