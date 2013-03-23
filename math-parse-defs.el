
(require 'math-nud)
(require 'math-led)
(require 'math-attrs)

(defconst math-nud-table (make-hash-table :test 'equal)
  "Maps a token identifier to it's parser nud attributes.")

(defconst math-led-table (make-hash-table :test 'equal)
  "Maps a token identifier to it's parser led attributes.")

;; Methods for registering tokens in the parser tables.
;;
(defun math-register-nud (id bp fn &optional name)
  (math-attributes-set id (math-attributes-make-instance bp fn name) math-nud-table))

(defun math-register-nud-prefix (id bp &optional name)
  (math-register-nud id bp 'math-parse-nud-prefix name))

(defun math-register-nud-blank (id bp &optional name)
  (math-register-nud id bp 'math-parse-nud-blank name))

(defun math-register-led (id bp fn &optional name)
  (math-attributes-set id (math-attributes-make-instance bp fn name) math-led-table))

(defun math-register-led-flat (id bp &optional name)
  (math-register-led id bp 'math-parse-led-left name))

(defun math-register-led-left (id bp &optional name)
  (math-register-led id bp 'math-parse-led-left name))

(defun math-register-led-right (id bp &optional name)
  (math-register-led id bp 'math-parse-led-right name))

(defun math-register-led-postfix (id bp &optional name)
  (math-register-led id bp 'math-parse-led-postfix name))

(defun math-register-symbol (id)
  (math-register-nud id 0 nil)
  (math-register-led id 0 nil))


;; Literals
;;
(math-register-nud :identifier 0 'math-parse-nud-literal)
(math-register-nud :string 0 'math-parse-nud-literal)
(math-register-nud :number 0 'math-parse-nud-literal)
(math-register-nud :slot 0 'math-parse-nud-literal)
(math-register-nud "\\[Infinity]"' 0 'math-parse-nud-literal 'Infinity)

;; _                     --> Blank[]
;; _expr1                --> Blank[expr1]
;; __                    --> BlankSequence[]
;; __expr1               --> BlankSequence[expr1]
;; ___                   --> BlankNullSequence[]
;; ___expr1              --> BlankNullSequence[expr1]
;; _.                    --> Optional[Blank[]]
(math-register-nud-blank "_" 800 'Blank)
(math-register-nud-blank "__" 800 'BlankSequence)
(math-register-nud-blank "___" 800 'BlankNullSequence)

;; symb_                     --> Pattern[symb,Blank[]]
;; symb_expr1                --> Pattern[symb,Blank[expr1]]
;; symb__                    --> Pattern[symb,BlankSequence[]]
;; symb__expr1               --> Pattern[symb,BlankSequence[expr1]]
;; symb___                   --> Pattern[symb,BlankNullSequence[]]
;; symb___expr1              --> Pattern[symb,BlankNullSequence[expr1]]
;; symb_.                    --> Optional[Pattern[symb,Blank[]]]

;; expr::string          --> MessageName[expr,"string"]
;; expr::string::string  --> MessageName[expr,"string"]
(math-register-led-left "::" 780 'MessageName)

;; name[expr1,expr2,...]
;;
(math-register-led "[" 745 'math-parse-led-sequence)

;; ?? Grouping operators ?? I do not know where these should go.
(math-register-nud "(" 745 'math-parse-nud-paren)
(math-register-nud "{" 745 'math-parse-nud-sequence)

;; expr1 @ expr2    --> expr1[expr2]
(math-register-led-right "@" 660)

;; expr1 ~~ expr2 ~~ expr3  --> expr2[expr1,expr3]

;; expr1 /@  expr2  -->  Map[expr1,expr2]
;; expr1 //@ expr2  -->  MapAll[expr1,expr2]
;; expr1 @@  expr2  -->  Apply[expr1,expr2]
;; expr1 @@@ expr2  -->  Apply[expr1,expr2,{1}]
(math-register-led-right "/@" 640 'Map)
(math-register-led-right "//@" 640 'MapAll)
(math-register-led-right "@@" 640 'Apply)
(math-register-led-right "@@@" 640 'Apply)

(math-register-led-right "^" 590 'Power)

;;
(math-register-nud-prefix "+" 490)
(math-register-nud-prefix "-" 490)

;;
(math-register-led-left "/" 480 'Divide)

;;
(math-register-led-left "*" 410 'Times)

;;
(math-register-led-flat "+" 330 'Plus)
(math-register-led-flat "-" 330 'Minus)

;; expr1 == expr2           --> Equal[expr1,expr2]
;; expr1 != expr2           --> Unequal[expr1,expr2]
;; expr1 >  expr2           --> Greater[expr1,expr2]
;; expr1 >= expr2           --> GreaterEqual[expr1,expr2]
;; expr1 <  expr2           --> Less[expr1,expr2]
;; expr1 <= expr2           --> LessEqual[expr1,expr2]
(math-register-led-flat "==" 290 'Equal)
(math-register-led-flat "!=" 290 'Unequal)
(math-register-led-flat ">" 290 'Greater)
(math-register-led-flat ">=" 290 'GreaterEqual)
(math-register-led-flat "<" 290 'Less)
(math-register-led-flat "<=" 290 'LessEqual)

;; expr..                   --> Repeated[expr]
;; expr...                  --> RepeatedNull[expr]
(math-register-led-postfix ".." 170 'Repeated)
(math-register-led-postfix "..." 170 'RepeatedNull)

;; expr1|expr2              --> Alternatives[expr1,expr2] 
(math-register-led-flat "|" 160 'Alternatives)

;; symb:expr                --> Pattern[symb,expr]
;; patt:expr                --> Optional[patt,expr]
(math-register-led-left ":" 150 'Pattern)

;; expr1 -> expr2    --> Rule[expr1,expr2]
;; expr1 :> expr2    --> RuleDelayed[expr1,expr2]
(math-register-led-right "->" 120 'Rule)
(math-register-led-right ":>" 120 'RuleDelayed)

;; expr1 /. expr2           --> ReplaceAll[expr1,expr2]
;; expr1 //. expr2          --> ReplaceRepeated[expr1,expr2]
(math-register-led-left "/." 110 'ReplaceAll)
(math-register-led-left "//." 110 'ReplaceRepeated)

;; expr1 &                  --> Function[expr1]
(math-register-led-postfix "&" 90 'Function)

;; expr1   = expr2          --> Set[expr1,expr2]
;; expr1  := expr2          --> SetDelayed[expr1,expr2]
;; expr1  ^= expr2          --> Upset[expr1,expr2]
;; expr1 ^:= expr2          --> UpsetDelayed[expr1,expr2]
;; expr =.                  --> UnSet[expr]
;; expr1 |-> expr2          --> Function[{expr1},expr2]
;; symb /: expr1  = expr2   --> TagSet[symb,expr1,expr2]
;; symb /: expr1 := expr2   --> TagSetDelayed[symb,expr1,expr2]
;; symb /: expr1 =.         --> TagUnset[expr]
(math-register-led-right "=" 40 'Set)
(math-register-led-right ":=" 40 'SetDelayed)
(math-register-led-right "^=" 40 'Upset)
(math-register-led-right "=." 40 'UpsetDelayed)
(math-register-led-right "|->" 40 'UnSet)
(math-register-led-right "/:" 40 'Tag)
;;(math-register-led-tagset "/:" 40 :Tag)

;; expr>>filename      --> Put[expr,"filename"]
;; expr>>>filename     --> PutAppend[expr,"filename"]
(math-register-led-left ">>" 30 'Put)
(math-register-led-left ">>>" 30 'PutAppend)

;; expr1;expr2;expr3   --> CompoundExpression[expr1,expr2,expr3]
;; expr1;expr2;        --> CompoundExpression[expr1,expr2,Null]
;;
;; TODO - add lookahead in tokenizer for ws to eol so that the eos
;; parser can check for the second version and add the Null marker.
;;(math-register-led-flat ";" 20)

;; expr1 \` expr2      --> FormBoxp[expr2,expr1]
(math-register-led-right "\\`" 10 'FormBox)

;; Separators need to be registered.
(math-register-symbol ",")

;; Closers need to be registered.
(math-register-symbol ";")
(math-register-symbol "]")
(math-register-symbol ")")
(math-register-symbol "}")
(math-register-symbol :eof)
(math-register-symbol :eol)

(provide 'math-parse-defs)
