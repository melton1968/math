;;;;;; Comment Indentation
;; 
;; In Mathematica comments start with the `(*' character sequence and
;; end with the `*)' character sequence, can be nested and are multi-line. 
;;
;;;; * Nomenclature *
;;
;; Consider the following comment stucture:
;; 
;; (**** There maybe text here.
;;       There is almost always text here.
;;       Sometimes there is text here too. ***)
;;
;; comment-start-string: `(****'
;;   comment-end-string: `***)'
;;           first-line: `(**** There maybe text here'
;;            last-line: `Sometimes there is text here too. ***)'
;;
;;;; * Indentation Rules *
;; 
;; The comment-start-string can be anchored relative to 1) the
;; enclosing parenthetical grouping or 2) the beginning of the line.
;;
;; The comment text can be anchored relative to 1) the
;; comment-start-string, or 2) the beginning of the line.
;;
;; If the comment-end-string is the first text on a line, it can be
;; anchored relative to 1) the comment-start-string, or 2) the
;; beginning of the line.
;;
;;;; * Fill Rules *
;;
;; The comment-start-string can be alone as the first line or the
;; comment text can be filled starting immediately after the
;; comment-start-string.
;;
;; The fill-column of the comment-text can be anchored
;; relative to 1) the comment-start-string, or 2) the beginning of the
;; line.
;;
;; The comment-end-string can be alone as the last line of the comment
;; or the comment text can be filled right up to the
;; comment-end-string.
;;
;;;; * Text Elements *
;;
;; `:comment-start'    - The comment-start-string.
;; `:comment-text'     - The comment text.
;; `:comment-end'      - The comment-end-string.
;;
;;;; * Anchor Elements *
;;
;; `:bol'           - the beginning of the line
;; `:comment-start' - the comment-start-string.
;; `:group'         - the enclosing parenthetical group.
;;
;; Use `math-anchor' to create an anchor.
;;
;; (defun math-anchor (anchor position offset) 
;; `anchor' - the anchor element.  
;; `position' - `:begin' or `:end' anchor to the start of the anchor text.
;; `offset' - The relative offset from the anchor.
;;

(require 'math-link)

(defconst math-comment-first-line-fill-option 'next-line
  "How to handle the first line of a comment when doing a
  fill-paragraph.

'default   - Allow fill-paragraph to fill the text including the delimiter.
'next-line - Fill the text starting on the line after the delimiter.
'verbatim  - Same as 'next-line, except leave the first line unaltered.
")

(defconst math-comment-last-line-fill-option 'next-line
  "How to handle the last line of a comment when doing a fill-paragraph.

'default       - Allow fill-paragraph to fill the text including the delimiter.
'previous-line - Fill the text up to the line before the delimiter.
'verbatim      - Same as 'previous-line, except leave the last line unaltered.
")

(defun math-link-lookup (target)
  (let ((list math-indent-list)
	(result nil))
    (while (and list (not result))
      (if (equal target (math-link-target (car list)))
	  (setq result (car list))
	(setq list (cdr list))))
    result))

(defun math-c--start-delimiter-range ()
  (save-excursion
    (let ((begin (nth 8 (syntax-ppss))))
      (goto-char begin)
      (forward-char 2)
      (while (= (char-before) (char-after))
	(forward-char))
      (cons begin (point)))))

(defun math-c--column-at-pos (pos)
  (save-excursion 
    (goto-char pos) 
    (current-column)))

(defun math-c--first-line-p ()
  "Is point on the first line of a comment?"
  (save-excursion 
    (let ((current-line (line-number-at-pos (point))))
      (goto-char (nth 8 (syntax-ppss)))
      (let ((last-line (line-number-at-pos (point))))
	(= current-line last-line)))))

(defun math-c--last-line-p ()
  "Is point on the last line of a comment?"
  (save-excursion 
    (let ((current-line (line-number-at-pos (point))))
      (goto-char (nth 8 (syntax-ppss)))
      (forward-comment 1)
      (let ((last-line (line-number-at-pos (point))))
	(= current-line last-line)))))

(defun math-c--perform-indent (column)
  (indent-line-to column)
  (move-to-column column t)
  (point))

(defun math-comment-indent ()
  "Indent the current line."
  (let* ((target (if (math-c--last-line-p) :comment-last :comment-text))
	 (link (math-link-lookup target))
	 (source (math-link-source link)))
    (cond
     ((equal :comment-start source)
      (let ((delimiter (math-c--start-delimiter-range)))
	(let ((pos (if (math-link-start link) (car delimiter) (cdr delimiter))))
	  (math-c--perform-indent (+ (math-link-offset link) (math-c--column-at-pos pos))))))

     ((equal :bol source)
      (math-c--perform-indent (+ (math-link-offset link) 0)))

     (t 
      (message 
       "math-comment-indent: `%s' is not implemented as a source for the `%s' indentation target"
       source target)
      nil))))

(defun math-c--apply-first-line-fill-option ()
  ;; Apply the `math-comment-first-line-fill-option' and return the start
  ;; position of the paragraph.
  ;;
  ;; The region is assumed to have been narrowed to the comment.
  (goto-char (point-min))
  (forward-char 2)
  (while (= (char-before) (char-after))
    (forward-char 1))
  
  (cond
   ;; The comment text starts immediately after the comment delimiter.
   ((equal 'default math-comment-first-line-fill-option)
    (point))
   
   ;; The comment start sequence is on its own line.
   ((equal 'next-line math-comment-first-line-fill-option)
    ;; Delete any whitespace immediately following the comment
    ;; delimiter.
    (if (looking-at "[[:blank:]]*")
	(delete-region (match-beginning 0) (match-end 0)))
    ;; If necessary, insert a newline so that the comment delimiter
    ;; is on its own line.
    (if (not (equal (char-after) ?\n))
	(newline))
    ;; Move past the newline and return the position.
    (forward-char 1)
    (point))
   
   ;; The first line of the comment is left as is and the comment
   ;; text starts on the next line.
   ((equal 'verbatim math-comment-first-line-fill-option)
    (forward-line 1)
    (point))
   
   (t
    (message 
     "math-comment: `%s' is not implemented as a math-comment-first-line-fill-option"
     math-comment-first-line-fill-option)
    (point))))

(defun math-c--apply-last-line-fill-option ()
;; Apply the `math-comment-last-line-fill-option' and return the end
;; position of the paragraph.
;;
;; The region is assume to have been narrowed to the comment.
  (goto-char (point-max))
  (backward-char 2)
  (while (= (char-before) (char-after))
    (backward-char 1))

  (cond
   ;; The comment text starts immediately before the comment delimiter.
   ((equal 'default math-comment-last-line-fill-option)
    (point))
   
   ;; The comment end sequence is on its own line.
   ((equal 'next-line math-comment-last-line-fill-option)
    ;; If necessary, insert a newline so that the comment delimiter
    ;; is on its own line.
    (if (looking-back "^[[:blank:]]*")
	(forward-line 0)
      (newline)
      (forward-char 1)
      (save-excursion
	(math-comment-indent))
      )
    (point))
   
   ;; The last line of the comment is left as is and the comment
   ;; text ends on the previous line.
   ((equal 'verbatim math-comment-first-line-fill-option)
    (forward-line 0)
    (backward-char 1)
    (point))
   
   (t
    (message 
     "math-comment: `%s' is not implemented as a math-comment-last-line-fill-option"
     math-comment-last-line-fill-option)
    (point))))

(defun math-c--comment-region ()
  ;; The current comment region as a pair (begin . end).
  (save-excursion
    (let ((begin (nth 8 (syntax-ppss))))
      (goto-char begin)
      (forward-comment 1)
      (cons begin (point)))))

(defun math-c--paragraph-region ()
  ;; The current paragraph region as a pair (begin . end)
  (save-excursion
    (forward-paragraph)
    (backward-paragraph)
    (let ((begin (point)))
      (forward-paragraph)
      (cons begin (point)))))

(defun math-comment-fill ()
  "Fill comment using fill-paragraph."
  (let ((c-region (math-c--comment-region)))
    (save-restriction
      (narrow-to-region (car c-region) (cdr c-region))
      (let* ((p-region (math-c--paragraph-region))
	     (first-line-p (= (point-min) (car p-region)))
	     (last-line-p (= (point-max) (cdr p-region)))
	     (begin (if first-line-p (math-c--apply-first-line-fill-option) (point-min)))
	     (end (if last-line-p (math-c--apply-last-line-fill-option) (point-max))))
	(narrow-to-region begin end)
	(fill-paragraph)))))
  

(provide 'math-comment)
