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

(defvar math-comment-first-line-fill-option :own-line
  "How to handle the first line of a comment when doing a
  fill-paragraph.

`:verbatim' - Leave the first line as it appears.
`:own-line' - Put the comment start string on the first line by itself.
`:fill-to'  - Fill the text starting immediately after the comment start string.
")

(defvar math-comment-last-line-fill-option :own-line
  "How to handle the last line of a comment when doing a fill-paragraph.

`:verbatim' - Leave the last line as it appears.
`:own-line' - Put the comment end string on the last line by itself.
`:fill-to'  - Fill the text right up to the comment end string.
")

(defconst math-link-target-index 0 "The target element to be indented.")
(defconst math-link-source-index 1 "The source element relative to which indent.")
(defconst math-link-offset-index 2 "The relative offset.")
(defconst math-link-start-index 3 "Link to start of source element.")
(defconst math-link-last-index 4 "The number of slots.")

(defun math-link (target source offset &optional start)
  "Create an indentation link for target relative to source."
  (let ((new-obj (make-vector math-link-last-index nil)))
    (aset new-obj math-link-target-index target)
    (aset new-obj math-link-source-index source)
    (aset new-obj math-link-offset-index offset)
    (aset new-obj math-link-start-index start)
    new-obj))

(defun math-link-target (link)
  (aref link math-link-target-index))

(defun math-link-source (link)
  (aref link math-link-source-index))

(defun math-link-offset (link)
  (aref link math-link-offset-index))

(defun math-link-start (link)
  (aref link math-link-start-index))

(defconst math-indent-list 
  (list 
   (math-link :comment-first :comment-start 0 t)
   (math-link :comment-text :comment-start +3 t)
   (math-link :comment-last :comment-start +1 t)
	))

(defun math-link-lookup (target)
  (let ((list math-indent-list)
	(result nil))
    (while (and list (not result))
      (if (equal target (math-link-target (car list)))
	  (setq result (car list))
	(setq list (cdr list))))
    result))

(defun math-column-at-pos (pos)
  (save-excursion 
    (goto-char pos) 
    (current-column)))

(defun math-comment-start-element ()
  "The extent of the start comment sequence as the pair (begin . end)"
  (save-excursion
    (let ((begin-comment (nth 8 (syntax-ppss))))
      (goto-char begin-comment)
      (let ((begin (point)))
	(goto-char (+ begin (length comment-start)))
	(while (equal (char-before (point)) (char-after (point)))
	  (forward-char 1))
	(cons begin (point))))))

(defun math-comment-end-element ()
  "The extent of the end comment sequence as the pair (begin . end)"
  (save-excursion
    (let ((begin-comment (nth 8 (syntax-ppss))))
      (goto-char begin-comment)
      (forward-comment 1)
      (let ((end (point)))
	(goto-char (- end (length comment-end)))
	(while (equal (char-before (point)) (char-after (point)))
	  (backward-char 1))
	(cons (point) end)))))

(defun math-comment-last-line-p ()
  "Is point on the last line of a comment?"
  (save-excursion 
    (let ((current-line (line-number-at-pos (point))))
      (goto-char (nth 8 (syntax-ppss)))
      (forward-comment 1)
      (let ((last-line (line-number-at-pos (point))))
	(= current-line last-line)))))

(defun math-comment-indent ()
  "Indent the current line and return a new point."
  (let* ((target (if (math-comment-last-line-p) :comment-last :comment-text))
	 (link (math-link-lookup target))
	 (source (math-link-source link)))
    (cond
     ((equal :comment-start source)
      (let ((start-element (math-comment-start-element)))
	(let ((pos (if (math-link-start link) (car start-element) (cdr start-element))))
	  (let ((column (+ (math-link-offset link) (math-column-at-pos pos))))
	    (indent-line-to column)
	    (move-to-column column t)
	    (point)))))
     (t nil))))

(defun math-comment-fill ())
  
(provide 'math-comment)
