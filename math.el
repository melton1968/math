
(require 'math-syntax)
(require 'math-indent)

(defconst math-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-i] 'math-indent-line)
    (define-key map [?\C-c ?\C-f] 'math-next-token-command)
    (define-key map [?\C-c ?\C-b] 'math-prev-token-command)
    (define-key map [?\C-c ?\C-p] 'math-parse-command)
    map)
  "Keymap for `math-mode'.")

(define-derived-mode math-mode prog-mode "Math"
  "Major mode for editing Mathmatica code.

The hook `math-mode-hook' is run with no args at mode initialization.

  \\{math-mode-map}"
  :syntax-table math-syntax-table
  :abbrev-table nil

  ;; We want to use the math-syntax-table to automagically handle
  ;; comments.
  (set (make-local-variable 'comment-start) "(*")
  (set (make-local-variable 'comment-end) "*)")
  (set (make-local-variable 'comment-use-syntax) t)

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  ;; We have to override the syntax-table to properly recognize
  ;; Mathmatica Syntax Characters as keywords.
  (set (make-local-variable 'syntax-propertize-function)
       math-syntax-propertize-function)

  ;; Turn on the syntax highlighting.
  (set (make-local-variable 'font-lock-defaults)
       '((math-mode-font-lock-defaults)))

  ;; Finally, run the math-mode-hook to users can make customizations
  ;; or take other actions.
  (run-hooks 'math-mode-hook)
  )

(provide 'math-mode)
