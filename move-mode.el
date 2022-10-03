;;; move-mode.el --- A major-mode for editing Move language -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Ashok Menon

;; Author: Ashok Menon
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages

;;; License:

;; This file is distributed under the terms of the Apache License
;; (version 2.0).

;;; Commentary:

;; This package implements a major-mode for editing smart contracts
;; written in Move.

;;; Code:



;;; Constants for use with Customization

(defconst core-move-builtin-functions
  '("assert!" "borrow_global" "freeze" "move_from" "move_to")
  "Built-in functions from Core Move")

;;; Customization

(defgroup move-mode nil
  "Support for Move source code."
  :link '(url-link "https://github.com/move-language/move")
  :group 'languages)

(defcustom move-indent-offset 4
  "Number of spaces to indent move code by."
  :type  'integer
  :group 'rust-mode
  :safe #'integerp)

(defcustom move-builtins core-move-builtin-functions
  "Functions to highlight as builtins (mutations require restarting font-lock)."
  :type '(list string)
  :group 'move-mode)

(defcustom move-bin "move"
  "Name of or path to move CLI binary."
  :type 'string
  :group 'move-mode)

(defcustom move-default-arguments ""
  "Default arguments when running common move CLI commands."
  :type 'string
  :group 'move-mode)

(defconst move-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (op '(?+ ?- ?* ?/ ?% ?& ?^ ?| ?< ?> ?! ?&))
      (modify-syntax-entry op "." table))

    ;; Parentheses
    (modify-syntax-entry ?\(  "()" table)
    (modify-syntax-entry ?\)  ")(" table)
    (modify-syntax-entry ?\{  "(}" table)
    (modify-syntax-entry ?\}  "){" table)
    (modify-syntax-entry ?\[  "(]" table)
    (modify-syntax-entry ?\]  ")[" table)

    ;; Comments
    (modify-syntax-entry ?/   ". 124b" table)
    (modify-syntax-entry ?*   ". 23n"  table)
    (modify-syntax-entry ?\n  "> b"    table)
    (modify-syntax-entry ?\^m "> b"    table)

    table))

(defconst move-mode-syntax-table+<>
  (let ((table (copy-syntax-table move-mode-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)

    table)
  "A variant of the syntax table that recognises angle braces as a bracketed
   construct, for use in detecting generic parameters")

;; Keybindings

(defvar move-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c C-b") 'move-build)
    (define-key map (kbd "C-c C-c C-d") 'move-disassemble)
    (define-key map (kbd "C-c C-c C-p") 'move-prover)
    (define-key map (kbd "C-c C-c C-t") 'move-test)
    map))

;;;###autoload
(define-derived-mode move-mode prog-mode "Move"
  "Major mode for Move source code.

\\{move-mode-map}"
  :group 'move-mode
  :syntax-table move-mode-syntax-table

  (setq-local font-lock-defaults
              '(move-mode-font-lock-keywords
                nil ;; KEYWORDS-ONLY
                nil ;; CASE-FOLD
                nil ;; SYNTAX-ALIST
                ;;;;;; VARIABLES
                (font-lock-syntactic-face-function
                 . move-mode-distinguish-comments)))

  ;; ! is punctuation unless it's at the end of a word, in which case,
  ;; it should be treated like piece of the preceding word.
  (setq-local syntax-propertize-function
              (syntax-propertize-rules ("\\sw\\(!\\)" (1 "w"))))

  ;; This is a weirdly lisp-specific variable
  (setq-local open-paren-in-column-0-is-defun-start nil)

  ;; Indentation
  (setq-local indent-line-function 'move-mode-indent-line)
  (setq-local electric-indent-chars
              (cons ?} (and (boundp 'electric-indent-chars)
                            electric-indent-chars)))

  ;; Comments
  (setq-local comment-end        "")
  (setq-local comment-line-break-function #'move-mode-comment-line-break)
  (setq-local comment-multi-line t)
  (setq-local comment-start      "// ")
  (setq-local comment-start-skip "\\(?://[/!]*\\|/\\*[*!]?\\)\\s-*")

  ;; Set paragraph-start to stop multi-line comments creeping up onto
  ;; an empty initial line, or across lines that contain only a `*'
  ;; and are therefore effectively empty.
  (setq-local paragraph-start
              (concat "\\s-*\\(?:" comment-start-skip ;; Comment start
                      "\\|\\*/?[[:space:]]*"          ;; Empty line in a comment
                      "\\|\\)$"))                     ;; Just plain empty
  (setq-local paragraph-separate
              paragraph-start)

  ;; Fill Paragraph
  (setq-local fill-paragraph-function   #'move-mode-fill-paragraph)
  (setq-local normal-auto-fill-function #'move-mode-auto-fill)
  (setq-local adaptive-fill-function    #'move-mode-adaptive-fill)
  (setq-local adaptive-fill-first-line-regexp ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.move\\'" . move-mode))

(defconst move-keywords
  '("abort" "acquires" "as" "break" "const" "continue" "copy" "else" "entry"
    "false" "friend" "fun" "has" "if" "invariant" "let" "loop" "module" "move"
    "mut" "native" "public" "return" "script" "spec" "struct" "true" "use"
    "while"))

(defconst move-prover-keywords
  '("aborts_if" "aborts_with" "apply" "assume" "axiom" "choose" "decreases"
    "ensures" "emits" "except" "exists" "forall" "global" "include" "internal"
    "local" "min" "modifies" "old" "post" "pragma" "requires" "schema"
    "succeeds_if" "to" "update" "with" "where")
  "Keywords that are only used by the move-prover.  Can be added to
   MOVE-BUILTINS to enable highlighting, defaults to not.")

(defconst move-integer-types
  '("u8" "u64" "u128"))

(defconst move-builtin-types
  (append move-integer-types '("address" "bool" "vector")))

(defconst move-abilities
  '("copy" "drop" "store" "key"))

(defconst move-integer-with-type-re
  (eval-when-compile
    (concat "\\_<"
            "\\(?:0x?\\|[1-9]\\)"
            "[[:digit:]a-fA-F]*"
            (regexp-opt move-integer-types t)
            "\\_>")))

(defconst move-ident-re
  "[a-zA-Z][a-zA-Z0-9_]*\\|_[a-zA-Z0-9_]+")

(defconst move-type-re
  "\\_<[A-Z][a-zA-Z0-9_]*\\_>")

(defconst move-limit-by-<>-form
  '(if (not (char-equal ?< (char-after))) (point)
       (with-syntax-table move-mode-syntax-table+<>
         (save-excursion (forward-sexp) (point))))
  "Form that, when evaluated, with the point over an open angled bracket,
   returns the position one after the matching close angled bracket.")

(defconst move-generic-constraint-matcher
  `(,(regexp-opt move-abilities 'symbols)
    ,move-limit-by-<>-form nil
    (0 font-lock-type-face))
  "Font lock sub-matcher for type constraints on generic type parameters,
   enclosed by angle brackets.")

(defvar move-mode-font-lock-keywords
  `((,(regexp-opt move-keywords 'symbols)      . font-lock-keyword-face)
    (,(regexp-opt move-builtin-types 'symbols) . font-lock-type-face)
    ("\\(#\\[[^]]*\\]\\)"                      1 font-lock-preprocessor-face keep)
    (,move-integer-with-type-re                1 font-lock-type-face)

    ;; "Types" heuristic -- CapitalizedIdentifiers.
    (,move-type-re                             . font-lock-type-face)

    ;; Module components
    (,(concat "\\(" move-ident-re "\\)::")     1 font-lock-constant-face)

    ;; Fields, function params, local variables with explicit types
    (,(concat "\\(" move-ident-re "\\)\\s-*:[^:]")
     1 font-lock-variable-name-face)

    ;; Let bindings with inferred type
    (,(concat "\\_<let\\s-+\\(" move-ident-re "\\)\\_>")
     1 font-lock-variable-name-face)

    ;; Function declarations
    (,(concat "\\_<fun\\s-+\\(" move-ident-re "\\)\\s-*")
     (1 font-lock-function-name-face)
     ,move-generic-constraint-matcher)

    ;; Struct declarations
    (,(concat "\\_<struct\\s-+\\(" move-ident-re "\\)\\s-*")
     (1 font-lock-type-face)

     ("\\_<phantom\\_>"
      ,move-limit-by-<>-form
      (with-syntax-table move-mode-syntax-table+<>
        (up-list) (backward-list))
      (0 font-lock-keyword-face))

     ,move-generic-constraint-matcher)

    ("\\_<has\\_>"

     (,(regexp-opt move-abilities 'symbols)
      (save-excursion
        (re-search-forward "{" (point-at-eol) t +1)
        (point))

      nil

      (0 font-lock-type-face)))

    (eval move--register-builtins)))

(defun move-build  () (interactive) (move--compile "build"))
(defun move-prover () (interactive) (move--compile "prover"))
(defun move-test   () (interactive) (move--compile "test"))

(defun move-disassemble (module-name)
  (interactive "sModule: ")
  (move--compile "disassemble" "--name" module-name))

(defun move-mode-distinguish-comments (state)
  "Distinguish between doc comments and normal comments in the given syntax
   STATE."
  (save-excursion
    (goto-char (nth 8 state))
    (cond ((looking-at "//[/!][^/!]")
           'font-lock-doc-face)
          ((looking-at "/[*][*!][^*!]")
           'font-lock-doc-face)
          ('font-lock-comment-face))))

(defun move-mode-indent-line ()
  "Sets the indent of the current line to the column calculated by
   MOVE--INDENT-COLUMN, jumping to that column if the point is currently before
   it, and leaving the point in place otherwise."
  (interactive)
  (when-let ((indent (move--indent-column)))
    ;; Jump to indentation column if the point is currently before it.
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent)
      (save-excursion
        (indent-line-to indent)))))

(defun move-mode-comment-line-break (&optional arg)
  "Create a new line continuing the comment at point."
  (let ((fill-prefix (move-mode-adaptive-fill)))
    (comment-indent-new-line arg)))

(defun move-mode-fill-paragraph (&rest args)
  "Move comment-aware wrapper for `fill-paragraph'."
  (let ((fill-prefix (move-mode-adaptive-fill))
        (fill-paragraph-handle-comment t)
        (fill-paragraph-function
         (unless (eq fill-paragraph-function #'move-mode-fill-paragraph)
           fill-paragraph-function)))
    (apply #'fill-paragraph args) t))

(defun move-mode-auto-fill (&rest args)
  "Move comment-aware wrapper for `do-auto-fill'."
  (let ((fill-prefix (move-mode-adaptive-fill)))
    (apply #'do-auto-fill args) t))

(defun move-mode-adaptive-fill ()
  "If the point is currently in a comment, return the fill prefix to use to
   continue that comment, otherwise return the existing FILL-PREFIX."
  (save-match-data
    (save-excursion
      (if (not (move--ppss-in-comment))
          fill-prefix
        (let* ((comment-start
                (move--ppss-comment-start))
               (comment-indent
                (progn (goto-char comment-start)
                       (beginning-of-line)
                       (buffer-substring-no-properties
                        (point) comment-start))))
          (goto-char comment-start)
          (cond
           ((looking-at "//[/!]*\\s-*")
            (concat comment-indent (match-string-no-properties 0)))
           ((looking-at "/\\*[*!]?\\s-*")
            (concat comment-indent " * "))
           (t fill-prefix)))))))

(defun move--compile (sub-command &rest args)
  "Find the Move project root for the current file, and run SUB-COMMAND of the
   Move CLI on it, with MOVE-DEFAULT-ARGUMENTS."
  (let ((default-directory (locate-dominating-file default-directory
                                                   "Move.toml")))
    (compile (concat move-bin " " sub-command " " move-default-arguments
                     " " (string-join args " ")))))

(defun move--register-builtins ()
  "Generate a font-lock MATCHER form for built-in constructs, specified via the
   MOVE-BUILTINS custom variable."
  `(,(regexp-opt move-builtins 'symbols) . font-lock-builtin-face))

(defun move--ppss-inner-paren   () (nth 1 (syntax-ppss)))
(defun move--ppss-in-comment    () (nth 4 (syntax-ppss)))
(defun move--ppss-comment-start () (nth 8 (syntax-ppss)))

(defun move--prev-assignment (bound)
  "Search backwards from the current point until BOUND looking for an `='
   character that isn't in a comment.  Returns `t' on success, with the point
   over the character, and `nil' otherwise with the point at an indeterminate
   position."
  (and (search-backward "=" bound t)
       (or (not (move--ppss-in-comment))
           (move--prev-assignment bound))))

(defun move--next-terminator (bound)
  "Search forwards from the current point until BOUND looking for a `;'
   character that isn't in a comment.  Returns `t' on success, with the point
   over the character, and `nil' otherwise with the point at an indeterminate
   position."
  (and (search-forward ";" bound t)
       (or (not (move--ppss-in-comment))
           (move--next-terminator bound)))) 

(defun move--indent-column ()
  "Calculates the column to indent the current line to.  The default indent is
   MOVE-INDENT-OFFSET greater than the indent of the line containing the
   innermost parenthesis at point, or 0 if there is no such innermost paren.

   This column is modified for closing parens, which are dedented by the offset,
   continuation lines of `/*'-style comments, which are indented by 1 to line up
   their `*', and assignment continuation lines, which are indented by a further
   offset."
  (save-excursion
    (back-to-indentation)
    (let* ((current-posn   (point))
           (parent-paren   (move--ppss-inner-paren))
           (default-indent (if (not parent-paren) 0
                             (save-excursion
                               (goto-char parent-paren)
                               (back-to-indentation)
                               (+ (current-column) move-indent-offset)))))
      (cond
       ;; `/*'-style comment continuation lines
       ((and (move--ppss-in-comment)
             (looking-at "*"))
        (+ default-indent 1))

       ;; Top-level items will remain completely unindented.
       ((= default-indent 0) 0)

       ;; Closing parentheses
       ((looking-at "[]})]")
        (- default-indent move-indent-offset))

       ;; Assignment continuation lines
       ((save-excursion
          (and parent-paren
               (save-excursion (goto-char parent-paren)
                               (looking-at "{"))
               (move--prev-assignment parent-paren)
               (not (move--next-terminator current-posn))))
        (+ default-indent move-indent-offset))

       (t default-indent)))))

(provide 'move-mode)

;;; move-mode.el ends here
