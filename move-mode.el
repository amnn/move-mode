;;; move-mode.el --- A major-mode for editing Move language -*- lexical-binding: t; -*-

;; Copyright (c) 2022 Ashok Menon

;; Author: Ashok Menon
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
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

(defcustom move-builtins
  core-move-builtin-functions
  "Functions to highlight as builtins (mutations require restarting font-lock)."
  :type '(list string)
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

;;;###autoload
(define-derived-mode move-mode prog-mode "Move"
  "Major mode for Move source code."
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
  ;; it should be treated like piece of the word preceding word.
  (setq-local syntax-propertize-function
              (syntax-propertize-rules ("\\sw\\(!\\)" (1 "w")))))

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
   MOVE-BUILTINS to enable highlighting, defaults to not."
  )

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

(defun move--register-builtins ()
  "Generate a font-lock MATCHER form for built-in constructs, specified via the
   MOVE-BUILTINS custom variable."
  `(,(regexp-opt move-builtins 'symbols) . font-lock-builtin-face))

(provide 'move-mode)

;;; move-mode.el ends here
