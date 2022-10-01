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



;;; Customization

(defgroup move-mode nil
  "Support for Move source code."
  :link '(url-link "https://github.com/move-language/move")
  :group 'languages)

;;;###autoload
(define-derived-mode move-mode prog-mode "Move"
  "Major mode for Move source code."
  :group 'move-mode

  (setq-local font-lock-defaults
              '(move-mode-font-lock-keywords
                nil ;; KEYWORDS-ONLY
                nil ;; CASE-FOLD
                nil ;; SYNTAX-ALIST
                )))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.move\\'" . move-mode))

(defconst move-keywords
  '("abort" "acquires" "as" "break" "const" "continue" "copy" "else" "friend"
    "fun" "has" "if" "invariant" "let" "loop" "module" "move" "native" "public"
    "return" "script" "spec" "struct" "use" "while"))

(defconst move-builtin-types
  '("address" "bool" "u8" "u64" "u128" "vector"))

(defvar move-mode-font-lock-keywords
  `((,(regexp-opt move-keywords 'symbols)      . font-lock-keyword-face)
    (,(regexp-opt move-builtin-types 'symbols) . font-lock-type-face)))

(provide 'move-mode)

;;; move-mode.el ends here
