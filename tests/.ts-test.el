;;; .ts-test.el --- local test configuration  -*- lexical-binding: t; -*-

;; This file configures tree-sitter language grammars for testing.
;; It is loaded by the Makefile before running tests.

;;; Code:

(require 'treesit)

;; Provide a minimal julia-ts-mode for testing, since it's not built
;; into Emacs 29.
(unless (fboundp 'julia-ts-mode)
  (define-derived-mode julia-ts-mode prog-mode "Julia"
    "Major mode for editing Julia files, powered by tree-sitter."
    :syntax-table (let ((table (make-syntax-table)))
                    (modify-syntax-entry ?# "<" table)
                    (modify-syntax-entry ?\n ">" table)
                    (modify-syntax-entry ?\" "\"" table)
                    table)
    (when (treesit-available-p)
      (treesit-parser-create 'julia))))

;; Provide a minimal html-ts-mode if not available (not built into
;; Emacs 29.3).
(unless (fboundp 'html-ts-mode)
  (define-derived-mode html-ts-mode html-mode "HTML"
    "Major mode for editing HTML files, powered by tree-sitter."
    (when (treesit-available-p)
      (treesit-parser-create 'html))))

(provide '.ts-test)
;;; .ts-test.el ends here
