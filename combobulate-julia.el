;;; combobulate-julia.el --- Julia support for Combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Mickey Petersen

;; Author: Mickey Petersen <mickey@masteringemacs.org>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-manipulation)
(require 'combobulate-interface)
(require 'combobulate-rules)
(require 'combobulate-setup)

(defun combobulate-julia-pretty-print-node-name (node default-name)
  "Pretty print the name of NODE."
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ("function_definition"
       (concat "function "
               (combobulate-node-text
                (combobulate-node-child-by-field node "name"))))
      ("macro_definition"
       (concat "macro "
               (combobulate-node-text
                (combobulate-node-child-by-field node "name"))))
      ("module_definition"
       (concat "module "
               (combobulate-node-text
                (combobulate-node-child-by-field node "name"))))
      ("struct_definition"
       (concat "struct "
               (combobulate-node-text
                (combobulate-node-child node 0))))
      ("abstract_definition"
       (concat "abstract type "
               (combobulate-node-text
                (combobulate-node-child node 0))))
      ("assignment"
       (combobulate-node-text
        (combobulate-node-child node 0)))
      ("identifier" (combobulate-node-text node))
      (_ default-name)))
   40))

(eval-and-compile
  (defvar combobulate-julia-definitions
    '((context-nodes
       '("identifier" "integer_literal" "float_literal"
         "string_literal" "character_literal" "boolean_literal"
         "operator"))
      (envelope-list
       '((:description
          "if ... end"
          :key "i"
          :mark-node t
          :shorthand general-statement
          :name "if-block"
          :template
          ("if " @ (p true "Condition") n>
           r> n>
           "end" >))
         (:description
          "if ... else ... end"
          :key "e"
          :mark-node t
          :shorthand general-statement
          :name "if-else-block"
          :template
          ("if " @ (p true "Condition") n>
           r> n>
           "else" n>
           @ n>
           "end" >))
         (:description
          "for ... end"
          :key "f"
          :mark-node t
          :shorthand general-statement
          :name "for-loop"
          :template
          ("for " @ (p iter "Iterator") " in " (p collection "Collection") n>
           r> n>
           "end" >))
         (:description
          "while ... end"
          :key "w"
          :mark-node t
          :shorthand general-statement
          :name "while-loop"
          :template
          ("while " @ (p true "Condition") n>
           r> n>
           "end" >))
         (:description
          "try ... catch ... end"
          :key "t"
          :mark-node t
          :shorthand general-statement
          :name "try-catch"
          :template
          ("try" n>
           r> n>
           "catch " @ (p e "Exception Variable") n>
           @ n>
           "end" >))
         (:description
          "function ... end"
          :key "d"
          :mark-node t
          :shorthand general-statement
          :name "function-def"
          :template
          ("function " @ (p name "Name") "(" (p args "Arguments") ")" n>
           r> n>
           "end" >))
         (:description
          "let ... end"
          :key "l"
          :mark-node t
          :shorthand general-statement
          :name "let-block"
          :template
          ("let " @ (p bindings "Bindings") n>
           r> n>
           "end" >))))
      (envelope-procedure-shorthand-alist
       '((general-statement
          . ((:activation-nodes
              ((:nodes ((rule "compound_statement") (rule "_statement")
                        (rule "_expression") (rule "source_file"))
                       :has-parent ("compound_statement" "source_file"
                                    "if_statement" "for_statement"
                                    "while_statement" "function_definition"
                                    "try_statement" "let_statement"
                                    "module_definition"))))))))
      (pretty-print-node-name-function #'combobulate-julia-pretty-print-node-name)
      (highlight-queries-default nil)
      (indent-after-edit nil)
      (procedures-edit nil)
      (procedures-sexp nil)
      (plausible-separators '("," "\n" ";"))
      (procedures-defun
       '((:activation-nodes
          ((:nodes ("function_definition" "macro_definition"
                    "struct_definition" "module_definition"
                    "abstract_definition"))))))
      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))
      (procedures-sibling
       '(;; argument lists and tuple elements
         (:activation-nodes
          ((:nodes ((rule "argument_list") (rule "tuple_expression")
                    (rule "vector_expression") (rule "curly_expression"))
                   :has-parent ("argument_list" "tuple_expression"
                                "vector_expression" "curly_expression"
                                "macro_argument_list")))
          :selector (:choose parent :match-children t))
         ;; for bindings
         (:activation-nodes
          ((:nodes ((rule "for_binding"))
                   :has-parent ("for_statement" "for_clause")))
          :selector (:choose parent :match-children (:match-rules ("for_binding"))))
         ;; statements in blocks (source_file, compound_statement, module body, etc.)
         (:activation-nodes
          ((:nodes ((rule "source_file") (rule "compound_statement")
                    (rule "_statement") (rule "_expression") (rule "_definition"))
                   :position at
                   :has-parent ("source_file" "compound_statement")))
          :selector (:choose parent :match-children t))
         ;; if/elseif/else clauses
         (:activation-nodes
          ((:nodes ("if_clause" "elseif_clause" "else_clause")
                   :position at
                   :has-parent ("if_statement")))
          :selector (:choose parent :match-children
                             (:match-rules ("if_clause" "elseif_clause" "else_clause"))))
         ;; catch/finally clauses
         (:activation-nodes
          ((:nodes ("catch_clause" "finally_clause")
                   :position at
                   :has-parent ("try_statement")))
          :selector (:choose parent :match-children
                             (:match-rules ("catch_clause" "finally_clause"))))
         ;; matrix rows
         (:activation-nodes
          ((:nodes ("matrix_row")
                   :has-parent ("matrix_expression")))
          :selector (:choose parent :match-children (:match-rules ("matrix_row"))))
         ;; import paths
         (:activation-nodes
          ((:nodes ((rule "import_path") (rule "selected_import"))
                   :has-parent ("import_statement" "using_statement")))
          :selector (:choose parent :match-children t))))
      (procedures-hierarchy
       '(;; Navigate into blocks / compound statements
         (:activation-nodes
          ((:nodes ("function_definition" "macro_definition"
                    "struct_definition" "module_definition"
                    "for_statement" "while_statement"
                    "if_statement" "try_statement"
                    "let_statement" "do_clause"
                    "quote_statement")
                   :position at))
          :selector (:choose node :match-children t))
         ;; Navigate into compound_statement children
         (:activation-nodes
          ((:nodes ("compound_statement") :position at))
          :selector (:choose node :match-children t))
         ;; General fallback
         (:activation-nodes
          ((:nodes ((all))))
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name julia
 :language julia
 :major-modes (julia-mode julia-ts-mode)
 :custom combobulate-julia-definitions
 :setup-fn combobulate-julia-setup)

(defun combobulate-julia-setup (_))

(provide 'combobulate-julia)
;;; combobulate-julia.el ends here
