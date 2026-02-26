;; -*- Gerbil -*-
;;;; AWK AST node definitions

(export #t)

;;; Program structure

(defstruct awk-program
  (rules functions)
  transparent: #t)

;;; Rules (pattern-action pairs)

(defstruct awk-rule
  (pattern action)
  transparent: #t)

;;; Pattern types

(defstruct awk-pattern-begin () transparent: #t)
(defstruct awk-pattern-end () transparent: #t)

(defstruct awk-pattern-expr
  (expr)
  transparent: #t)

(defstruct awk-pattern-range
  (start end)
  transparent: #t)

;;; Statements

(defstruct awk-stmt-block
  (stmts)
  transparent: #t)

(defstruct awk-stmt-if
  (condition then-branch else-branch)
  transparent: #t)

(defstruct awk-stmt-while
  (condition body)
  transparent: #t)

(defstruct awk-stmt-do-while
  (body condition)
  transparent: #t)

(defstruct awk-stmt-for
  (init condition update body)
  transparent: #t)

(defstruct awk-stmt-for-in
  (var array body)
  transparent: #t)

(defstruct awk-stmt-break () transparent: #t)
(defstruct awk-stmt-continue () transparent: #t)
(defstruct awk-stmt-next () transparent: #t)
(defstruct awk-stmt-nextfile () transparent: #t)

(defstruct awk-stmt-exit
  (code)
  transparent: #t)

(defstruct awk-stmt-return
  (value)
  transparent: #t)

(defstruct awk-stmt-delete
  (target)    ; awk-expr (var or array-ref)
  transparent: #t)

(defstruct awk-stmt-print
  (args redirect)
  transparent: #t)

(defstruct awk-stmt-printf
  (format args redirect)
  transparent: #t)

(defstruct awk-stmt-expr
  (expr)
  transparent: #t)

;;; Expressions

(defstruct awk-expr-number
  (value)
  transparent: #t)

(defstruct awk-expr-string
  (value)
  transparent: #t)

(defstruct awk-expr-regex
  (pattern)
  transparent: #t)

(defstruct awk-expr-var
  (name)     ; symbol
  transparent: #t)

(defstruct awk-expr-field
  (index)    ; awk-expr
  transparent: #t)

(defstruct awk-expr-array-ref
  (name subscripts)   ; symbol, list of awk-expr
  transparent: #t)

(defstruct awk-expr-binop
  (op left right)
  transparent: #t)

(defstruct awk-expr-unop
  (op operand)
  transparent: #t)

(defstruct awk-expr-assign
  (target value)
  transparent: #t)

(defstruct awk-expr-assign-op
  (op target value)
  transparent: #t)

(defstruct awk-expr-pre-inc  (target) transparent: #t)
(defstruct awk-expr-pre-dec  (target) transparent: #t)
(defstruct awk-expr-post-inc (target) transparent: #t)
(defstruct awk-expr-post-dec (target) transparent: #t)

(defstruct awk-expr-ternary
  (condition then-expr else-expr)
  transparent: #t)

(defstruct awk-expr-concat
  (left right)
  transparent: #t)

(defstruct awk-expr-in
  (subscripts array)   ; list of awk-expr, symbol
  transparent: #t)

(defstruct awk-expr-match
  (expr pattern negate?)
  transparent: #t)

(defstruct awk-expr-call
  (name args)    ; symbol, list of awk-expr
  transparent: #t)

(defstruct awk-expr-getline
  (var source command?)
  transparent: #t)

;;; I/O Redirection

(defstruct awk-redirect
  (type target)   ; symbol (file, append, pipe, pipe-in), awk-expr
  transparent: #t)

;;; Function definition

(defstruct awk-func
  (name params body)
  transparent: #t)
