;; -*- Gerbil -*-
;;;; AWK AST node definitions
;;
;; These structures represent the parsed AWK program.

(export #t)

;;; Program structure

(defstruct awk-program
  (rules       ; list of awk-rule
   functions   ; hash-table of name -> awk-func
   includes    ; list of strings (@include files)
   loads       ; list of strings (@load extensions)
   namespace)  ; current namespace (default "awk")
  transparent: #t)

;;; Rules (pattern-action pairs)

(defstruct awk-rule
  (pattern     ; awk-pattern or #f
   action      ; list of awk-stmt (or #f for print)
   line)       ; source line number
  transparent: #t)

;;; Pattern types

(defstruct awk-pattern-begin () transparent: #t)
(defstruct awk-pattern-end () transparent: #t)
(defstruct awk-pattern-beginfile () transparent: #t)
(defstruct awk-pattern-endfile () transparent: #t)

(defstruct awk-pattern-expr
  (expr)       ; awk-expr
  transparent: #t)

(defstruct awk-pattern-range
  (start       ; awk-pattern
   end)        ; awk-pattern
  transparent: #t)

;;; Statements

(defstruct awk-stmt-block
  (stmts)      ; list of awk-stmt
  transparent: #t)

(defstruct awk-stmt-if
  (condition   ; awk-expr
   then-branch ; awk-stmt
   else-branch) ; awk-stmt or #f
  transparent: #t)

(defstruct awk-stmt-while
  (condition   ; awk-expr
   body)       ; awk-stmt
  transparent: #t)

(defstruct awk-stmt-do-while
  (body        ; awk-stmt
   condition)  ; awk-expr
  transparent: #t)

(defstruct awk-stmt-for
  (init        ; awk-expr or #f
   condition   ; awk-expr or #f
   update      ; awk-expr or #f
   body)       ; awk-stmt
  transparent: #t)

(defstruct awk-stmt-for-in
  (var         ; string (variable name)
   array       ; string (array name)
   body)       ; awk-stmt
  transparent: #t)

(defstruct awk-stmt-switch
  (expr        ; awk-expr
   cases)      ; list of awk-case
  transparent: #t)

(defstruct awk-case
  (value       ; awk-expr or #f for default
   stmts)      ; list of awk-stmt
  transparent: #t)

(defstruct awk-stmt-break () transparent: #t)
(defstruct awk-stmt-continue () transparent: #t)
(defstruct awk-stmt-next () transparent: #t)
(defstruct awk-stmt-nextfile () transparent: #t)

(defstruct awk-stmt-exit
  (code)       ; awk-expr or #f
  transparent: #t)

(defstruct awk-stmt-return
  (value)      ; awk-expr or #f
  transparent: #t)

(defstruct awk-stmt-delete
  (array       ; string (array name)
   subscript)  ; list of awk-expr (for multi-dim) or #f
  transparent: #t)

(defstruct awk-stmt-print
  (args        ; list of awk-expr (empty = print $0)
   redirect    ; awk-redirect or #f
   append?)    ; boolean for >> redirect
  transparent: #t)

(defstruct awk-stmt-printf
  (format      ; awk-expr
   args        ; list of awk-expr
   redirect    ; awk-redirect or #f
   append?)    ; boolean for >> redirect
  transparent: #t)

(defstruct awk-stmt-expr
  (expr)       ; awk-expr
  transparent: #t)

;;; Expressions

(defstruct awk-expr-number
  (value)      ; number
  transparent: #t)

(defstruct awk-expr-string
  (value)      ; string
  transparent: #t)

(defstruct awk-expr-regex
  (pattern)    ; string (regex pattern)
  ignorecase)  ; boolean
  transparent: #t)

(defstruct awk-expr-var
  (name)       ; string
  namespace)   ; string or #f
  transparent: #t)

(defstruct awk-expr-field
  (index)      ; awk-expr
  transparent: #t)

(defstruct awk-expr-array-ref
  (array       ; string (array name)
   subscripts) ; list of awk-expr
  transparent: #t)

(defstruct awk-expr-binop
  (op          ; symbol: + - * / % ^ ~ !~ < <= > >= == != && || ?:
   left        ; awk-expr
   right)      ; awk-expr
  transparent: #t)

(defstruct awk-expr-unop
  (op          ; symbol: ! - +
   operand)    ; awk-expr
  transparent: #t)

(defstruct awk-expr-assign
  (target      ; awk-expr-var or awk-expr-field or awk-expr-array-ref
   value)      ; awk-expr
  transparent: #t)

(defstruct awk-expr-assign-op
  (op          ; symbol: += -= *= /= %= ^=
   target      ; awk-expr
   value)      ; awk-expr
  transparent: #t)

(defstruct awk-expr-pre-inc
  (target)     ; awk-expr-var or awk-expr-field
  transparent: #t)

(defstruct awk-expr-pre-dec
  (target)     ; awk-expr-var or awk-expr-field
  transparent: #t)

(defstruct awk-expr-post-inc
  (target)     ; awk-expr-var or awk-expr-field
  transparent: #t)

(defstruct awk-expr-post-dec
  (target)     ; awk-expr-var or awk-expr-field
  transparent: #t)

(defstruct awk-expr-ternary
  (condition   ; awk-expr
   then-expr   ; awk-expr
   else-expr)  ; awk-expr
  transparent: #t)

(defstruct awk-expr-concat
  (left        ; awk-expr
   right)      ; awk-expr
  transparent: #t)

(defstruct awk-expr-in
  (subscripts  ; list of awk-expr
   array)      ; string (array name)
  transparent: #t)

(defstruct awk-expr-call
  (name        ; string (function name)
   args)       ; list of awk-expr
  transparent: #t)

(defstruct awk-expr-indirect-call
  (var         ; string (variable containing function name)
   args)       ; list of awk-expr
  transparent: #t)

(defstruct awk-expr-getline
  (var         ; string or #f
   file        ; awk-expr or #f
   command     ; awk-expr or #f
   coprocess?) ; boolean for |&
  transparent: #t)

;;; I/O Redirection

(defstruct awk-redirect-file
  (filename    ; awk-expr
   append?)    ; boolean
  transparent: #t)

(defstruct awk-redirect-pipe
  (command     ; awk-expr
   coprocess?) ; boolean for |&
  transparent: #t)

;;; Function definition

(defstruct awk-func
  (name        ; string
   params      ; list of strings
   body        ; awk-stmt-block
   line)       ; source line number
  transparent: #t)

;;; Utility predicates

(def (awk-pattern? x)
  (or (awk-pattern-begin? x)
      (awk-pattern-end? x)
      (awk-pattern-beginfile? x)
      (awk-pattern-endfile? x)
      (awk-pattern-expr? x)
      (awk-pattern-range? x)))

(def (awk-stmt? x)
  (or (awk-stmt-block? x)
      (awk-stmt-if? x)
      (awk-stmt-while? x)
      (awk-stmt-do-while? x)
      (awk-stmt-for? x)
      (awk-stmt-for-in? x)
      (awk-stmt-switch? x)
      (awk-stmt-break? x)
      (awk-stmt-continue? x)
      (awk-stmt-next? x)
      (awk-stmt-nextfile? x)
      (awk-stmt-exit? x)
      (awk-stmt-return? x)
      (awk-stmt-delete? x)
      (awk-stmt-print? x)
      (awk-stmt-printf? x)
      (awk-stmt-expr? x)))

(def (awk-expr? x)
  (or (awk-expr-number? x)
      (awk-expr-string? x)
      (awk-expr-regex? x)
      (awk-expr-var? x)
      (awk-expr-field? x)
      (awk-expr-array-ref? x)
      (awk-expr-binop? x)
      (awk-expr-unop? x)
      (awk-expr-assign? x)
      (awk-expr-assign-op? x)
      (awk-expr-pre-inc? x)
      (awk-expr-pre-dec? x)
      (awk-expr-post-inc? x)
      (awk-expr-post-dec? x)
      (awk-expr-ternary? x)
      (awk-expr-concat? x)
      (awk-expr-in? x)
      (awk-expr-call? x)
      (awk-expr-indirect-call? x)
      (awk-expr-getline? x)))
