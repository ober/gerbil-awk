;; -*- Gerbil -*-
;;;; AWK Parser
;;
;; Recursive descent parser for AWK. Builds AST from lexer tokens.

(import ./lexer ./ast)
(export #t)

;;; Parser state

(defstruct parser-state
  (lexer        ; lexer instance
   functions    ; hash-table of function definitions
   rules        ; list of rules
   includes     ; list of @include files
   loads        ; list of @load extensions
   namespace    ; current namespace
   errors)      ; list of error messages
  transparent: #t)

(def (make-parser-from-string input)
  (make-parser-state
   (make-lexer-from-string input)
   (make-hash-table)
   '()
   '()
   '()
   "awk"
   '()))

;;; Token utilities

(def (parser-peek ps)
  (lexer-peek-token! (parser-state-lexer ps)))

(def (parser-advance! ps)
  (lexer-next-token! (parser-state-lexer ps)))

(def (parser-check ps type)
  (let ((tok (parser-peek ps)))
    (and tok (eq? (tok-type tok) type))))

(def (parser-match! ps type)
  (when (parser-check ps type)
    (parser-advance! ps)))

(def (parser-expect! ps type (msg #f))
  (let ((tok (parser-peek ps)))
    (if (and tok (eq? (tok-type tok) type))
      (parser-advance! ps)
      (let ((actual (if tok (tok-type tok) 'EOF)))
        (error (or msg (format "expected ~a, got ~a at line ~a" type actual (if tok (tok-line tok) 0))))))))

(def (parser-error! ps msg)
  (let ((tok (parser-peek ps)))
    (set! (parser-state-errors ps)
          (cons (list msg (if tok (tok-line tok) 0))
                (parser-state-errors ps)))))

;;; Main parse function

(def (parse-program ps)
  "Parse an AWK program"
  (let loop ()
    (lexer-skip-newlines! (parser-state-lexer ps))
    (unless (parser-check ps 'EOF)
      (parse-item ps)
      (loop)))
  (make-awk-program
   (reverse (parser-state-rules ps))
   (parser-state-functions ps)
   (reverse (parser-state-includes ps))
   (reverse (parser-state-loads ps))
   (parser-state-namespace ps)))

(def (parse-item ps)
  "Parse a top-level item (rule, function, or directive)"
  (let ((tok (parser-peek ps)))
    (cond
      ;; @include, @load, @namespace
      ((parser-check ps 'AT-DIRECTIVE)
       (parse-directive ps))
      ;; function
      ((parser-check ps 'function)
       (parse-function ps))
      ;; rule (pattern-action)
      (else
       (parse-rule ps)))))

(def (parse-directive ps)
  "Parse @include, @load, or @namespace directive"
  (let* ((dir-tok (parser-advance! ps))
         (name (tok-value dir-tok)))
    (cond
      ((eq? name 'include)
       (let ((str-tok (parser-expect! ps 'STRING "@include requires string")))
         (set! (parser-state-includes ps)
               (cons (tok-value str-tok) (parser-state-includes ps)))))
      ((eq? name 'load)
       (let ((str-tok (parser-expect! ps 'STRING "@load requires string")))
         (set! (parser-state-loads ps)
               (cons (tok-value str-tok) (parser-state-loads ps)))))
      ((eq? name 'namespace)
       (let ((str-tok (parser-expect! ps 'STRING "@namespace requires string")))
         (set! (parser-state-namespace ps) (tok-value str-tok))))
      (else
       (parser-error! ps (format "unknown directive: @~a" name))))))

(def (parse-function ps)
  "Parse function definition"
  (let ((line (tok-line (parser-peek ps))))
    (parser-expect! ps 'function)
    (let* ((name-tok (parser-expect! ps 'NAME "function name expected"))
           (name (tok-value name-tok))
           (params (parse-param-list ps)))
      (lexer-skip-newlines! (parser-state-lexer ps))
      (let ((body (parse-statement ps)))
        (hash-put! (parser-state-functions ps) name
                   (make-awk-func name params body line))))))

(def (parse-param-list ps)
  "Parse function parameter list"
  (parser-expect! ps 'LPAREN)
  (let loop ((params '()))
    (lexer-skip-newlines! (parser-state-lexer ps))
    (if (parser-check ps 'RPAREN)
      (begin (parser-advance! ps) (reverse params))
      (let ((param-tok (parser-expect! ps 'NAME "parameter name expected")))
        (let ((params (cons (tok-value param-tok) params)))
          (lexer-skip-newlines! (parser-state-lexer ps))
          (if (parser-check ps 'COMMA)
            (begin (parser-advance! ps) (loop params))
            (begin
              (parser-expect! ps 'RPAREN)
              (reverse params))))))))

(def (parse-rule ps)
  "Parse a rule (pattern-action pair)"
  (let ((line (tok-line (parser-peek ps))))
    (let-values (((pattern action) (parse-rule-parts ps)))
      (set! (parser-state-rules ps)
            (cons (make-awk-rule pattern action line)
                  (parser-state-rules ps))))))

(def (parse-rule-parts ps)
  "Parse pattern and action parts of a rule"
  (let ((tok (parser-peek ps)))
    (cond
      ;; BEGIN, END, BEGINFILE, ENDFILE
      ((parser-check ps 'BEGIN)
       (parser-advance! ps)
       (values (make-awk-pattern-begin)
               (parse-action ps)))
      ((parser-check ps 'END)
       (parser-advance! ps)
       (values (make-awk-pattern-end)
               (parse-action ps)))
      ((parser-check ps 'BEGINFILE)
       (parser-advance! ps)
       (values (make-awk-pattern-beginfile)
               (parse-action ps)))
      ((parser-check ps 'ENDFILE)
       (parser-advance! ps)
       (values (make-awk-pattern-endfile)
               (parse-action ps)))
      ;; Action without pattern
      ((parser-check ps 'LBRACE)
       (values #f (parse-action ps)))
      ;; Pattern-action
      (else
       (let ((pattern (parse-pattern ps)))
         (lexer-skip-newlines! (parser-state-lexer ps))
         (let ((action (parse-action ps)))
           (values pattern action)))))))

(def (parse-pattern ps)
  "Parse a pattern"
  (let ((first (parse-pattern-primary ps)))
    (lexer-skip-newlines! (parser-state-lexer ps))
    (if (parser-check ps 'COMMA)
      (begin
        (parser-advance! ps)
        (lexer-skip-newlines! (parser-state-lexer ps))
        (let ((second (parse-pattern-primary ps)))
          (make-awk-pattern-range first second)))
      first)))

(def (parse-pattern-primary ps)
  "Parse a primary pattern (expr or regex)"
  (if (parser-check ps 'REGEX)
    (let ((regex-tok (parser-advance! ps)))
      (make-awk-pattern-expr
       (make-awk-expr-regex (tok-value regex-tok) #f)))
    (make-awk-pattern-expr (parse-expression ps))))

(def (parse-action ps)
  "Parse an action (block of statements)"
  (if (parser-check ps 'LBRACE)
    (begin
      (parser-advance! ps)
      (let ((stmts (parse-statement-list ps)))
        (parser-expect! ps 'RBRACE)
        (make-awk-stmt-block stmts)))
    #f))

(def (parse-statement-list ps)
  "Parse a list of statements"
  (let loop ((stmts '()))
    (lexer-skip-newlines! (parser-state-lexer ps))
    (if (or (parser-check ps 'RBRACE)
            (parser-check ps 'EOF))
      (reverse stmts)
      (let ((stmt (parse-statement ps)))
        (loop (cons stmt stmts))))))

(def (parse-statement ps)
  "Parse a single statement"
  (lexer-skip-newlines! (parser-state-lexer ps))
  (let ((tok (parser-peek ps)))
    (cond
      ((parser-check ps 'LBRACE)
       (parse-block ps))
      ((parser-check ps 'if)
       (parse-if ps))
      ((parser-check ps 'while)
       (parse-while ps))
      ((parser-check ps 'do)
       (parse-do-while ps))
      ((parser-check ps 'for)
       (parse-for ps))
      ((parser-check ps 'switch)
       (parse-switch ps))
      ((parser-check ps 'break)
       (parser-advance! ps)
       (make-awk-stmt-break))
      ((parser-check ps 'continue)
       (parser-advance! ps)
       (make-awk-stmt-continue))
      ((parser-check ps 'next)
       (parser-advance! ps)
       (make-awk-stmt-next))
      ((parser-check ps 'nextfile)
       (parser-advance! ps)
       (make-awk-stmt-nextfile))
      ((parser-check ps 'exit)
       (parse-exit ps))
      ((parser-check ps 'return)
       (parse-return ps))
      ((parser-check ps 'delete)
       (parse-delete ps))
      ((parser-check ps 'print)
       (parse-print ps))
      ((parser-check ps 'printf)
       (parse-printf ps))
      ((parser-check ps 'SEMI)
       (parser-advance! ps)
       (make-awk-stmt-block '()))
      (else
       (let ((expr (parse-expression ps)))
         (make-awk-stmt-expr expr))))))

(def (parse-block ps)
  "Parse a block { statements }"
  (parser-expect! ps 'LBRACE)
  (let ((stmts (parse-statement-list ps)))
    (parser-expect! ps 'RBRACE)
    (make-awk-stmt-block stmts)))

(def (parse-if ps)
  "Parse if statement"
  (parser-advance! ps)
  (parser-expect! ps 'LPAREN)
  (let ((condition (parse-expression ps)))
    (parser-expect! ps 'RPAREN)
    (lexer-skip-newlines! (parser-state-lexer ps))
    (let ((then-branch (parse-statement ps)))
      (lexer-skip-newlines! (parser-state-lexer ps))
      (if (parser-check ps 'else)
        (begin
          (parser-advance! ps)
          (lexer-skip-newlines! (parser-state-lexer ps))
          (let ((else-branch (parse-statement ps)))
            (make-awk-stmt-if condition then-branch else-branch)))
        (make-awk-stmt-if condition then-branch #f)))))

(def (parse-while ps)
  "Parse while statement"
  (parser-advance! ps)
  (parser-expect! ps 'LPAREN)
  (let ((condition (parse-expression ps)))
    (parser-expect! ps 'RPAREN)
    (lexer-skip-newlines! (parser-state-lexer ps))
    (let ((body (parse-statement ps)))
      (make-awk-stmt-while condition body))))

(def (parse-do-while ps)
  "Parse do-while statement"
  (parser-advance! ps)
  (lexer-skip-newlines! (parser-state-lexer ps))
  (let ((body (parse-statement ps)))
    (lexer-skip-newlines! (parser-state-lexer ps))
    (parser-expect! ps 'while)
    (parser-expect! ps 'LPAREN)
    (let ((condition (parse-expression ps)))
      (parser-expect! ps 'RPAREN)
      (make-awk-stmt-do-while body condition))))

(def (parse-for ps)
  "Parse for statement"
  (parser-advance! ps)
  (parser-expect! ps 'LPAREN)
  
  ;; Check for for-in
  (if (and (parser-check ps 'NAME)
           (let ((next (lexer-char-at (parser-state-lexer ps) 0)))
             (eq? next #\space)))
      (let loop ((names '()))
        (let ((name-tok (parser-peek ps)))
          (if (and name-tok (eq? (tok-type name-tok) 'NAME))
            (begin
              (parser-advance! ps)
              (let ((names (cons (tok-value name-tok) names)))
                (if (parser-check ps 'COMMA)
                  (begin (parser-advance! ps) (loop names))
                  (begin
                    (parser-expect! ps 'in)
                    (let ((array-tok (parser-expect! ps 'NAME "array name expected")))
                      (parser-expect! ps 'RPAREN)
                      (lexer-skip-newlines! (parser-state-lexer ps))
                      (let ((body (parse-statement ps)))
                        ;; Multi-dimensional: reconstruct as single var
                        (if (= (length names) 1)
                          (make-awk-stmt-for-in (car names) (tok-value array-tok) body)
                          ;; For multi-dim, use first var and handle specially
                          (make-awk-stmt-for-in (car names) (tok-value array-tok) body)))))))))
            (error "expected variable in for-in"))))
      ;; Regular for
      (let ((init (if (parser-check ps 'SEMI) #f (parse-expression ps))))
        (parser-expect! ps 'SEMI)
        (let ((condition (if (parser-check ps 'SEMI) #f (parse-expression ps))))
          (parser-expect! ps 'SEMI)
          (let ((update (if (parser-check ps 'RPAREN) #f (parse-expression ps))))
            (parser-expect! ps 'RPAREN)
            (lexer-skip-newlines! (parser-state-lexer ps))
            (let ((body (parse-statement ps)))
              (make-awk-stmt-for init condition update body)))))))

(def (parse-switch ps)
  "Parse switch statement"
  (parser-advance! ps)
  (parser-expect! ps 'LPAREN)
  (let ((expr (parse-expression ps)))
    (parser-expect! ps 'RPAREN)
    (lexer-skip-newlines! (parser-state-lexer ps))
    (parser-expect! ps 'LBRACE)
    (let ((cases (parse-case-list ps)))
      (parser-expect! ps 'RBRACE)
      (make-awk-stmt-switch expr cases))))

(def (parse-case-list ps)
  "Parse list of cases"
  (let loop ((cases '()))
    (lexer-skip-newlines! (parser-state-lexer ps))
    (if (parser-check ps 'RBRACE)
      (reverse cases)
      (let ((case (parse-case ps)))
        (loop (cons case cases))))))

(def (parse-case ps)
  "Parse a case clause"
  (cond
    ((parser-check ps 'case)
     (parser-advance! ps)
     (let ((value (parse-expression ps)))
       (parser-expect! ps 'COLON)
       (let ((stmts (parse-statement-list-until-case ps)))
         (make-awk-case value stmts))))
    ((parser-check ps 'default)
     (parser-advance! ps)
     (parser-expect! ps 'COLON)
     (let ((stmts (parse-statement-list-until-case ps)))
       (make-awk-case #f stmts)))
    (else
     (error "expected case or default"))))

(def (parse-statement-list-until-case ps)
  "Parse statements until next case/default/}"
  (let loop ((stmts '()))
    (lexer-skip-newlines! (parser-state-lexer ps))
    (if (or (parser-check ps 'case)
            (parser-check ps 'default)
            (parser-check ps 'RBRACE))
      (reverse stmts)
      (let ((stmt (parse-statement ps)))
        (loop (cons stmt stmts))))))

(def (parse-exit ps)
  "Parse exit statement"
  (parser-advance! ps)
  (lexer-skip-newlines! (parser-state-lexer ps))
  (if (or (parser-check ps 'NEWLINE)
          (parser-check ps 'SEMI)
          (parser-check ps 'RBRACE))
    (make-awk-stmt-exit #f)
    (make-awk-stmt-exit (parse-expression ps))))

(def (parse-return ps)
  "Parse return statement"
  (parser-advance! ps)
  (lexer-skip-newlines! (parser-state-lexer ps))
  (if (or (parser-check ps 'NEWLINE)
          (parser-check ps 'SEMI)
          (parser-check ps 'RBRACE))
    (make-awk-stmt-return #f)
    (make-awk-stmt-return (parse-expression ps))))

(def (parse-delete ps)
  "Parse delete statement"
  (parser-advance! ps)
  (let ((array-tok (parser-expect! ps 'NAME "array name expected")))
    (if (parser-check ps 'LBRACKET)
      (begin
        (parser-advance! ps)
        (let ((subscripts (parse-expression-list ps)))
          (parser-expect! ps 'RBRACKET)
          (make-awk-stmt-delete (tok-value array-tok) subscripts)))
      (make-awk-stmt-delete (tok-value array-tok) #f))))

(def (parse-print ps)
  "Parse print statement"
  (parser-advance! ps)
  (let-values (((args redirect append?) (parse-print-args ps)))
    (make-awk-stmt-print args redirect append?)))

(def (parse-printf ps)
  "Parse printf statement"
  (parser-advance! ps)
  (let* ((format (parse-expression ps))
         (_ (lexer-skip-newlines! (parser-state-lexer ps)))
         (args (if (parser-check ps 'COMMA)
                 (begin
                   (parser-advance! ps)
                   (parse-expression-list ps))
                 '())))
    (let-values (((_ redirect append?) (parse-redirect ps)))
      (make-awk-stmt-printf format args redirect append?))))

(def (parse-print-args ps)
  "Parse arguments and redirection for print"
  (let loop ((args '()))
    (lexer-skip-newlines! (parser-state-lexer ps))
    (cond
      ((parser-check ps 'COMMA)
       (parser-advance! ps)
       (loop (cons (parse-expression ps) args)))
      ((or (parser-check ps 'GT)
            (parser-check ps 'PIPE)
            (parser-check ps 'PIPEAMP)
            (parser-check ps 'RSHIFT))
       (let-values (((redirect append?) (parse-redirect ps)))
         (values (reverse args) redirect append?)))
      ((or (parser-check ps 'NEWLINE)
            (parser-check ps 'SEMI)
            (parser-check ps 'RBRACE)
            (parser-check ps 'EOF))
       (values (reverse args) #f #f))
      ((null? args)
       ;; First arg without comma
       (let ((expr (parse-expression ps)))
         (loop (cons expr args))))
      (else
       (values (reverse args) #f #f)))))

(def (parse-redirect ps)
  "Parse output redirection"
  (cond
    ((parser-check ps 'GT)
     (parser-advance! ps)
     (values (make-awk-redirect-file (parse-expression ps) #f) #f))
    ((parser-check ps 'RSHIFT)
     (parser-advance! ps)
     (values (make-awk-redirect-file (parse-expression ps) #t) #t))
    ((parser-check ps 'PIPE)
     (parser-advance! ps)
     (values (make-awk-redirect-pipe (parse-expression ps) #f) #f))
    ((parser-check ps 'PIPEAMP)
     (parser-advance! ps)
     (values (make-awk-redirect-pipe (parse-expression ps) #t) #t))
    (else (values #f #f))))

(def (parse-expression-list ps)
  "Parse comma-separated expression list"
  (let loop ((exprs '()))
    (let ((expr (parse-expression ps)))
      (if (parser-check ps 'COMMA)
        (begin
          (parser-advance! ps)
          (loop (cons expr exprs)))
        (reverse (cons expr exprs))))))

;;; Expression parsing (precedence climbing)

(def (parse-expression ps)
  "Parse an expression (entry point)"
  (parse-assignment ps))

(def (parse-assignment ps)
  "Parse assignment (right-associative, lowest precedence)"
  (let ((left (parse-ternary ps)))
    (if (memq (tok-type (parser-peek ps))
              '(ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ CARETEQ))
      (let ((op-tok (parser-advance! ps)))
        (let ((right (parse-assignment ps))
              (op (case (tok-type op-tok)
                    ((ASSIGN) '=)
                    ((PLUSEQ) '+=)
                    ((MINUSEQ) '-=)
                    ((STAREQ) '*=)
                    ((SLASHEQ) '/=)
                    ((PERCENTEQ) '%=)
                    ((CARETEQ) '^=))))
          (if (eq? op '=)
            (make-awk-expr-assign left right)
            (make-awk-expr-assign-op op left right))))
      left)))

(def (parse-ternary ps)
  "Parse ternary conditional (right-associative)"
  (let ((condition (parse-or ps)))
    (if (parser-check ps 'QUESTION)
      (begin
        (parser-advance! ps)
        (let ((then-expr (parse-expression ps)))
          (parser-expect! ps 'COLON)
          (let ((else-expr (parse-ternary ps)))
            (make-awk-expr-ternary condition then-expr else-expr))))
      condition)))

(def (parse-or ps)
  "Parse logical OR"
  (let loop ((left (parse-and ps)))
    (if (parser-check ps 'OR)
      (begin
        (parser-advance! ps)
        (let ((right (parse-and ps)))
          (loop (make-awk-expr-binop '|| left right))))
      left)))

(def (parse-and ps)
  "Parse logical AND"
  (let loop ((left (parse-in ps)))
    (if (parser-check ps 'AND)
      (begin
        (parser-advance! ps)
        (let ((right (parse-in ps)))
          (loop (make-awk-expr-binop '&& left right))))
      left)))

(def (parse-in ps)
  "Parse (index) in array"
  (let ((left (parse-match ps)))
    (if (parser-check ps 'in)
      (let ((in-tok (parser-advance! ps)))
        (if (awk-expr-array-ref? left)
          (let ((array-tok (parser-expect! ps 'NAME "array name expected")))
            (make-awk-expr-in (awk-expr-array-ref-subscripts left)
                             (tok-value array-tok)))
          (error "'in' requires array subscript")))
      left)))

(def (parse-match ps)
  "Parse regex match operators"
  (let loop ((left (parse-comparison ps)))
    (if (memq (tok-type (parser-peek ps)) '(MATCH NOMATCH))
      (let ((op-tok (parser-advance! ps)))
        (let ((right (parse-comparison ps)))
          (loop (make-awk-expr-binop
                 (if (eq? (tok-type op-tok) 'MATCH) '~ '!~)
                 left right))))
      left)))

(def (parse-comparison ps)
  "Parse comparison operators"
  (let ((left (parse-concat ps)))
    (if (memq (tok-type (parser-peek ps))
              '(LT LE GT GE EQ NE))
      (let ((op-tok (parser-advance! ps)))
        (let ((right (parse-concat ps))
              (op (case (tok-type op-tok)
                    ((LT) '<)
                    ((LE) '<=)
                    ((GT) '>)
                    ((GE) '>=)
                    ((EQ) '==)
                    ((NE) '!=))))
          (make-awk-expr-binop op left right)))
      left)))

(def (parse-concat ps)
  "Parse implicit string concatenation"
  (let loop ((left (parse-additive ps)))
    (let ((tok (parser-peek ps)))
      ;; Concatenation: adjacent value-producing tokens without operator
      (if (and tok
               (memq (tok-type tok)
                     '(NUMBER STRING NAME REGEX TYPED-REGEX
                       LPAREN DOLLAR NOT PLUS MINUS)))
          (let ((right (parse-additive ps)))
            (loop (make-awk-expr-concat left right)))
          left))))

(def (parse-additive ps)
  "Parse addition and subtraction"
  (let loop ((left (parse-multiplicative ps)))
    (if (memq (tok-type (parser-peek ps)) '(PLUS MINUS))
      (let ((op-tok (parser-advance! ps)))
        (let ((right (parse-multiplicative ps)))
          (loop (make-awk-expr-binop
                 (if (eq? (tok-type op-tok) 'PLUS) '+ '-)
                 left right))))
      left)))

(def (parse-multiplicative ps)
  "Parse multiplication, division, modulo"
  (let loop ((left (parse-power ps)))
    (if (memq (tok-type (parser-peek ps)) '(STAR SLASH PERCENT))
      (let ((op-tok (parser-advance! ps)))
        (let ((right (parse-power ps))
              (op (case (tok-type op-tok)
                    ((STAR) '*)
                    ((SLASH) '/)
                    ((PERCENT) '%))))
          (loop (make-awk-expr-binop op left right))))
      left)))

(def (parse-power ps)
  "Parse exponentiation (right-associative)"
  (let ((left (parse-unary ps)))
    (if (memq (tok-type (parser-peek ps)) '(CARET STARSTAR))
      (begin
        (parser-advance! ps)
        (let ((right (parse-power ps)))
          (make-awk-expr-binop '^ left right)))
      left)))

(def (parse-unary ps)
  "Parse unary operators"
  (let ((tok (parser-peek ps)))
    (cond
      ((parser-check ps 'NOT)
       (parser-advance! ps)
       (make-awk-expr-unop '! (parse-unary ps)))
      ((parser-check ps 'MINUS)
       (parser-advance! ps)
       (make-awk-expr-unop '- (parse-unary ps)))
      ((parser-check ps 'PLUS)
       (parser-advance! ps)
       (make-awk-expr-unop '+ (parse-unary ps)))
      ((parser-check ps 'PLUSPLUS)
       (parser-advance! ps)
       (make-awk-expr-pre-inc (parse-postfix ps)))
      ((parser-check ps 'MINUSMINUS)
       (parser-advance! ps)
       (make-awk-expr-pre-dec (parse-postfix ps)))
      (else (parse-postfix ps)))))

(def (parse-postfix ps)
  "Parse postfix operators (++, --)"
  (let ((left (parse-primary ps)))
    (let loop ((expr left))
      (cond
        ((parser-check ps 'PLUSPLUS)
         (parser-advance! ps)
         (loop (make-awk-expr-post-inc expr)))
        ((parser-check ps 'MINUSMINUS)
         (parser-advance! ps)
         (loop (make-awk-expr-post-dec expr)))
        (else expr)))))

(def (parse-primary ps)
  "Parse primary expressions"
  (let ((tok (parser-peek ps)))
    (cond
      ;; Number
      ((parser-check ps 'NUMBER)
       (let ((tok (parser-advance! ps)))
         (make-awk-expr-number (tok-value tok))))
      
      ;; String
      ((parser-check ps 'STRING)
       (let ((tok (parser-advance! ps)))
         (make-awk-expr-string (tok-value tok))))
      
      ;; Regex
      ((parser-check ps 'REGEX)
       (let ((tok (parser-advance! ps)))
         (make-awk-expr-regex (tok-value tok) #f)))
      
      ;; Typed regex @/pattern/
      ((parser-check ps 'TYPED-REGEX)
       (let ((tok (parser-advance! ps)))
         (make-awk-expr-regex (tok-value tok) #f)))
      
      ;; Field reference $
      ((parser-check ps 'DOLLAR)
       (parser-advance! ps)
       (make-awk-expr-field (parse-unary ps)))
      
      ;; Parenthesized expression or (subscript) in array
      ((parser-check ps 'LPAREN)
       (parser-advance! ps)
       (let ((expr (parse-expression ps)))
         (if (parser-check ps 'COMMA)
           ;; Multi-dimensional subscript
           (let ((subscripts (cons expr (parse-subscript-rest ps))))
             (parser-expect! ps 'RPAREN)
             (if (parser-check ps 'in)
               (begin (parser-advance! ps)
                      (let ((array-tok (parser-expect! ps 'NAME)))
                        (make-awk-expr-in subscripts (tok-value array-tok))))
               (error "expected 'in' after subscript")))
           (begin
             (parser-expect! ps 'RPAREN)
             expr))))
      
      ;; Name (variable, function call, or array reference)
      ((parser-check ps 'NAME)
       (let* ((name-tok (parser-advance! ps))
              (name (tok-value name-tok)))
         (cond
           ;; Function call
           ((parser-check ps 'LPAREN)
            (parser-advance! ps)
            (let ((args (if (parser-check ps 'RPAREN)
                          '()
                          (parse-expression-list ps))))
              (parser-expect! ps 'RPAREN)
              (make-awk-expr-call name args)))
           ;; Array reference
           ((parser-check ps 'LBRACKET)
            (parser-advance! ps)
            (let ((subscripts (parse-expression-list ps)))
              (parser-expect! ps 'RBRACKET)
              (make-awk-expr-array-ref name subscripts)))
           ;; Simple variable
           (else
            (make-awk-expr-var name #f)))))
      
      ;; getline
      ((parser-check ps 'getline)
       (parse-getline ps))
      
      (else
       (error "unexpected token in expression" tok)))))

(def (parse-subscript-rest ps)
  "Parse rest of multi-dimensional subscript after first comma"
  (let loop ((subscripts '()))
    (parser-expect! ps 'COMMA)
    (let ((expr (parse-expression ps)))
      (if (parser-check ps 'COMMA)
        (loop (cons expr subscripts))
        (reverse (cons expr subscripts))))))

(def (parse-getline ps)
  "Parse getline statement (7 forms)"
  (parser-advance! ps)
  (let ((var #f)
        (file #f)
        (command #f)
        (coprocess? #f))
    ;; Check for variable
    (when (parser-check ps 'NAME)
      (set! var (tok-value (parser-advance! ps))))
    ;; Check for redirection
    (cond
      ((parser-check ps 'LT)
       (parser-advance! ps)
       (set! file (parse-expression ps)))
      ((parser-check ps 'PIPE)
       (parser-advance! ps)
       (set! command (parse-expression ps)))
      ((parser-check ps 'PIPEAMP)
       (parser-advance! ps)
       (set! command (parse-expression ps))
       (set! coprocess? #t)))
    (make-awk-expr-getline var file command coprocess?)))

;;; Convenience function

(def (parse-awk-string input)
  "Parse AWK program from string"
  (let ((ps (make-parser-from-string input)))
    (parse-program ps)))
