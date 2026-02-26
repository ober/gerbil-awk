;; -*- Gerbil -*-
;;;; AWK Parser — recursive descent

(import ./lexer ./ast)
(export #t)

;;; Parser state

(defstruct pstate
  (lex functions rules)
  transparent: #t)

(def (make-parser input)
  (make-pstate (make-awk-lexer input) (make-hash-table) '()))

;;; Token helpers

(def (peek ps) (lex-peek! (pstate-lex ps)))
(def (advance! ps) (lex-next! (pstate-lex ps)))
(def (skip-nl! ps) (lex-skip-newlines! (pstate-lex ps)))

(def (check? ps type)
  (eq? (tok-type (peek ps)) type))

(def (match! ps type)
  (if (check? ps type) (advance! ps) #f))

(def (expect! ps type)
  (if (check? ps type)
    (advance! ps)
    (let ((t (peek ps)))
      (error (string-append "expected " (symbol->string type)
                            " got " (symbol->string (tok-type t))
                            " at line " (number->string (tok-line t)))))))

(def (term! ps)
  "Consume a statement terminator (newline, semicolon, or peek at } / EOF)"
  (cond
    ((check? ps 'NEWLINE) (advance! ps))
    ((check? ps 'SEMI) (advance! ps))
    ((check? ps 'RBRACE) (void))
    ((check? ps 'EOF) (void))
    (else (void))))

;;; Main parse

(def (parse-awk-string input)
  (let ((ps (make-parser input)))
    (parse-program ps)))

(def (parse-program ps)
  (let loop ()
    (skip-nl! ps)
    (unless (check? ps 'EOF)
      (parse-item ps)
      (loop)))
  (make-awk-program
   (reverse (pstate-rules ps))
   (pstate-functions ps)))

(def (parse-item ps)
  (cond
    ((check? ps 'function) (parse-function ps))
    (else (parse-rule ps))))

;;; Function definition

(def (parse-function ps)
  (advance! ps) ;; skip 'function'
  (let* ((name (tok-value (expect! ps 'NAME)))
         (params (parse-params ps)))
    (skip-nl! ps)
    (let ((body (parse-block ps)))
      (hash-put! (pstate-functions ps) name
                 (make-awk-func name params body)))))

(def (parse-params ps)
  (expect! ps 'LPAREN)
  (let loop ((params '()))
    (skip-nl! ps)
    (cond
      ((check? ps 'RPAREN) (advance! ps) (reverse params))
      (else
       (let ((name (tok-value (expect! ps 'NAME))))
         (skip-nl! ps)
         (if (check? ps 'COMMA)
           (begin (advance! ps) (loop (cons name params)))
           (begin (expect! ps 'RPAREN) (reverse (cons name params)))))))))

;;; Rules

(def (parse-rule ps)
  (let-values (((pat act) (parse-rule-parts ps)))
    (set! (pstate-rules ps)
          (cons (make-awk-rule pat act)
                (pstate-rules ps)))))

(def (parse-rule-parts ps)
  (cond
    ((check? ps 'BEGIN)
     (advance! ps) (skip-nl! ps)
     (values (make-awk-pattern-begin) (parse-action ps)))
    ((check? ps 'END)
     (advance! ps) (skip-nl! ps)
     (values (make-awk-pattern-end) (parse-action ps)))
    ((check? ps 'LBRACE)
     (values #f (parse-action ps)))
    (else
     (let ((pat (parse-pattern ps)))
       (skip-nl! ps)
       (if (check? ps 'LBRACE)
         (values pat (parse-action ps))
         ;; Pattern without action = print $0
         (values pat (make-awk-stmt-block
                      (list (make-awk-stmt-print '() #f)))))))))

(def (parse-pattern ps)
  (let ((first (parse-pattern-primary ps)))
    (skip-nl! ps)
    (if (check? ps 'COMMA)
      (begin (advance! ps) (skip-nl! ps)
        (let ((second (parse-pattern-primary ps)))
          (make-awk-pattern-range first second)))
      first)))

(def (parse-pattern-primary ps)
  (if (check? ps 'REGEX)
    (let ((t (advance! ps)))
      (make-awk-pattern-expr (make-awk-expr-regex (tok-value t))))
    (make-awk-pattern-expr (parse-expr ps))))

(def (parse-action ps)
  (if (check? ps 'LBRACE)
    (parse-block ps)
    #f))

;;; Block and statements

(def (parse-block ps)
  (expect! ps 'LBRACE)
  (let ((stmts (parse-stmt-list ps)))
    (expect! ps 'RBRACE)
    (make-awk-stmt-block stmts)))

(def (parse-stmt-list ps)
  (let loop ((stmts '()))
    (skip-nl! ps)
    (if (or (check? ps 'RBRACE) (check? ps 'EOF))
      (reverse stmts)
      (let ((s (parse-stmt ps)))
        (term! ps)
        (loop (cons s stmts))))))

(def (parse-stmt ps)
  (skip-nl! ps)
  (let ((type (tok-type (peek ps))))
    (case type
      ((LBRACE) (parse-block ps))
      ((if) (parse-if ps))
      ((while) (parse-while ps))
      ((do) (parse-do-while ps))
      ((for) (parse-for ps))
      ((break) (advance! ps) (make-awk-stmt-break))
      ((continue) (advance! ps) (make-awk-stmt-continue))
      ((next) (advance! ps) (make-awk-stmt-next))
      ((nextfile) (advance! ps) (make-awk-stmt-nextfile))
      ((exit) (parse-exit ps))
      ((return) (parse-return ps))
      ((delete) (parse-delete ps))
      ((print) (parse-print ps))
      ((printf) (parse-printf ps))
      ((SEMI) (advance! ps) (make-awk-stmt-block '()))
      (else
       (let ((e (parse-expr ps)))
         (make-awk-stmt-expr e))))))

(def (parse-if ps)
  (advance! ps)
  (expect! ps 'LPAREN)
  (let ((cond (parse-expr ps)))
    (expect! ps 'RPAREN)
    (skip-nl! ps)
    (let ((then (parse-stmt ps)))
      (term! ps)
      (skip-nl! ps)
      (if (check? ps 'else)
        (begin (advance! ps) (skip-nl! ps)
          (make-awk-stmt-if cond then (parse-stmt ps)))
        (make-awk-stmt-if cond then #f)))))

(def (parse-while ps)
  (advance! ps)
  (expect! ps 'LPAREN)
  (let ((cond (parse-expr ps)))
    (expect! ps 'RPAREN)
    (skip-nl! ps)
    (make-awk-stmt-while cond (parse-stmt ps))))

(def (parse-do-while ps)
  (advance! ps) (skip-nl! ps)
  (let ((body (parse-stmt ps)))
    (skip-nl! ps) (term! ps) (skip-nl! ps)
    (expect! ps 'while)
    (expect! ps 'LPAREN)
    (let ((cond (parse-expr ps)))
      (expect! ps 'RPAREN)
      (make-awk-stmt-do-while body cond))))

(def (parse-for ps)
  (advance! ps) ;; skip 'for'
  (expect! ps 'LPAREN)
  (skip-nl! ps)
  ;; Try to detect for-in: for (VAR in ARRAY)
  ;; Look ahead: NAME followed by 'in'
  (if (and (check? ps 'NAME)
           (eq? (tok-type (lex-peek-ahead (pstate-lex ps))) 'in))
    ;; for-in
    (let* ((var (tok-value (advance! ps))))
      (advance! ps) ;; skip 'in'
      (let ((arr (tok-value (expect! ps 'NAME))))
        (expect! ps 'RPAREN)
        (skip-nl! ps)
        (make-awk-stmt-for-in var arr (parse-stmt ps))))
    ;; Regular for
    (let ((init (if (check? ps 'SEMI) #f (parse-expr ps))))
      (expect! ps 'SEMI)
      (let ((cond (if (check? ps 'SEMI) #f (parse-expr ps))))
        (expect! ps 'SEMI)
        (let ((upd (if (check? ps 'RPAREN) #f (parse-expr ps))))
          (expect! ps 'RPAREN)
          (skip-nl! ps)
          (make-awk-stmt-for init cond upd (parse-stmt ps)))))))

(def (lex-peek-ahead lex)
  "Peek at the token AFTER the already-peeked one. Non-destructive."
  ;; Save state, read one token from current pos, restore
  (let* ((saved-peeked (lexer-peeked lex))
         (saved-pos (lexer-pos lex))
         (saved-line (lexer-line lex))
         (saved-col (lexer-column lex))
         (saved-last (lexer-last-type lex)))
    ;; Clear peeked so lex-next! reads fresh from pos
    (set! (lexer-peeked lex) #f)
    ;; pos is already past the peeked token, so next token is what follows it
    (let ((result (lex-next! lex)))
      ;; Restore everything
      (set! (lexer-peeked lex) saved-peeked)
      (set! (lexer-pos lex) saved-pos)
      (set! (lexer-line lex) saved-line)
      (set! (lexer-column lex) saved-col)
      (set! (lexer-last-type lex) saved-last)
      result)))

(def (parse-exit ps)
  (advance! ps)
  (if (or (check? ps 'NEWLINE) (check? ps 'SEMI)
          (check? ps 'RBRACE) (check? ps 'EOF))
    (make-awk-stmt-exit #f)
    (make-awk-stmt-exit (parse-expr ps))))

(def (parse-return ps)
  (advance! ps)
  (if (or (check? ps 'NEWLINE) (check? ps 'SEMI)
          (check? ps 'RBRACE) (check? ps 'EOF))
    (make-awk-stmt-return #f)
    (make-awk-stmt-return (parse-expr ps))))

(def (parse-delete ps)
  (advance! ps) ;; skip 'delete'
  (let ((target (parse-primary ps)))
    (make-awk-stmt-delete target)))

;;; Print / Printf

(def (parse-print ps)
  (advance! ps) ;; skip 'print'
  ;; Parse arguments until redirect or terminator
  (let loop ((args '()) (first? #t))
    (cond
      ;; Terminator
      ((or (check? ps 'NEWLINE) (check? ps 'SEMI)
           (check? ps 'RBRACE) (check? ps 'EOF)
           (check? ps 'PIPE))
       (let ((redir (parse-output-redir ps)))
         (make-awk-stmt-print (reverse args) redir)))
      ;; Redirect
      ((or (check? ps 'GT) (check? ps 'APPEND))
       (let ((redir (parse-output-redir ps)))
         (make-awk-stmt-print (reverse args) redir)))
      ;; Comma between args
      ((and (not first?) (check? ps 'COMMA))
       (advance! ps) (skip-nl! ps)
       (loop (cons (parse-non-assign-expr ps) args) #f))
      ;; First or next arg
      (first?
       (loop (cons (parse-non-assign-expr ps) args) #f))
      (else
       (let ((redir (parse-output-redir ps)))
         (make-awk-stmt-print (reverse args) redir))))))

(def (parse-printf ps)
  (advance! ps) ;; skip 'printf'
  (let ((fmt (parse-non-assign-expr ps)))
    (let loop ((args '()))
      (cond
        ((check? ps 'COMMA)
         (advance! ps) (skip-nl! ps)
         (loop (cons (parse-non-assign-expr ps) args)))
        (else
         (let ((redir (parse-output-redir ps)))
           (make-awk-stmt-printf fmt (reverse args) redir)))))))

(def (parse-output-redir ps)
  "Parse output redirection for print/printf, or return #f"
  (cond
    ((check? ps 'GT)
     (advance! ps)
     (make-awk-redirect 'file (parse-non-assign-expr ps)))
    ((check? ps 'APPEND)
     (advance! ps)
     (make-awk-redirect 'append (parse-non-assign-expr ps)))
    ((check? ps 'PIPE)
     (advance! ps)
     (make-awk-redirect 'pipe (parse-non-assign-expr ps)))
    (else #f)))

;;; Expression parsing — parse-non-assign-expr avoids treating > as comparison
;;; inside print args (it's output redirection there)

(def (parse-non-assign-expr ps)
  "Parse expression but stop at >/>>/| (for print redirection)"
  (parse-ternary-no-redir ps))

(def (parse-ternary-no-redir ps)
  (let ((cond (parse-or-no-redir ps)))
    (if (check? ps 'QUESTION)
      (begin (advance! ps)
        (let ((then (parse-expr ps)))
          (expect! ps 'COLON)
          (let ((els (parse-ternary-no-redir ps)))
            (make-awk-expr-ternary cond then els))))
      cond)))

(def (parse-or-no-redir ps)
  (let loop ((left (parse-and-no-redir ps)))
    (if (check? ps 'OR)
      (begin (advance! ps) (skip-nl! ps)
        (loop (make-awk-expr-binop '|| left (parse-and-no-redir ps))))
      left)))

(def (parse-and-no-redir ps)
  (let loop ((left (parse-in-expr-no-redir ps)))
    (if (check? ps 'AND)
      (begin (advance! ps) (skip-nl! ps)
        (loop (make-awk-expr-binop '&& left (parse-in-expr-no-redir ps))))
      left)))

(def (parse-in-expr-no-redir ps)
  (let ((left (parse-ere-match-no-redir ps)))
    (if (check? ps 'in)
      (begin (advance! ps)
        (let ((arr (tok-value (expect! ps 'NAME))))
          (let ((subs (if (awk-expr-array-ref? left)
                        (awk-expr-array-ref-subscripts left)
                        (list left))))
            (make-awk-expr-in subs arr))))
      left)))

(def (parse-ere-match-no-redir ps)
  (let loop ((left (parse-comparison-no-redir ps)))
    (cond
      ((check? ps 'MATCH)
       (advance! ps)
       (loop (make-awk-expr-match left (parse-comparison-no-redir ps) #f)))
      ((check? ps 'NOMATCH)
       (advance! ps)
       (loop (make-awk-expr-match left (parse-comparison-no-redir ps) #t)))
      (else left))))

(def (parse-comparison-no-redir ps)
  "Like parse-comparison but does NOT consume GT, GE, APPEND, PIPE"
  (let ((left (parse-concat ps)))
    (let ((type (tok-type (peek ps))))
      (if (memq type '(LT LE EQ NE))
        (let* ((t (advance! ps))
               (op (case type
                     ((LT) '<) ((LE) '<=) ((EQ) '==) ((NE) '!=))))
          (make-awk-expr-binop op left (parse-concat ps)))
        left))))

(def (parse-expr ps)
  "Parse full expression including assignment"
  (let ((left (parse-ternary ps)))
    (cond
      ((check? ps 'ASSIGN)
       (advance! ps)
       (make-awk-expr-assign left (parse-expr ps)))
      ((memq (tok-type (peek ps)) '(PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ CARETEQ))
       (let* ((t (advance! ps))
              (op (case (tok-type t)
                    ((PLUSEQ) '+=) ((MINUSEQ) '-=) ((STAREQ) '*=)
                    ((SLASHEQ) '/=) ((PERCENTEQ) '%=) ((CARETEQ) '^=))))
         (make-awk-expr-assign-op op left (parse-expr ps))))
      (else left))))

(def (parse-ternary ps)
  (let ((cond (parse-or ps)))
    (if (check? ps 'QUESTION)
      (begin (advance! ps)
        (let ((then (parse-expr ps)))
          (expect! ps 'COLON)
          (let ((els (parse-ternary ps)))
            (make-awk-expr-ternary cond then els))))
      cond)))

(def (parse-or ps)
  (let loop ((left (parse-and ps)))
    (if (check? ps 'OR)
      (begin (advance! ps) (skip-nl! ps)
        (loop (make-awk-expr-binop '|| left (parse-and ps))))
      left)))

(def (parse-and ps)
  (let loop ((left (parse-in-expr ps)))
    (if (check? ps 'AND)
      (begin (advance! ps) (skip-nl! ps)
        (loop (make-awk-expr-binop '&& left (parse-in-expr ps))))
      left)))

(def (parse-in-expr ps)
  (let ((left (parse-ere-match ps)))
    (if (check? ps 'in)
      (begin (advance! ps)
        (let ((arr (tok-value (expect! ps 'NAME))))
          ;; Convert left into subscript list
          (let ((subs (if (awk-expr-array-ref? left)
                        (awk-expr-array-ref-subscripts left)
                        (list left))))
            (make-awk-expr-in subs arr))))
      left)))

(def (parse-ere-match ps)
  (let loop ((left (parse-comparison ps)))
    (cond
      ((check? ps 'MATCH)
       (advance! ps)
       (loop (make-awk-expr-match left (parse-comparison ps) #f)))
      ((check? ps 'NOMATCH)
       (advance! ps)
       (loop (make-awk-expr-match left (parse-comparison ps) #t)))
      (else left))))

(def (parse-comparison ps)
  (let ((left (parse-concat ps)))
    (let ((type (tok-type (peek ps))))
      (cond
        ((memq type '(LT LE GT GE EQ NE))
         (let* ((t (advance! ps))
                (op (case type
                      ((LT) '<) ((LE) '<=) ((GT) '>) ((GE) '>=)
                      ((EQ) '==) ((NE) '!=))))
           (make-awk-expr-binop op left (parse-concat ps))))
        ;; cmd | getline [var]
        ((and (eq? type 'PIPE) (eq? (tok-type (lex-peek-ahead (pstate-lex ps))) 'getline))
         (advance! ps) ;; skip PIPE
         (advance! ps) ;; skip getline
         (let ((var (if (check? ps 'NAME) (tok-value (advance! ps)) #f)))
           (make-awk-expr-getline var left #t)))
        (else left)))))

(def (parse-concat ps)
  (let loop ((left (parse-addition ps)))
    ;; Concatenation: next token starts a value but is NOT an operator
    (let ((type (tok-type (peek ps))))
      (if (memq type '(NUMBER STRING NAME REGEX LPAREN DOLLAR NOT
                        PLUSPLUS MINUSMINUS MINUS))
        ;; But MINUS could be subtraction — only concat if it's unary context
        ;; Actually in AWK, adjacency IS concatenation. But we must not
        ;; grab things that are part of a different production.
        ;; Safest: only concat for value-starting tokens that aren't binary ops
        (if (memq type '(NUMBER STRING NAME REGEX LPAREN DOLLAR NOT
                          PLUSPLUS MINUSMINUS))
          (loop (make-awk-expr-concat left (parse-addition ps)))
          left)
        left))))

(def (parse-addition ps)
  (let loop ((left (parse-multiplication ps)))
    (let ((type (tok-type (peek ps))))
      (cond
        ((eq? type 'PLUS)
         (advance! ps) (loop (make-awk-expr-binop '+ left (parse-multiplication ps))))
        ((eq? type 'MINUS)
         (advance! ps) (loop (make-awk-expr-binop '- left (parse-multiplication ps))))
        (else left)))))

(def (parse-multiplication ps)
  (let loop ((left (parse-power ps)))
    (let ((type (tok-type (peek ps))))
      (cond
        ((eq? type 'STAR)
         (advance! ps) (loop (make-awk-expr-binop '* left (parse-power ps))))
        ((eq? type 'SLASH)
         (advance! ps) (loop (make-awk-expr-binop '/ left (parse-power ps))))
        ((eq? type 'PERCENT)
         (advance! ps) (loop (make-awk-expr-binop '% left (parse-power ps))))
        (else left)))))

(def (parse-power ps)
  (let ((left (parse-unary ps)))
    (if (or (check? ps 'CARET) (check? ps 'STARSTAR))
      (begin (advance! ps) (make-awk-expr-binop '^ left (parse-power ps)))
      left)))

(def (parse-unary ps)
  (let ((type (tok-type (peek ps))))
    (cond
      ((eq? type 'NOT)
       (advance! ps) (make-awk-expr-unop '! (parse-unary ps)))
      ((eq? type 'MINUS)
       (advance! ps) (make-awk-expr-unop '- (parse-unary ps)))
      ((eq? type 'PLUS)
       (advance! ps) (make-awk-expr-unop '+ (parse-unary ps)))
      ((eq? type 'PLUSPLUS)
       (advance! ps) (make-awk-expr-pre-inc (parse-unary ps)))
      ((eq? type 'MINUSMINUS)
       (advance! ps) (make-awk-expr-pre-dec (parse-unary ps)))
      (else (parse-postfix ps)))))

(def (parse-postfix ps)
  (let ((left (parse-primary ps)))
    (let loop ((e left))
      (cond
        ((check? ps 'PLUSPLUS)  (advance! ps) (loop (make-awk-expr-post-inc e)))
        ((check? ps 'MINUSMINUS) (advance! ps) (loop (make-awk-expr-post-dec e)))
        (else e)))))

(def (parse-primary ps)
  (let ((type (tok-type (peek ps))))
    (case type
      ((NUMBER) (let ((t (advance! ps))) (make-awk-expr-number (tok-value t))))
      ((STRING) (let ((t (advance! ps))) (make-awk-expr-string (tok-value t))))
      ((REGEX) (let ((t (advance! ps))) (make-awk-expr-regex (tok-value t))))
      ((DOLLAR) (advance! ps) (make-awk-expr-field (parse-primary ps)))
      ((LPAREN)
       (advance! ps)
       (let ((e (parse-expr ps)))
         ;; Check for (expr, expr, ...) in array
         (if (check? ps 'COMMA)
           ;; Multi-dim subscript for 'in' expression
           (let loop ((subs (list e)))
             (if (check? ps 'COMMA)
               (begin (advance! ps) (loop (cons (parse-expr ps) subs)))
               (begin (expect! ps 'RPAREN)
                 (if (check? ps 'in)
                   (begin (advance! ps)
                     (let ((arr (tok-value (expect! ps 'NAME))))
                       (make-awk-expr-in (reverse subs) arr)))
                   (error "expected 'in' after (expr,expr)")))))
           (begin (expect! ps 'RPAREN) e))))
      ((NAME)
       (let* ((t (advance! ps))
              (name (tok-value t)))
         (cond
           ;; Function call: NAME(
           ((check? ps 'LPAREN)
            (advance! ps)
            (let ((args (if (check? ps 'RPAREN) '() (parse-expr-list ps))))
              (expect! ps 'RPAREN)
              (make-awk-expr-call name args)))
           ;; Array ref: NAME[
           ((check? ps 'LBRACKET)
            (advance! ps)
            (let ((subs (parse-expr-list ps)))
              (expect! ps 'RBRACKET)
              (make-awk-expr-array-ref name subs)))
           ;; Bare 'length' without parens = length($0)
           ((eq? name 'length)
            (make-awk-expr-call 'length '()))
           ;; Simple variable
           (else (make-awk-expr-var name)))))
      ((getline) (parse-getline-expr ps))
      (else
       (error (string-append "unexpected token: " (symbol->string type)
                             " at line " (number->string (tok-line (peek ps)))))))))

(def (parse-expr-list ps)
  (let loop ((exprs (list (parse-expr ps))))
    (if (check? ps 'COMMA)
      (begin (advance! ps) (skip-nl! ps) (loop (cons (parse-expr ps) exprs)))
      (reverse exprs))))

;;; Getline

(def (parse-getline-expr ps)
  (advance! ps) ;; skip 'getline'
  ;; getline [var] [< file]
  (let ((var #f) (source #f) (cmd? #f))
    (when (check? ps 'NAME)
      (set! var (tok-value (advance! ps))))
    (when (check? ps 'LT)
      (advance! ps)
      (set! source (parse-primary ps)))
    (make-awk-expr-getline var source cmd?)))
