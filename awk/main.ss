;; -*- Gerbil -*-
;;;; AWK Main Entry Point

(import ./lexer ./parser ./ast ./value ./runtime
        ./builtins/string ./builtins/math ./builtins/io
        :std/getopt :std/format :std/srfi/13
        :gerbil-pcre/pcre2/pcre2)
(export main run-awk)

;; Buffer for multi-char RS record splitting
(def *record-buffer* '())

;;; CLI

(def (main . args)
  (try
    (run-awk args)
    (catch (awk-signal-exit? e)
      (exit (awk-signal-exit-code e)))
    (catch (e)
      (display-exception e (current-error-port))
      (exit 2))))

(def (run-awk args)
  (let-values (((program-text files var-assigns fs) (parse-args args)))
    (let* ((prog (parse-awk-string program-text))
           (env (make-initial-env)))
      ;; Apply -F
      (when fs (env-set! env 'FS (make-awk-string fs)))
      ;; Apply -v assignments (before BEGIN)
      (for-each (lambda (va) (apply-var-assign! env va)) var-assigns)
      ;; Register user functions
      (hash-for-each
       (lambda (name func) (hash-put! (awk-env-functions env) name func))
       (awk-program-functions prog))
      ;; Initialize ARGV and ENVIRON
      (env-init-argv! env (cons "awk" files))
      (env-init-environ! env)
      ;; Run BEGIN
      (run-rules env prog 'begin)
      ;; Process files
      (if (null? files)
        (process-stream env prog (current-input-port) "-")
        (process-files env prog files))
      ;; Run END
      (run-rules env prog 'end)
      ;; Cleanup
      (env-close-all! env)
      (when (awk-env-exit-code env)
        (exit (awk-env-exit-code env))))))

(def (parse-args args)
  "Parse command-line arguments. Returns (values program-text files var-assigns fs)"
  (let loop ((args args) (prog #f) (files '()) (vars '()) (fs #f) (expect #f))
    (if (null? args)
      (if prog
        (values prog (reverse files) (reverse vars) fs)
        (error "no program text"))
      (let ((arg (car args))
            (rest (cdr args)))
        (case expect
          ((fs) (loop rest prog files vars arg #f))
          ((var) (loop rest prog files (cons arg vars) fs #f))
          ((file)
           (let ((text (read-file-to-string arg)))
             (loop rest (if prog (string-append prog "\n" text) text) files vars fs #f)))
          (else
           (cond
             ((string=? arg "-F")
              (loop rest prog files vars fs 'fs))
             ((and (> (string-length arg) 2) (string=? (substring arg 0 2) "-F"))
              (loop rest prog files vars (substring arg 2 (string-length arg)) #f))
             ((string=? arg "-v")
              (loop rest prog files vars fs 'var))
             ((and (> (string-length arg) 2) (string=? (substring arg 0 2) "-v"))
              (loop rest prog files (cons (substring arg 2 (string-length arg)) vars) fs #f))
             ((string=? arg "-f")
              (loop rest prog files vars fs 'file))
             ((string=? arg "--")
              ;; Rest are files
              (loop '() prog (append (reverse rest) files) vars fs #f))
             ((not prog)
              ;; First non-option arg is program text
              (loop rest arg files vars fs #f))
             (else
              (loop rest prog (cons arg files) vars fs #f)))))))))

(def (apply-var-assign! env str)
  (let ((eq-pos (string-contains str "=")))
    (when eq-pos
      (let ((name (string->symbol (substring str 0 eq-pos)))
            (val (substring str (+ eq-pos 1) (string-length str))))
        (env-set! env name (make-awk-string val))))))

(def (read-file-to-string filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
            (string-join (reverse lines) "\n")
            (loop (cons line lines))))))))

;;; Rule execution

(def (run-rules env prog phase)
  "Run BEGIN or END rules"
  (for-each
   (lambda (rule)
     (let ((pat (awk-rule-pattern rule)))
       (when (case phase
               ((begin) (awk-pattern-begin? pat))
               ((end) (awk-pattern-end? pat))
               (else #f))
         (when (awk-rule-action rule)
           (try
             (exec-stmt env (awk-rule-action rule))
             (catch (awk-signal-exit? e)
               (set! (awk-env-exit-code env) (awk-signal-exit-code e))
               (when (eq? phase 'begin)
                 ;; Still run END blocks
                 (run-rules env prog 'end)
                 (env-close-all! env)
                 (exit (awk-signal-exit-code e)))))))))
   (awk-program-rules prog)))

(def (process-files env prog files)
  (for-each
   (lambda (file)
     ;; Check for var=value assignments in file list
     (if (string-contains file "=")
       (apply-var-assign! env file)
       (call-with-input-file file
         (lambda (port)
           (process-stream env prog port file)))))
   files))

(def (process-stream env prog port filename)
  (set! (awk-env-filename env) filename)
  (env-set! env 'FILENAME (make-awk-string filename))
  (set! (awk-env-fnr env) 0)
  (set! *record-buffer* '())
  (let ((rs (env-get-str env 'RS)))
    (try
      (let loop ()
        (let ((record (read-record port rs)))
          (when record
            ;; Update counters
            (set! (awk-env-nr env) (+ (awk-env-nr env) 1))
            (set! (awk-env-fnr env) (+ (awk-env-fnr env) 1))
            ;; Set $0 and split fields
            (env-set-record! env record)
            ;; Update NR/FNR variables
            (hash-put! (awk-env-globals env) 'NR (make-awk-number (awk-env-nr env)))
            (hash-put! (awk-env-globals env) 'FNR (make-awk-number (awk-env-fnr env)))
            ;; Execute main rules
            (try
              (for-each
               (lambda (rule)
                 (let ((pat (awk-rule-pattern rule)))
                   (when (and (not (awk-pattern-begin? pat))
                              (not (awk-pattern-end? pat)))
                     (when (eval-pattern env pat rule)
                       (let ((action (or (awk-rule-action rule)
                                         (make-awk-stmt-block
                                          (list (make-awk-stmt-print '() #f))))))
                         (exec-stmt env action))))))
               (awk-program-rules prog))
              (catch (awk-signal-next? _) (void)))
            (loop))))
      (catch (awk-signal-exit? e)
        (set! (awk-env-exit-code env) (awk-signal-exit-code e)))
      (catch (awk-signal-nextfile? _) (void)))))

(def (read-record port rs)
  "Read one record from port according to RS"
  (cond
    ((string=? rs "\n")
     ;; Default: line-based
     (let ((line (read-line port)))
       (if (eof-object? line) #f line)))
    ((string=? rs "")
     ;; Paragraph mode: records separated by blank lines
     (let loop ((lines '()) (started? #f))
       (let ((line (read-line port)))
         (cond
           ((eof-object? line)
            (if (null? lines) #f
                (string-join (reverse lines) "\n")))
           ((= (string-length line) 0)
            (if started?
              (string-join (reverse lines) "\n")
              (loop lines #f)))
           (else
            (loop (cons line lines) #t))))))
    ((= (string-length rs) 1)
     ;; Single character RS
     (let ((sep (string-ref rs 0)))
       (let loop ((chars '()))
         (let ((c (read-char port)))
           (cond
             ((eof-object? c)
              (if (null? chars) #f
                  (list->string (reverse chars))))
             ((char=? c sep)
              (list->string (reverse chars)))
             (else
              (loop (cons c chars))))))))
    (else
     ;; Multi-char RS — treat as regex (gawk extension)
     ;; Buffer approach: read all input on first call, split by RS, return one at a time
     (if (pair? *record-buffer*)
       (let ((rec (car *record-buffer*)))
         (set! *record-buffer* (cdr *record-buffer*))
         rec)
       ;; Read all remaining input and split by RS regex
       (let loop ((chars '()))
         (let ((c (read-char port)))
           (if (eof-object? c)
             (if (null? chars) #f
                 (let* ((all (list->string (reverse chars)))
                        (records (pcre2-split rs all))
                        ;; Remove trailing empty record
                        (records (if (and (pair? records)
                                         (string=? (car (last-pair records)) ""))
                                   (reverse (cdr (reverse records)))
                                   records)))
                   (if (null? records) #f
                       (begin
                         (set! *record-buffer* (cdr records))
                         (car records)))))
             (loop (cons c chars)))))))))

;;; Pattern evaluation

(def (eval-pattern env pat rule)
  (cond
    ((not pat) #t)
    ((awk-pattern-expr? pat)
     (awk->bool (eval-expr env (awk-pattern-expr-expr pat))))
    ((awk-pattern-range? pat)
     (let* ((key (eq?-hash rule))
            (states (awk-env-range-states env))
            (in-range? (hash-ref states key #f)))
       (if in-range?
         (begin
           (when (awk->bool (eval-expr env (awk-pattern-expr-expr (awk-pattern-range-end pat))))
             (hash-put! states key #f))
           #t)
         (if (awk->bool (eval-expr env (awk-pattern-expr-expr (awk-pattern-range-start pat))))
           (begin
             (hash-put! states key #t)
             ;; Check if end matches same record
             (when (awk->bool (eval-expr env (awk-pattern-expr-expr (awk-pattern-range-end pat))))
               (hash-put! states key #f))
             #t)
           #f))))
    (else #t)))

;;; Statement execution

(def (exec-stmt env stmt)
  (cond
    ((awk-stmt-block? stmt)
     (for-each (lambda (s) (exec-stmt env s))
               (awk-stmt-block-stmts stmt)))
    ((awk-stmt-if? stmt)
     (if (awk->bool (eval-expr env (awk-stmt-if-condition stmt)))
       (exec-stmt env (awk-stmt-if-then-branch stmt))
       (when (awk-stmt-if-else-branch stmt)
         (exec-stmt env (awk-stmt-if-else-branch stmt)))))
    ((awk-stmt-while? stmt)
     (try
       (let loop ()
         (when (awk->bool (eval-expr env (awk-stmt-while-condition stmt)))
           (try (exec-stmt env (awk-stmt-while-body stmt))
                (catch (awk-signal-continue? _) (void)))
           (loop)))
       (catch (awk-signal-break? _) (void))))
    ((awk-stmt-do-while? stmt)
     (try
       (let loop ()
         (try (exec-stmt env (awk-stmt-do-while-body stmt))
              (catch (awk-signal-continue? _) (void)))
         (when (awk->bool (eval-expr env (awk-stmt-do-while-condition stmt)))
           (loop)))
       (catch (awk-signal-break? _) (void))))
    ((awk-stmt-for? stmt)
     (when (awk-stmt-for-init stmt)
       (eval-expr env (awk-stmt-for-init stmt)))
     (try
       (let loop ()
         (when (or (not (awk-stmt-for-condition stmt))
                   (awk->bool (eval-expr env (awk-stmt-for-condition stmt))))
           (try (exec-stmt env (awk-stmt-for-body stmt))
                (catch (awk-signal-continue? _) (void)))
           (when (awk-stmt-for-update stmt)
             (eval-expr env (awk-stmt-for-update stmt)))
           (loop)))
       (catch (awk-signal-break? _) (void))))
    ((awk-stmt-for-in? stmt)
     (let* ((arr-name (awk-stmt-for-in-array stmt))
            (var-name (awk-stmt-for-in-var stmt))
            (arr (and (hash-key? (awk-env-arrays env) arr-name)
                      (hash-ref (awk-env-arrays env) arr-name))))
       (when arr
         (try
           (hash-for-each
            (lambda (key val)
              (env-set! env var-name (make-awk-string key))
              (try (exec-stmt env (awk-stmt-for-in-body stmt))
                   (catch (awk-signal-continue? _) (void))))
            arr)
           (catch (awk-signal-break? _) (void))))))
    ((awk-stmt-break? stmt) (raise (make-awk-signal-break)))
    ((awk-stmt-continue? stmt) (raise (make-awk-signal-continue)))
    ((awk-stmt-next? stmt) (raise (make-awk-signal-next)))
    ((awk-stmt-nextfile? stmt) (raise (make-awk-signal-nextfile)))
    ((awk-stmt-exit? stmt)
     (let ((code (if (awk-stmt-exit-code stmt)
                   (inexact->exact (floor (awk->number (eval-expr env (awk-stmt-exit-code stmt)))))
                   0)))
       (set! (awk-env-exit-code env) code)
       (raise (make-awk-signal-exit code))))
    ((awk-stmt-return? stmt)
     (let ((val (if (awk-stmt-return-value stmt)
                  (eval-expr env (awk-stmt-return-value stmt))
                  (make-awk-uninit))))
       (raise (make-awk-signal-return val))))
    ((awk-stmt-delete? stmt)
     (exec-delete env (awk-stmt-delete-target stmt)))
    ((awk-stmt-print? stmt)
     (exec-print env stmt))
    ((awk-stmt-printf? stmt)
     (exec-printf env stmt))
    ((awk-stmt-expr? stmt)
     (eval-expr env (awk-stmt-expr-expr stmt)))
    (else (void))))

;;; Delete

(def (exec-delete env target)
  (cond
    ((awk-expr-array-ref? target)
     (let* ((name (awk-expr-array-ref-name target))
            (subs (map (lambda (e) (awk->string (eval-expr env e)))
                       (awk-expr-array-ref-subscripts target)))
            (key (string-join subs (env-get-str env 'SUBSEP))))
       (env-array-delete! env name key)))
    ((awk-expr-var? target)
     (env-array-delete! env (awk-expr-var-name target) #f))
    (else (void))))

;;; Print

(def (exec-print env stmt)
  (let* ((args (map (lambda (e) (eval-expr env e))
                    (awk-stmt-print-args stmt)))
         (redir (awk-stmt-print-redirect stmt))
         (port (if redir (get-redir-output-port env redir) (current-output-port)))
         (ofs (env-get-str env 'OFS))
         (ors (env-get-str env 'ORS))
         (ofmt (env-get-str env 'OFMT)))
    (if (null? args)
      (display (awk->string (env-get-field env 0) ofmt) port)
      (let loop ((args args) (first? #t))
        (unless (null? args)
          (unless first? (display ofs port))
          (display (awk->string (car args) ofmt) port)
          (loop (cdr args) #f))))
    (display ors port)
    (force-output port)))

(def (exec-printf env stmt)
  (let* ((fmt (awk->string (eval-expr env (awk-stmt-printf-format stmt))))
         (args (map (lambda (e) (eval-expr env e))
                    (awk-stmt-printf-args stmt)))
         (redir (awk-stmt-printf-redirect stmt))
         (port (if redir (get-redir-output-port env redir) (current-output-port)))
         (text (awk-sprintf fmt args)))
    (display text port)
    (force-output port)))

(def (get-redir-output-port env redir)
  (let ((type (awk-redirect-type redir))
        (target (awk->string (eval-expr env (awk-redirect-target redir)))))
    (case type
      ((file) (env-get-output-port env target #f))
      ((append) (env-get-output-port env target #t))
      ((pipe) (env-get-pipe-port env target))
      (else (current-output-port)))))

;;; Expression evaluation

(def (eval-expr env expr)
  (cond
    ((awk-expr-number? expr)
     (make-awk-number (awk-expr-number-value expr)))
    ((awk-expr-string? expr)
     (make-awk-string (awk-expr-string-value expr)))
    ((awk-expr-regex? expr)
     ;; Regex in expression context: match against $0
     (let* ((pattern (awk-expr-regex-pattern expr))
            (line (awk->string (env-get-field env 0))))
       (make-awk-number (if (pcre2-search pattern line) 1 0))))
    ((awk-expr-var? expr)
     (env-get env (awk-expr-var-name expr)))
    ((awk-expr-field? expr)
     (let ((idx (inexact->exact (floor (awk->number (eval-expr env (awk-expr-field-index expr)))))))
       (env-get-field env (max 0 idx))))
    ((awk-expr-array-ref? expr)
     (let* ((name (awk-expr-array-ref-name expr))
            (subs (map (lambda (e) (awk->string (eval-expr env e)))
                       (awk-expr-array-ref-subscripts expr)))
            (key (string-join subs (env-get-str env 'SUBSEP))))
       (env-array-ref env name key)))
    ((awk-expr-assign? expr)
     (let ((val (eval-expr env (awk-expr-assign-value expr))))
       (assign-target! env (awk-expr-assign-target expr) val)
       val))
    ((awk-expr-assign-op? expr)
     (let* ((target (awk-expr-assign-op-target expr))
            (old (eval-expr env target))
            (rhs (eval-expr env (awk-expr-assign-op-value expr)))
            (new (case (awk-expr-assign-op-op expr)
                   ((+=) (awk+ old rhs))
                   ((-=) (awk- old rhs))
                   ((*=) (awk* old rhs))
                   ((/=) (awk/ old rhs))
                   ((%=) (awk-mod old rhs))
                   ((^=) (awk-pow old rhs)))))
       (assign-target! env target new)
       new))
    ((awk-expr-pre-inc? expr)
     (let* ((target (awk-expr-pre-inc-target expr))
            (old (awk->number (eval-expr env target)))
            (new (make-awk-number (+ old 1))))
       (assign-target! env target new)
       new))
    ((awk-expr-pre-dec? expr)
     (let* ((target (awk-expr-pre-dec-target expr))
            (old (awk->number (eval-expr env target)))
            (new (make-awk-number (- old 1))))
       (assign-target! env target new)
       new))
    ((awk-expr-post-inc? expr)
     (let* ((target (awk-expr-post-inc-target expr))
            (old (eval-expr env target))
            (old-num (awk->number old))
            (new (make-awk-number (+ old-num 1))))
       (assign-target! env target new)
       (make-awk-number old-num)))
    ((awk-expr-post-dec? expr)
     (let* ((target (awk-expr-post-dec-target expr))
            (old (eval-expr env target))
            (old-num (awk->number old))
            (new (make-awk-number (- old-num 1))))
       (assign-target! env target new)
       (make-awk-number old-num)))
    ((awk-expr-binop? expr)
     (eval-binop env expr))
    ((awk-expr-unop? expr)
     (let ((val (eval-expr env (awk-expr-unop-operand expr))))
       (case (awk-expr-unop-op expr)
         ((!) (awk-not val))
         ((-) (awk-negate val))
         ((+) (awk-plus val)))))
    ((awk-expr-ternary? expr)
     (if (awk->bool (eval-expr env (awk-expr-ternary-condition expr)))
       (eval-expr env (awk-expr-ternary-then-expr expr))
       (eval-expr env (awk-expr-ternary-else-expr expr))))
    ((awk-expr-concat? expr)
     (awk-concat (eval-expr env (awk-expr-concat-left expr))
                 (eval-expr env (awk-expr-concat-right expr))))
    ((awk-expr-in? expr)
     (let* ((subs (map (lambda (e) (awk->string (eval-expr env e)))
                       (awk-expr-in-subscripts expr)))
            (key (string-join subs (env-get-str env 'SUBSEP)))
            (arr-name (awk-expr-in-array expr)))
       (make-awk-number (if (env-array-exists? env arr-name key) 1 0))))
    ((awk-expr-match? expr)
     (let* ((str (awk->string (eval-expr env (awk-expr-match-expr expr))))
            (pat (let ((p (awk-expr-match-pattern expr)))
                   (if (awk-expr-regex? p)
                     (awk-expr-regex-pattern p)
                     (awk->string (eval-expr env p)))))
            (matched? (pcre2-search pat str)))
       (make-awk-number (if (awk-expr-match-negate? expr)
                          (if matched? 0 1)
                          (if matched? 1 0)))))
    ((awk-expr-call? expr)
     (eval-call env expr))
    ((awk-expr-getline? expr)
     (eval-getline env expr))
    (else (make-awk-uninit))))

(def (eval-binop env expr)
  (let ((op (awk-expr-binop-op expr)))
    (case op
      ((&&)
       (make-awk-number
        (if (and (awk->bool (eval-expr env (awk-expr-binop-left expr)))
                 (awk->bool (eval-expr env (awk-expr-binop-right expr))))
          1 0)))
      ((||)
       (make-awk-number
        (if (or (awk->bool (eval-expr env (awk-expr-binop-left expr)))
                (awk->bool (eval-expr env (awk-expr-binop-right expr))))
          1 0)))
      (else
       (let ((left (eval-expr env (awk-expr-binop-left expr)))
             (right (eval-expr env (awk-expr-binop-right expr))))
         (case op
           ((+)  (awk+ left right))
           ((-)  (awk- left right))
           ((*)  (awk* left right))
           ((/)  (awk/ left right))
           ((%)  (awk-mod left right))
           ((^)  (awk-pow left right))
           ((<)  (make-awk-number (if (awk<? left right) 1 0)))
           ((<=) (make-awk-number (if (awk<=? left right) 1 0)))
           ((>)  (make-awk-number (if (awk>? left right) 1 0)))
           ((>=) (make-awk-number (if (awk>=? left right) 1 0)))
           ((==) (make-awk-number (if (awk-equal? left right) 1 0)))
           ((!=) (make-awk-number (if (awk!=? left right) 1 0)))
           (else (make-awk-number 0))))))))

;;; Assignment to lvalues

(def (assign-target! env target val)
  (cond
    ((awk-expr-var? target)
     (let ((name (awk-expr-var-name target)))
       (case name
         ((NF) (env-set-nf! env (inexact->exact (floor (awk->number val)))))
         (else (env-set! env name val)))))
    ((awk-expr-field? target)
     (let ((idx (inexact->exact (floor (awk->number (eval-expr env (awk-expr-field-index target)))))))
       (if (= idx 0)
         (env-set-record! env (awk->string val))
         (env-set-field! env idx val))))
    ((awk-expr-array-ref? target)
     (let* ((name (awk-expr-array-ref-name target))
            (subs (map (lambda (e) (awk->string (eval-expr env e)))
                       (awk-expr-array-ref-subscripts target)))
            (key (string-join subs (env-get-str env 'SUBSEP))))
       (env-array-set! env name key val)))
    (else (void))))

;;; Function calls

(def (eval-call env expr)
  (let ((name (awk-expr-call-name expr))
        (arg-exprs (awk-expr-call-args expr)))
    ;; Check builtins first
    (case name
      ((length)
       ;; length(x) — if x is an array name, return element count
       (if (and (pair? arg-exprs)
                (awk-expr-var? (car arg-exprs))
                (hash-key? (awk-env-arrays env) (awk-expr-var-name (car arg-exprs))))
         (make-awk-number (hash-length (env-get-array env (awk-expr-var-name (car arg-exprs)))))
         (let ((args (map (lambda (e) (eval-expr env e)) arg-exprs)))
           (awk-builtin-length env args))))
      ((substr)
       (let ((args (map (lambda (e) (eval-expr env e)) arg-exprs)))
         (awk-builtin-substr env args)))
      ((index)
       (let ((args (map (lambda (e) (eval-expr env e)) arg-exprs)))
         (awk-builtin-index env args)))
      ((split)
       ;; split needs array name as symbol
       ;; split(str, arr [, fs]) — fs can be string or regex
       (let* ((str (eval-expr env (car arg-exprs)))
              (arr-name (if (awk-expr-var? (cadr arg-exprs))
                          (awk-expr-var-name (cadr arg-exprs))
                          (string->symbol (awk->string (eval-expr env (cadr arg-exprs))))))
              (fs-arg (if (> (length arg-exprs) 2)
                        (let ((fs-expr (caddr arg-exprs)))
                          ;; Regex literal: extract pattern string
                          (if (awk-expr-regex? fs-expr)
                            (make-awk-string (awk-expr-regex-pattern fs-expr))
                            (eval-expr env fs-expr)))
                        #f))
              (args (if fs-arg (list str arr-name fs-arg) (list str arr-name))))
         (awk-builtin-split env args)))
      ((sub)
       ;; sub(ere, repl [, target]) — target is lvalue
       (let* ((ere (let ((e (car arg-exprs)))
                     (if (awk-expr-regex? e) e (eval-expr env e))))
              (repl (awk->string (eval-expr env (cadr arg-exprs))))
              (target-expr (if (> (length arg-exprs) 2)
                             (caddr arg-exprs)
                             (make-awk-expr-field (make-awk-expr-number 0))))
              (target-val (eval-expr env target-expr)))
         (awk-builtin-sub env arg-exprs ere repl target-val
                          (lambda (v) (assign-target! env target-expr v)))))
      ((gsub)
       (let* ((ere (let ((e (car arg-exprs)))
                     (if (awk-expr-regex? e) e (eval-expr env e))))
              (repl (awk->string (eval-expr env (cadr arg-exprs))))
              (target-expr (if (> (length arg-exprs) 2)
                             (caddr arg-exprs)
                             (make-awk-expr-field (make-awk-expr-number 0))))
              (target-val (eval-expr env target-expr)))
         (awk-builtin-gsub env arg-exprs ere repl target-val
                           (lambda (v) (assign-target! env target-expr v)))))
      ((match)
       (let* ((str (eval-expr env (car arg-exprs)))
              (ere (let ((e (cadr arg-exprs)))
                     (if (awk-expr-regex? e) e (eval-expr env e)))))
         (awk-builtin-match env (list str ere))))
      ((sprintf)
       (let ((args (map (lambda (e) (eval-expr env e)) arg-exprs)))
         (awk-builtin-sprintf env args)))
      ((tolower)
       (let ((args (map (lambda (e) (eval-expr env e)) arg-exprs)))
         (awk-builtin-tolower env args)))
      ((toupper)
       (let ((args (map (lambda (e) (eval-expr env e)) arg-exprs)))
         (awk-builtin-toupper env args)))
      ((sin) (awk-builtin-sin env (map (lambda (e) (eval-expr env e)) arg-exprs)))
      ((cos) (awk-builtin-cos env (map (lambda (e) (eval-expr env e)) arg-exprs)))
      ((atan2) (awk-builtin-atan2 env (map (lambda (e) (eval-expr env e)) arg-exprs)))
      ((exp) (awk-builtin-exp env (map (lambda (e) (eval-expr env e)) arg-exprs)))
      ((log) (awk-builtin-log env (map (lambda (e) (eval-expr env e)) arg-exprs)))
      ((sqrt) (awk-builtin-sqrt env (map (lambda (e) (eval-expr env e)) arg-exprs)))
      ((int) (awk-builtin-int env (map (lambda (e) (eval-expr env e)) arg-exprs)))
      ((rand) (awk-builtin-rand env '()))
      ((srand) (awk-builtin-srand env (map (lambda (e) (eval-expr env e)) arg-exprs)))
      ((close) (awk-builtin-close env (map (lambda (e) (eval-expr env e)) arg-exprs)))
      ((system) (awk-builtin-system env (map (lambda (e) (eval-expr env e)) arg-exprs)))
      ((fflush) (awk-builtin-fflush env (map (lambda (e) (eval-expr env e)) arg-exprs)))
      (else
       ;; User-defined function
       (let ((func (hash-ref (awk-env-functions env) name #f)))
         (if func
           (let ((args (map (lambda (e) (eval-expr env e)) arg-exprs)))
             (env-push-locals! env (awk-func-params func) args)
             (let ((result
                    (try
                      (exec-stmt env (awk-func-body func))
                      (make-awk-uninit)
                      (catch (awk-signal-return? e)
                        (or (awk-signal-return-value e) (make-awk-uninit))))))
               (env-pop-locals! env)
               result))
           (error (string-append "gerbawk: unknown function: " (symbol->string name)))))))))

;;; Getline

(def (eval-getline env expr)
  (let* ((var (awk-expr-getline-var expr))
         (source (awk-expr-getline-source expr))
         (cmd? (awk-expr-getline-command? expr)))
    (cond
      (cmd?
       ;; cmd | getline [var]
       (let* ((cmd (awk->string (eval-expr env source)))
              (port (env-get-input-pipe-port env cmd))
              (line (read-line port)))
         (if (eof-object? line)
           (make-awk-number 0)
           (begin
             (set! (awk-env-nr env) (+ (awk-env-nr env) 1))
             (if var
               (env-set! env var (make-awk-string line))
               (begin
                 (env-set-record! env line)
                 (set! (awk-env-fnr env) (+ (awk-env-fnr env) 1))))
             (make-awk-number 1)))))
      (source
       ;; getline [var] < file
       (let* ((filename (awk->string (eval-expr env source)))
              (port (env-get-input-port env filename))
              (line (read-line port)))
         (if (eof-object? line)
           (make-awk-number 0)
           (begin
             (if var
               (env-set! env var (make-awk-string line))
               (env-set-record! env line))
             (make-awk-number 1)))))
      (else
       ;; Plain getline (from stdin)
       (let ((line (read-line (current-input-port))))
         (if (eof-object? line)
           (make-awk-number 0)
           (begin
             (set! (awk-env-nr env) (+ (awk-env-nr env) 1))
             (if var
               (env-set! env var (make-awk-string line))
               (begin
                 (env-set-record! env line)
                 (set! (awk-env-fnr env) (+ (awk-env-fnr env) 1))))
             (make-awk-number 1))))))))
