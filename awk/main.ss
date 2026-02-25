;; -*- Gerbil -*-
;;;; AWK Main Entry Point
;;
;; CLI handling, compilation, and execution.

(import ./lexer ./parser ./ast ./value ./runtime ./fields
        ./builtins/string ./builtins/math ./builtins/io
        :std/getopt :std/sugar :std/format)
(export main)

;;; CLI Definition

(def awk-cli
  (getopt
   (flag 'posix #\P "--posix" help: "POSIX mode")
   (flag 'traditional #\c "--traditional" help: "Traditional AWK mode")
   (flag 'lint #\L "--lint" help: "Enable lint warnings")
   (flag 'sandbox #\S "--sandbox" help: "Sandbox mode (no system(), etc.)")
   (option 'field-separator #\F "--field-separator" default: #f
           help: "Field separator")
   (option 'assign #\v "--assign" default: #f
           help: "Variable assignment (var=value)")
   (option 'file #\f "--file" default: #f
           help: "Read program from file")
   (option 'source #\e "--source" default: #f
           help: "Program source text")
   (rest-arguments 'files help: "Input files")))

;;; Main entry point

(def (main args)
  (try
    (let-values (((opt cmd-args) (getopt-parse awk-cli args)))
      (run-awk opt cmd-args))
    (catch (getopt-error? e)
      (displayln (getopt-error-message e))
      (exit 1))
    (catch (e)
      (display-exception e (current-error-port))
      (exit 1))))

(def (run-awk opt cmd-args)
  (let* ((program-source (get-program-source opt cmd-args))
         (prog (parse-awk-string program-source))
         (rt (make-awk-runtime-initial))
         (fs-opt (hash-get opt 'field-separator))
         (assign-opt (hash-get opt 'assign)))
    ;; Handle -F option
    (when fs-opt
      (awk-set-var! rt 'FS (make-awk-string fs-opt)))
    ;; Handle -v assignments
    (when assign-opt
      (let ((parts (string-split assign-opt #\=)))
        (when (= (length parts) 2)
          (awk-set-var! rt (string->symbol (car parts))
                        (make-awk-string (cadr parts))))))
    ;; Initialize environment
    (awk-init-environ! rt)
    ;; Compile program
    (let ((compiled (compile-program prog rt)))
      ;; Run BEGIN blocks
      (run-begin-blocks compiled rt)
      ;; Process input files
      (let ((files (hash-get opt 'files)))
        (if (or (null? files) (equal? files '("-")))
          (process-input compiled rt (current-input-port) "-")
          (for-each
           (lambda (file)
             (if (string-contains file #\=)
               ;; Variable assignment
               (let ((parts (string-split file #\=)))
                 (when (= (length parts) 2)
                   (awk-set-var! rt (string->symbol (car parts))
                                 (make-awk-string (cadr parts)))))
               ;; Input file
               (call-with-input-file file
                 (lambda (port)
                   (process-input compiled rt port file)))))
           files)))
      ;; Run END blocks
      (run-end-blocks compiled rt)
      ;; Return exit code
      (when (awk-runtime-exit-code rt)
        (exit (awk-runtime-exit-code rt))))))

(def (get-program-source opt cmd-args)
  "Get program source from -f file or -e text or first argument"
  (let ((file-opt (hash-get opt 'file))
        (source-opt (hash-get opt 'source)))
    (cond
      (file-opt
       (call-with-input-file file-opt read-all-string))
      (source-opt source-opt)
      ((null? cmd-args)
       (error "no program specified"))
      (else
       (let ((prog (car cmd-args)))
         ;; Remove program from args
         (set! (hash-get opt 'files) (cdr cmd-args))
         prog)))))

(def (read-all-string port)
  (let ((out (open-output-string)))
    (let loop ()
      (let ((line (read-line port)))
        (if (eof-object? line)
          (get-output-string out)
          (begin
            (display line out)
            (newline out)
            (loop)))))))

;;; Compilation

(defstruct compiled-program
  (begin-actions    ; list of thunks
   rules            ; list of compiled-rule
   end-actions      ; list of thunks
   functions)       ; hash-table of compiled functions
  transparent: #t)

(defstruct compiled-rule
  (pattern-fn       ; () -> bool
   action-fn        ; () -> void
   range-state)     ; symbol for range tracking
  transparent: #t)

(def (compile-program prog rt)
  "Compile AWK program to executable closures"
  (let* ((functions (awk-runtime-functions rt))
         (compiled-fns (make-hash-table)))
    ;; First pass: collect function definitions
    (hash-for-each
     (lambda (name func)
       (hash-put! compiled-fns name (compile-function func rt)))
     (awk-program-functions prog))
    ;; Compile rules
    (let ((begin-actions '())
          (end-actions '())
          (compiled-rules '()))
      (for-each
       (lambda (rule)
         (let ((pattern (awk-rule-pattern rule))
               (action (awk-rule-action rule)))
           (cond
             ((awk-pattern-begin? pattern)
              (when action
                (set! begin-actions
                      (cons (compile-action action rt compiled-fns)
                            begin-actions))))
             ((awk-pattern-end? pattern)
              (when action
                (set! end-actions
                      (cons (compile-action action rt compiled-fns)
                            end-actions))))
             (else
              (let ((rule-key (gensym 'rule)))
                (set! compiled-rules
                      (cons (make-compiled-rule
                             (compile-pattern pattern rt compiled-fns rule-key)
                             (compile-action (or action
                                                (make-awk-stmt-block
                                                 (list (make-awk-stmt-print '() #f #f))))
                                            rt compiled-fns)
                             rule-key)
                            compiled-rules)))))))
       (awk-program-rules prog))
      (make-compiled-program
       (reverse begin-actions)
       (reverse compiled-rules)
       (reverse end-actions)
       compiled-fns))))

(def (compile-function func rt compiled-fns)
  "Compile a user-defined function"
  (let ((params (awk-func-params func))
        (body (awk-func-body func)))
    (lambda (args)
      (let ((local-vars (make-hash-table)))
        ;; Bind parameters to arguments
        (for-each
         (lambda (param arg)
           (hash-put! local-vars param arg))
         params (if (< (length args) (length params))
                   (append args (make-list (- (length params) (length args))
                                           (make-awk-uninit)))
                   args))
        ;; Execute body with local scope
        (parameterize ((current-local-vars local-vars))
          (execute-statement body rt compiled-fns))))))

(def current-local-vars (make-parameter #f))

(def (compile-pattern pattern rt compiled-fns rule-key)
  "Compile pattern to predicate function"
  (cond
    ((not pattern) (lambda () #t))
    ((awk-pattern-expr? pattern)
     (let ((expr-fn (compile-expr (awk-pattern-expr-expr pattern) rt compiled-fns)))
       (lambda ()
         (awk->bool (expr-fn)))))
    ((awk-pattern-range? pattern)
     (let ((start-fn (compile-pattern (awk-pattern-range-start pattern) rt compiled-fns rule-key))
           (end-fn (compile-pattern (awk-pattern-range-end pattern) rt compiled-fns rule-key))
           (state-key rule-key))
       (lambda ()
         (let* ((states (awk-runtime-range-states rt))
                (in-range? (hash-ref states state-key #f)))
           (if in-range?
             ;; Check end
             (if (end-fn)
               (begin
                 (hash-put! states state-key #f)
                 #t)
               #t)
             ;; Check start
             (if (start-fn)
               (begin
                 (hash-put! states state-key #t)
                 ;; Check if end matches on same line
                 (if (end-fn)
                   (begin
                     (hash-put! states state-key #f)
                     #t)
                   #t))
               #f))))))
    (else (lambda () #t))))

(def (compile-action action rt compiled-fns)
  "Compile action to executable thunk"
  (lambda ()
    (execute-statement action rt compiled-fns)))

;;; Expression compilation

(def (compile-expr expr rt compiled-fns)
  "Compile expression to function that returns awk-value"
  (cond
    ((awk-expr-number? expr)
     (let ((n (awk-expr-number-value expr)))
       (lambda () (make-awk-number n))))
    ((awk-expr-string? expr)
     (let ((s (awk-expr-string-value expr)))
       (lambda () (make-awk-string s))))
    ((awk-expr-regex? expr)
     (let ((pattern (awk-expr-regex-pattern expr)))
       (lambda ()
         ;; Regex in expression context matches against $0
         (let* ((rx (make-regex pattern (awk-IGNORECASE rt)))
                (line (awk->string (awk-get-field rt 0)))
                (match? (pregexp-match rx line)))
           (make-awk-number (if match? 1 0))))))
    ((awk-expr-var? expr)
     (let ((name (awk-expr-var-name expr)))
       (lambda ()
         (let ((local (current-local-vars)))
           (if (and local (hash-key? local name))
             (hash-ref local name)
             (awk-get-or-create-var rt name))))))
    ((awk-expr-field? expr)
     (let ((index-fn (compile-expr (awk-expr-field-index expr) rt compiled-fns)))
       (lambda ()
         (let ((idx (inexact->exact (awk->number (index-fn)))))
           (awk-get-field rt idx)))))
    ((awk-expr-array-ref? expr)
     (let ((name (awk-expr-array-ref-array expr))
           (subscript-fns (map (lambda (e) (compile-expr e rt compiled-fns))
                              (awk-expr-array-ref-subscripts expr))))
       (lambda ()
         (let ((subscript (string-join (map awk->string (map (lambda (f) (f)) subscript-fns))
                                      (awk-SUBSEP rt))))
           (awk-array-ref rt name subscript)))))
    ((awk-expr-binop? expr)
     (compile-binop expr rt compiled-fns))
    ((awk-expr-unop? expr)
     (compile-unop expr rt compiled-fns))
    ((awk-expr-assign? expr)
     (compile-assign expr rt compiled-fns))
    ((awk-expr-assign-op? expr)
     (compile-assign-op expr rt compiled-fns))
    ((awk-expr-pre-inc? expr)
     (compile-pre-inc expr rt compiled-fns))
    ((awk-expr-pre-dec? expr)
     (compile-pre-dec expr rt compiled-fns))
    ((awk-expr-post-inc? expr)
     (compile-post-inc expr rt compiled-fns))
    ((awk-expr-post-dec? expr)
     (compile-post-dec expr rt compiled-fns))
    ((awk-expr-ternary? expr)
     (compile-ternary expr rt compiled-fns))
    ((awk-expr-concat? expr)
     (compile-concat expr rt compiled-fns))
    ((awk-expr-in? expr)
     (compile-in expr rt compiled-fns))
    ((awk-expr-call? expr)
     (compile-call expr rt compiled-fns))
    (else
     (lambda () (make-awk-uninit)))))

(def (compile-binop expr rt compiled-fns)
  (let ((op (awk-expr-binop-op expr))
        (left-fn (compile-expr (awk-expr-binop-left expr) rt compiled-fns))
        (right-fn (compile-expr (awk-expr-binop-right expr) rt compiled-fns)))
    (case op
      ((+) (lambda () (awk+ (left-fn) (right-fn))))
      ((-) (lambda () (awk- (left-fn) (right-fn))))
      ((*) (lambda () (awk* (left-fn) (right-fn))))
      ((/) (lambda () (awk/ (left-fn) (right-fn))))
      ((%) (lambda () (awk% (left-fn) (right-fn))))
      ((^) (lambda () (awk^ (left-fn) (right-fn))))
      ((<) (lambda () (make-awk-number (if (awk<? (left-fn) (right-fn)) 1 0))))
      ((<=) (lambda () (make-awk-number (if (awk<=? (left-fn) (right-fn)) 1 0))))
      ((>) (lambda () (make-awk-number (if (awk>? (left-fn) (right-fn)) 1 0))))
      ((>=) (lambda () (make-awk-number (if (awk>=? (left-fn) (right-fn)) 1 0))))
      ((==) (lambda () (make-awk-number (if (awk-equal? (left-fn) (right-fn)) 1 0))))
      ((!=) (lambda () (make-awk-number (if (awk!=? (left-fn) (right-fn)) 1 0))))
      ((~)
       (lambda ()
         (let* ((str (awk->string (left-fn)))
                (pattern (awk->string (right-fn)))
                (rx (make-regex pattern (awk-IGNORECASE rt))))
           (make-awk-number (if (pregexp-match rx str) 1 0)))))
      ((!~)
       (lambda ()
         (let* ((str (awk->string (left-fn)))
                (pattern (awk->string (right-fn)))
                (rx (make-regex pattern (awk-IGNORECASE rt))))
           (make-awk-number (if (pregexp-match rx str) 0 1)))))
      ((&&)
       (lambda ()
         (if (awk->bool (left-fn))
           (make-awk-number (if (awk->bool (right-fn)) 1 0))
           (make-awk-number 0))))
      ((||)
       (lambda ()
         (if (awk->bool (left-fn))
           (make-awk-number 1)
           (make-awk-number (if (awk->bool (right-fn)) 1 0)))))
      (else (lambda () (make-awk-number 0))))))

(def (compile-unop expr rt compiled-fns)
  (let ((op (awk-expr-unop-op expr))
        (operand-fn (compile-expr (awk-expr-unop-operand expr) rt compiled-fns)))
    (case op
      ((!) (lambda () (awk-not (operand-fn))))
      ((-) (lambda () (awk-negate (operand-fn))))
      ((+) (lambda () (awk-plus (operand-fn))))
      (else (lambda () (make-awk-number 0))))))

(def (compile-assign expr rt compiled-fns)
  (let ((target (awk-expr-assign-target expr))
        (value-fn (compile-expr (awk-expr-assign-value expr) rt compiled-fns)))
    (cond
      ((awk-expr-var? target)
       (let ((name (awk-expr-var-name target)))
         (lambda ()
           (let ((val (value-fn)))
             (let ((local (current-local-vars)))
               (if local
                 (hash-put! local name val)
                 (awk-set-var! rt name val)))
             val))))
      ((awk-expr-field? target)
       (let ((index-fn (compile-expr (awk-expr-field-index target) rt compiled-fns)))
         (lambda ()
           (let ((val (value-fn))
                 (idx (inexact->exact (awk->number (index-fn)))))
             (awk-set-field-index! rt idx val)
             val))))
      ((awk-expr-array-ref? target)
       (let ((name (awk-expr-array-ref-array target))
             (subscript-fns (map (lambda (e) (compile-expr e rt compiled-fns))
                                (awk-expr-array-ref-subscripts target))))
         (lambda ()
           (let ((val (value-fn))
                 (subscript (string-join (map awk->string (map (lambda (f) (f)) subscript-fns))
                                        (awk-SUBSEP rt))))
             (awk-array-set! rt name subscript val)
             val)))))))

(def (compile-assign-op expr rt compiled-fns)
  (let* ((op (awk-expr-assign-op-op expr))
         (target (awk-expr-assign-op-target expr))
         (value-fn (compile-expr (awk-expr-assign-op-value expr) rt compiled-fns))
         (get-fn (compile-expr target rt compiled-fns)))
    (case op
      ((+=)
       (lambda ()
         (let* ((old (get-fn))
                (new (awk+ old (value-fn))))
           ;; Store new value
           new)))
      ((-=)
       (lambda ()
         (let* ((old (get-fn))
                (new (awk- old (value-fn))))
           new)))
      ((*=)
       (lambda ()
         (let* ((old (get-fn))
                (new (awk* old (value-fn))))
           new)))
      ((/=)
       (lambda ()
         (let* ((old (get-fn))
                (new (awk/ old (value-fn))))
           new)))
      ((%=)
       (lambda ()
         (let* ((old (get-fn))
                (new (awk% old (value-fn))))
           new)))
      ((^=)
       (lambda ()
         (let* ((old (get-fn))
                (new (awk^ old (value-fn))))
           new))))))

(def (compile-pre-inc expr rt compiled-fns)
  (let ((get-fn (compile-expr (awk-expr-pre-inc-target expr) rt compiled-fns)))
    (lambda ()
      (let* ((old (awk->number (get-fn)))
             (new (make-awk-number (+ old 1))))
        new))))

(def (compile-pre-dec expr rt compiled-fns)
  (let ((get-fn (compile-expr (awk-expr-pre-dec-target expr) rt compiled-fns)))
    (lambda ()
      (let* ((old (awk->number (get-fn)))
             (new (make-awk-number (- old 1))))
        new))))

(def (compile-post-inc expr rt compiled-fns)
  (let ((get-fn (compile-expr (awk-expr-post-inc-target expr) rt compiled-fns)))
    (lambda ()
      (let* ((old (get-fn))
             (new (make-awk-number (+ (awk->number old) 1))))
        old))))

(def (compile-post-dec expr rt compiled-fns)
  (let ((get-fn (compile-expr (awk-expr-post-dec-target expr) rt compiled-fns)))
    (lambda ()
      (let* ((old (get-fn))
             (new (make-awk-number (- (awk->number old) 1))))
        old))))

(def (compile-ternary expr rt compiled-fns)
  (let ((cond-fn (compile-expr (awk-expr-ternary-condition expr) rt compiled-fns))
        (then-fn (compile-expr (awk-expr-ternary-then-expr expr) rt compiled-fns))
        (else-fn (compile-expr (awk-expr-ternary-else-expr expr) rt compiled-fns)))
    (lambda ()
      (if (awk->bool (cond-fn))
        (then-fn)
        (else-fn)))))

(def (compile-concat expr rt compiled-fns)
  (let ((left-fn (compile-expr (awk-expr-concat-left expr) rt compiled-fns))
        (right-fn (compile-expr (awk-expr-concat-right expr) rt compiled-fns)))
    (lambda ()
      (awk-concat (left-fn) (right-fn)))))

(def (compile-in expr rt compiled-fns)
  (let ((subscript-fns (map (lambda (e) (compile-expr e rt compiled-fns))
                           (awk-expr-in-subscripts expr)))
        (array-name (awk-expr-in-array expr)))
    (lambda ()
      (let ((subscript (string-join (map awk->string (map (lambda (f) (f)) subscript-fns))
                                   (awk-SUBSEP rt))))
        (make-awk-number (if (awk-array-exists? rt array-name subscript) 1 0))))))

(def (compile-call expr rt compiled-fns)
  (let ((name (awk-expr-call-name expr))
        (arg-fns (map (lambda (e) (compile-expr e rt compiled-fns))
                     (awk-expr-call-args expr))))
    (cond
      ;; Built-in functions
      ((eq? name 'length)
       (lambda () (awk-builtin-length rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'substr)
       (lambda () (awk-builtin-substr rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'index)
       (lambda () (awk-builtin-index rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'split)
       (lambda () (awk-builtin-split rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'sub)
       (lambda () (awk-builtin-sub rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'gsub)
       (lambda () (awk-builtin-gsub rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'match)
       (lambda () (awk-builtin-match rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'sprintf)
       (lambda () (awk-builtin-sprintf rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'tolower)
       (lambda () (awk-builtin-tolower rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'toupper)
       (lambda () (awk-builtin-toupper rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'sin)
       (lambda () (awk-builtin-sin rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'cos)
       (lambda () (awk-builtin-cos rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'atan2)
       (lambda () (awk-builtin-atan2 rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'exp)
       (lambda () (awk-builtin-exp rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'log)
       (lambda () (awk-builtin-log rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'sqrt)
       (lambda () (awk-builtin-sqrt rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'int)
       (lambda () (awk-builtin-int rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'rand)
       (lambda () (awk-builtin-rand rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'srand)
       (lambda () (awk-builtin-srand rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'close)
       (lambda () (awk-builtin-close rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'system)
       (lambda () (awk-builtin-system rt (map (lambda (f) (f)) arg-fns))))
      ((eq? name 'fflush)
       (lambda () (awk-builtin-fflush rt (map (lambda (f) (f)) arg-fns))))
      ;; User-defined function
      ((hash-key? compiled-fns name)
       (lambda ()
         (let ((fn (hash-ref compiled-fns name)))
           (fn (map (lambda (f) (f)) arg-fns)))))
      (else
       (lambda () (error "awk: unknown function" name))))))

;;; Statement execution

(def (execute-statement stmt rt compiled-fns)
  (cond
    ((awk-stmt-block? stmt)
     (for-each
      (lambda (s) (execute-statement s rt compiled-fns))
      (awk-stmt-block-stmts stmt)))
    ((awk-stmt-if? stmt)
     (let ((cond-fn (compile-expr (awk-stmt-if-condition stmt) rt compiled-fns)))
       (if (awk->bool (cond-fn))
         (execute-statement (awk-stmt-if-then-branch stmt) rt compiled-fns)
         (when (awk-stmt-if-else-branch stmt)
           (execute-statement (awk-stmt-if-else-branch stmt) rt compiled-fns)))))
    ((awk-stmt-while? stmt)
     (let ((cond-fn (compile-expr (awk-stmt-while-condition stmt) rt compiled-fns)))
       (try
        (let loop ()
          (when (awk->bool (cond-fn))
            (try
             (execute-statement (awk-stmt-while-body stmt) rt compiled-fns)
             (catch (awk-continue?)
               (void)))
            (loop)))
        (catch (awk-break?)
          (void)))))
    ((awk-stmt-for? stmt)
     (let ((init-fn (and (awk-stmt-for-init stmt)
                        (compile-expr (awk-stmt-for-init stmt) rt compiled-fns)))
           (cond-fn (and (awk-stmt-for-condition stmt)
                        (compile-expr (awk-stmt-for-condition stmt) rt compiled-fns)))
           (update-fn (and (awk-stmt-for-update stmt)
                          (compile-expr (awk-stmt-for-update stmt) rt compiled-fns))))
       (when init-fn (init-fn))
       (try
        (let loop ()
          (when (or (not cond-fn) (awk->bool (cond-fn)))
            (try
             (execute-statement (awk-stmt-for-body stmt) rt compiled-fns)
             (catch (awk-continue?)
               (void)))
            (when update-fn (update-fn))
            (loop)))
        (catch (awk-break?)
          (void)))))
    ((awk-stmt-for-in? stmt)
     (let ((var (awk-stmt-for-in-var stmt))
           (array-name (awk-stmt-for-in-array stmt))
           (body (awk-stmt-for-in-body stmt)))
       (let ((arr (awk-get-array rt array-name #f)))
         (when arr
           (try
            (hash-for-each
             (lambda (key val)
               (awk-set-var! rt (string->symbol var) (make-awk-string key))
               (try
                (execute-statement body rt compiled-fns)
                (catch (awk-continue?)
                  (void))))
             arr)
            (catch (awk-break?)
              (void)))))))
    ((awk-stmt-break? stmt)
     (raise (make-awk-break)))
    ((awk-stmt-continue? stmt)
     (raise (make-awk-continue)))
    ((awk-stmt-next? stmt)
     (raise (make-awk-next)))
    ((awk-stmt-nextfile? stmt)
     (raise (make-awk-nextfile)))
    ((awk-stmt-exit? stmt)
     (let ((code-fn (and (awk-stmt-exit-code stmt)
                        (compile-expr (awk-stmt-exit-code stmt) rt compiled-fns))))
       (set! (awk-runtime-exit-code rt)
             (if code-fn (inexact->exact (awk->number (code-fn))) 0))
       (raise (make-awk-exit (awk-runtime-exit-code rt)))))
    ((awk-stmt-return? stmt)
     (let ((val-fn (and (awk-stmt-return-value stmt)
                       (compile-expr (awk-stmt-return-value stmt) rt compiled-fns))))
       (raise (make-awk-return (and val-fn (val-fn))))))
    ((awk-stmt-delete? stmt)
     (let ((array-name (awk-stmt-delete-array stmt))
           (subscript (awk-stmt-delete-subscript stmt)))
       (if subscript
         (let ((sub-fns (map (lambda (e) (compile-expr e rt compiled-fns)) subscript)))
           (let ((sub (string-join (map awk->string (map (lambda (f) (f)) sub-fns))
                                  (awk-SUBSEP rt))))
             (awk-array-delete! rt array-name sub)))
         (awk-array-delete! rt array-name #f))))
    ((awk-stmt-print? stmt)
     (let ((arg-fns (map (lambda (e) (compile-expr e rt compiled-fns))
                        (awk-stmt-print-args stmt))))
       (let ((vals (map (lambda (f) (f)) arg-fns)))
         (if (null? vals)
           (displayln (awk->string (awk-get-field rt 0)))
           (displayln (string-join (map awk->string vals) (awk-OFS rt)))))))
    ((awk-stmt-printf? stmt)
     (let ((fmt-fn (compile-expr (awk-stmt-printf-format stmt) rt compiled-fns))
           (arg-fns (map (lambda (e) (compile-expr e rt compiled-fns))
                        (awk-stmt-printf-args stmt))))
       (display (awk-printf-format (awk->string (fmt-fn))
                                   (map (lambda (f) (f)) arg-fns)
                                   rt))))
    ((awk-stmt-expr? stmt)
     (let ((expr-fn (compile-expr (awk-stmt-expr-expr stmt) rt compiled-fns)))
       (expr-fn)))))

;;; Execution phases

(def (run-begin-blocks compiled rt)
  (for-each
   (lambda (action) (action))
   (compiled-program-begin-actions compiled)))

(def (run-end-blocks compiled rt)
  (for-each
   (lambda (action) (action))
   (compiled-program-end-actions compiled)))

(def (process-input compiled rt port filename)
  "Process all records from input port"
  (set! (awk-runtime-filename rt) filename)
  (awk-set-var! rt 'FILENAME (make-awk-string filename))
  (let loop ()
    (let ((line (read-line port)))
      (unless (eof-object? line)
        ;; Set $0 and split fields
        (awk-set-$0! rt line)
        ;; Update record counters
        (set! (awk-runtime-nr rt) (+ (awk-runtime-nr rt) 1))
        (set! (awk-runtime-fnr rt) (+ (awk-runtime-fnr rt) 1))
        (awk-set-var! rt 'NR (make-awk-number (awk-runtime-nr rt)))
        (awk-set-var! rt 'FNR (make-awk-number (awk-runtime-fnr rt)))
        (awk-set-var! rt 'NF (make-awk-number (awk-runtime-nf rt)))
        ;; Execute rules
        (try
         (for-each
          (lambda (rule)
            (when ((compiled-rule-pattern-fn rule))
              ((compiled-rule-action-fn rule))))
          (compiled-program-rules compiled))
         (catch (awk-next?)
           (void)))
        (loop)))))
