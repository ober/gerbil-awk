;; -*- Gerbil -*-
;;;; AWK Runtime Environment

(import ./value ./ast :std/srfi/13 :gerbil-pcre/pcre2/pcre2)
(export #t)

;;; Execution state

(defstruct awk-env
  (globals       ; hash-table: symbol -> awk-value
   arrays        ; hash-table: symbol -> hash-table(string -> awk-value)
   functions     ; hash-table: symbol -> awk-func
   fields        ; vector of awk-value ($0, $1, ...)
   nf            ; current NF
   nr            ; total records read
   fnr           ; records in current file
   filename      ; current filename
   input-files   ; hash-table: string -> port (open input files)
   output-files  ; hash-table: string -> port (open output files)
   range-states  ; hash-table: key -> bool (range pattern states)
   exit-code     ; #f or integer
   local-frames) ; list of hash-tables (local variable scopes)
  transparent: #t)

(def (make-initial-env)
  (let ((env (make-awk-env
              (make-hash-table)  ; globals
              (make-hash-table)  ; arrays
              (make-hash-table)  ; functions
              (vector (make-awk-string ""))  ; fields
              0 0 0 ""           ; nf, nr, fnr, filename
              (make-hash-table)  ; input-files
              (make-hash-table)  ; output-files
              (make-hash-table)  ; range-states
              #f                 ; exit-code
              '())))             ; local-frames
    ;; Initialize predefined variables
    (env-set! env 'FS (make-awk-string " "))
    (env-set! env 'RS (make-awk-string "\n"))
    (env-set! env 'OFS (make-awk-string " "))
    (env-set! env 'ORS (make-awk-string "\n"))
    (env-set! env 'OFMT (make-awk-string "%.6g"))
    (env-set! env 'CONVFMT (make-awk-string "%.6g"))
    (env-set! env 'SUBSEP (make-awk-string (string (integer->char 28))))
    (env-set! env 'RSTART (make-awk-number 0))
    (env-set! env 'RLENGTH (make-awk-number -1))
    (env-set! env 'NR (make-awk-number 0))
    (env-set! env 'NF (make-awk-number 0))
    (env-set! env 'FNR (make-awk-number 0))
    (env-set! env 'FILENAME (make-awk-string ""))
    env))

;;; Variable access — checks local frames first, then globals

(def (env-get env name)
  "Get variable value (local scope first, then global)"
  (let loop ((frames (awk-env-local-frames env)))
    (if (null? frames)
      ;; Check special variables
      (case name
        ((NR) (make-awk-number (awk-env-nr env)))
        ((NF) (make-awk-number (awk-env-nf env)))
        ((FNR) (make-awk-number (awk-env-fnr env)))
        ((FILENAME) (make-awk-string (awk-env-filename env)))
        (else
         (hash-ref (awk-env-globals env) name (make-awk-uninit))))
      (if (hash-key? (car frames) name)
        (hash-ref (car frames) name)
        (loop (cdr frames))))))

(def (env-set! env name val)
  "Set variable (local scope if exists, otherwise global)"
  (let ((v (->awk val)))
    ;; Check special variables
    (case name
      ((NR) (set! (awk-env-nr env) (inexact->exact (floor (awk->number v)))))
      ((NF) (env-set-nf! env (inexact->exact (floor (awk->number v)))))
      ((FNR) (set! (awk-env-fnr env) (inexact->exact (floor (awk->number v)))))
      ((FS OFS ORS RS OFMT CONVFMT SUBSEP RSTART RLENGTH FILENAME)
       (hash-put! (awk-env-globals env) name v))
      (else
       (let loop ((frames (awk-env-local-frames env)))
         (if (null? frames)
           (hash-put! (awk-env-globals env) name v)
           (if (hash-key? (car frames) name)
             (hash-put! (car frames) name v)
             (loop (cdr frames)))))))))

(def (env-get-str env name)
  (awk->string (env-get env name)))

;;; Array access

(def (env-get-array env name)
  "Get or create array"
  (if (hash-key? (awk-env-arrays env) name)
    (hash-ref (awk-env-arrays env) name)
    (let ((arr (make-hash-table)))
      (hash-put! (awk-env-arrays env) name arr)
      arr)))

(def (env-array-ref env name subscript)
  (let ((arr (env-get-array env name)))
    (hash-ref arr subscript (make-awk-uninit))))

(def (env-array-set! env name subscript val)
  (let ((arr (env-get-array env name)))
    (hash-put! arr subscript (->awk val))))

(def (env-array-exists? env name subscript)
  (and (hash-key? (awk-env-arrays env) name)
       (hash-key? (hash-ref (awk-env-arrays env) name) subscript)))

(def (env-array-delete! env name subscript)
  (when (hash-key? (awk-env-arrays env) name)
    (if subscript
      (hash-remove! (hash-ref (awk-env-arrays env) name) subscript)
      (hash-remove! (awk-env-arrays env) name))))

;;; Field access

(def (env-get-field env idx)
  (let ((fields (awk-env-fields env)))
    (if (and (>= idx 0) (< idx (vector-length fields)))
      (vector-ref fields idx)
      (make-awk-string ""))))

(def (env-set-field! env idx val)
  (let ((v (->awk val))
        (nf (awk-env-nf env)))
    (when (> idx nf)
      (set! (awk-env-nf env) idx))
    ;; Extend fields vector if needed
    (when (>= idx (vector-length (awk-env-fields env)))
      (let ((new-fields (make-vector (+ idx 1) (make-awk-string ""))))
        (vector-copy! new-fields 0 (awk-env-fields env))
        (set! (awk-env-fields env) new-fields)))
    (vector-set! (awk-env-fields env) idx v)
    ;; Rebuild $0
    (when (> idx 0)
      (env-rebuild-record! env))))

(def (env-set-record! env str)
  "Set $0 and split into fields"
  (let ((fields (env-split-fields env str)))
    (let ((nf (length fields))
          (vec (list->vector (cons (make-awk-string str)
                                   (map make-awk-strnum fields)))))
      (set! (awk-env-fields env) vec)
      (set! (awk-env-nf env) nf)
      (hash-put! (awk-env-globals env) 'NF (make-awk-number nf)))))

(def (env-rebuild-record! env)
  "Rebuild $0 from fields"
  (let* ((nf (awk-env-nf env))
         (ofs (env-get-str env 'OFS))
         (parts (let loop ((i 1) (acc '()))
                  (if (> i nf)
                    (reverse acc)
                    (loop (+ i 1)
                          (cons (awk->string (env-get-field env i)) acc))))))
    (vector-set! (awk-env-fields env) 0
                 (make-awk-string (string-join parts ofs)))))

(def (env-set-nf! env new-nf)
  (let ((old-nf (awk-env-nf env)))
    (cond
      ((= new-nf old-nf) (void))
      ((< new-nf old-nf)
       (set! (awk-env-nf env) new-nf)
       (env-rebuild-record! env))
      (else
       ;; Expand
       (let ((new-vec (make-vector (+ new-nf 1) (make-awk-string ""))))
         (vector-copy! new-vec 0 (awk-env-fields env))
         (set! (awk-env-fields env) new-vec)
         (set! (awk-env-nf env) new-nf)
         (env-rebuild-record! env))))))

;;; Field splitting

(def (env-split-fields env str)
  "Split string by FS, return list of field strings (not including $0)"
  (let ((fs (env-get-str env 'FS)))
    (cond
      ;; Default FS=" " — split on runs of whitespace, strip leading/trailing
      ((string=? fs " ")
       (split-on-whitespace str))
      ;; Single char FS
      ((= (string-length fs) 1)
       (split-on-char str (string-ref fs 0)))
      ;; Multi-char FS — treat as regex
      (else
       (split-on-regex str fs)))))

(def (split-on-whitespace str)
  (let* ((trimmed (string-trim-both str))
         (len (string-length trimmed)))
    (if (= len 0)
      '()
      (let loop ((i 0) (start 0) (parts '()) (in-ws? #f))
        (if (>= i len)
          (reverse (cons (substring trimmed start len) parts))
          (let ((c (string-ref trimmed i)))
            (if (char-whitespace? c)
              (if in-ws?
                (loop (+ i 1) start parts #t)
                (loop (+ i 1) (+ i 1)
                      (cons (substring trimmed start i) parts) #t))
              (if in-ws?
                (loop (+ i 1) i parts #f)
                (loop (+ i 1) start parts #f)))))))))

(def (split-on-char str ch)
  (let ((len (string-length str)))
    (let loop ((i 0) (start 0) (parts '()))
      (cond
        ((>= i len)
         (reverse (cons (substring str start len) parts)))
        ((char=? (string-ref str i) ch)
         (loop (+ i 1) (+ i 1) (cons (substring str start i) parts)))
        (else
         (loop (+ i 1) start parts))))))

(def (split-on-regex str pattern)
  (pcre2-split pattern str))

;;; Local scope management

(def (env-push-locals! env params args)
  "Push a new local scope for function call"
  (let ((frame (make-hash-table)))
    ;; Bind params to args, extras get uninit
    (let loop ((ps params) (as args))
      (unless (null? ps)
        (hash-put! frame (car ps)
                   (if (null? as) (make-awk-uninit) (->awk (car as))))
        (loop (cdr ps) (if (null? as) '() (cdr as)))))
    (set! (awk-env-local-frames env)
          (cons frame (awk-env-local-frames env)))))

(def (env-pop-locals! env)
  "Pop local scope"
  (set! (awk-env-local-frames env)
        (cdr (awk-env-local-frames env))))

;;; Control flow signals

(defstruct awk-signal-exit (code) transparent: #t)
(defstruct awk-signal-next () transparent: #t)
(defstruct awk-signal-nextfile () transparent: #t)
(defstruct awk-signal-break () transparent: #t)
(defstruct awk-signal-continue () transparent: #t)
(defstruct awk-signal-return (value) transparent: #t)

;;; Environment initialization

(def (env-init-argv! env args)
  (let ((argc (length args))
        (arr (env-get-array env 'ARGV)))
    (env-set! env 'ARGC (make-awk-number argc))
    (let loop ((i 0) (as args))
      (unless (null? as)
        (hash-put! arr (number->string i) (make-awk-string (car as)))
        (loop (+ i 1) (cdr as))))))

(def (env-init-environ! env)
  (let ((arr (env-get-array env 'ENVIRON)))
    ;; Use Gambit's ##os-environ to get env vars
    (let ((envlist (##os-environ)))
      (when (pair? envlist)
        (for-each
         (lambda (entry)
           (let ((eq-pos (string-contains entry "=")))
             (when eq-pos
               (hash-put! arr
                          (substring entry 0 eq-pos)
                          (make-awk-string (substring entry (+ eq-pos 1) (string-length entry)))))))
         envlist)))))

;;; I/O port management

(def (env-get-output-port env target append?)
  "Get or open an output port for file redirection"
  (let ((key target))
    (if (hash-key? (awk-env-output-files env) key)
      (hash-ref (awk-env-output-files env) key)
      (let ((port (open-output-file [path: target
                                     create: 'maybe
                                     truncate: (not append?)
                                     append: append?])))
        (hash-put! (awk-env-output-files env) key port)
        port))))

(def (env-get-pipe-port env cmd)
  "Get or open a pipe for output"
  (let ((key (string-append "|" cmd)))
    (if (hash-key? (awk-env-output-files env) key)
      (hash-ref (awk-env-output-files env) key)
      (let ((port (open-process (list path: "/bin/sh"
                                      arguments: (list "-c" cmd)
                                      stdin-redirection: #t
                                      stdout-redirection: #f
                                      stderr-redirection: #f))))
        (hash-put! (awk-env-output-files env) key port)
        port))))

(def (env-get-input-port env filename)
  "Get or open an input port"
  (if (hash-key? (awk-env-input-files env) filename)
    (hash-ref (awk-env-input-files env) filename)
    (let ((port (open-input-file filename)))
      (hash-put! (awk-env-input-files env) filename port)
      port)))

(def (env-get-input-pipe-port env cmd)
  "Get or open a pipe for reading from a command"
  (let ((key (string-append "pipe-in|" cmd)))
    (if (hash-key? (awk-env-input-files env) key)
      (hash-ref (awk-env-input-files env) key)
      (let ((port (open-process (list path: "/bin/sh"
                                      arguments: (list "-c" cmd)
                                      stdin-redirection: #f
                                      stdout-redirection: #t
                                      stderr-redirection: #f))))
        (hash-put! (awk-env-input-files env) key port)
        port))))

(def (env-close-all! env)
  "Close all open files and pipes, wait for pipe processes"
  (hash-for-each
   (lambda (k p)
     (with-catch void
       (lambda ()
         (force-output p)
         (close-port p)
         ;; Wait for pipe processes to finish
         (when (string-prefix? "|" k)
           (with-catch void (lambda () (process-status p)))))))
   (awk-env-output-files env))
  (hash-for-each
   (lambda (k p)
     (with-catch void
       (lambda ()
         (close-port p)
         (when (string-prefix? "pipe-in|" k)
           (with-catch void (lambda () (process-status p)))))))
   (awk-env-input-files env))
  (hash-clear! (awk-env-output-files env))
  (hash-clear! (awk-env-input-files env)))

(def (env-close-file! env name)
  "Close a specific file/pipe"
  (cond
    ((hash-key? (awk-env-output-files env) name)
     (close-port (hash-ref (awk-env-output-files env) name))
     (hash-remove! (awk-env-output-files env) name)
     0)
    ((hash-key? (awk-env-input-files env) name)
     (close-port (hash-ref (awk-env-input-files env) name))
     (hash-remove! (awk-env-input-files env) name)
     0)
    ;; Also check pipe key
    ((hash-key? (awk-env-output-files env) (string-append "|" name))
     (close-port (hash-ref (awk-env-output-files env) (string-append "|" name)))
     (hash-remove! (awk-env-output-files env) (string-append "|" name))
     0)
    (else -1)))
