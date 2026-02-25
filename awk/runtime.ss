;; -*- Gerbil -*-
;;;; AWK Runtime Environment
;;
;; Manages execution state, variables, and control flow.

(import ./value ./ast)
(export #t)

;;; Execution state

(defstruct awk-runtime
  (variables    ; hash-table of variable name -> awk-value
   arrays       ; hash-table of array name -> hash-table
   functions    ; hash-table of function name -> awk-func
   fields       ; vector of field values
   nf           ; number of fields
   nr           ; total records read
   fnr          ; records read in current file
   filename     ; current input filename
   argv         ; vector of command-line arguments
   argc         ; argument count
   environ      ; hash-table of environment variables
   input-ports  ; hash-table of open input files/pipes
   output-ports ; hash-table of open output files/pipes
   range-states ; hash-table of range pattern states
   exit-code    ; exit code (or #f)
   next?        ; next flag (set by 'next')
   nextfile?    ; nextfile flag (set by 'nextfile')
   break?       ; break flag
   continue?    ; continue flag
   return-value) ; return value from function
  transparent: #t)

(def (make-awk-runtime-initial)
  "Create initial AWK runtime with predefined variables"
  (let ((rt (make-awk-runtime
             (make-hash-table)    ; variables
             (make-hash-table)    ; arrays
             (make-hash-table)    ; functions
             (vector (make-awk-string "")) ; fields ($0 initially empty)
             0 0 0 ""             ; nf, nr, fnr, filename
             (vector) 0           ; argv, argc
             (make-hash-table)    ; environ
             (make-hash-table)    ; input-ports
             (make-hash-table)    ; output-ports
             (make-hash-table)    ; range-states
             #f #f #f #f #f #f #f)))
    ;; Initialize predefined variables
    (awk-set-var! rt 'FS (make-awk-string " "))
    (awk-set-var! rt 'RS (make-awk-string "\n"))
    (awk-set-var! rt 'OFS (make-awk-string " "))
    (awk-set-var! rt 'ORS (make-awk-string "\n"))
    (awk-set-var! rt 'OFMT (make-awk-string "%.6g"))
    (awk-set-var! rt 'CONVFMT (make-awk-string "%.6g"))
    (awk-set-var! rt 'SUBSEP (make-awk-string "\x1c"))
    (awk-set-var! rt 'RSTART (make-awk-number 0))
    (awk-set-var! rt 'RLENGTH (make-awk-number -1))
    (awk-set-var! rt 'IGNORECASE (make-awk-number 0))
    (awk-set-var! rt 'FILENAME (make-awk-string ""))
    (awk-set-var! rt 'RT (make-awk-string ""))
    rt))

;;; Variable access

(def (awk-get-var rt name (default #f))
  "Get variable value"
  (let ((vars (awk-runtime-variables rt)))
    (hash-ref vars name default)))

(def (awk-set-var! rt name value)
  "Set variable value"
  (let ((vars (awk-runtime-variables rt)))
    (hash-put! vars name (->awk value))))

(def (awk-get-or-create-var rt name)
  "Get or create variable (returns uninitialized if not exists)"
  (let ((vars (awk-runtime-variables rt)))
    (if (hash-key? vars name)
      (hash-ref vars name)
      (let ((val (make-awk-uninit)))
        (hash-put! vars name val)
        val))))

;;; Array access

(def (awk-get-array rt name (create? #t))
  "Get array (create if doesn't exist and create?)"
  (let ((arrays (awk-runtime-arrays rt)))
    (if (hash-key? arrays name)
      (hash-ref arrays name)
      (if create?
        (let ((arr (make-hash-table)))
          (hash-put! arrays name arr)
          arr)
        #f))))

(def (awk-array-ref rt name subscript)
  "Get array element (creates if doesn't exist)"
  (let ((arr (awk-get-array rt name)))
    (if (hash-key? arr subscript)
      (hash-ref arr subscript)
      (let ((val (make-awk-uninit)))
        (hash-put! arr subscript val)
        val))))

(def (awk-array-set! rt name subscript value)
  "Set array element"
  (let ((arr (awk-get-array rt name)))
    (hash-put! arr subscript (->awk value))))

(def (awk-array-exists? rt name subscript)
  "Check if array element exists (without creating)"
  (let ((arrays (awk-runtime-arrays rt)))
    (and (hash-key? arrays name)
         (hash-key? (hash-ref arrays name) subscript))))

(def (awk-array-delete! rt name (subscript #f))
  "Delete array element or entire array"
  (let ((arrays (awk-runtime-arrays rt)))
    (if (hash-key? arrays name)
      (if subscript
        (hash-remove! (hash-ref arrays name) subscript)
        (hash-remove! arrays name)))))

;;; Field access

(def (awk-get-field rt index)
  "Get field by index ($0 = whole record, $1..$NF = fields)"
  (let ((fields (awk-runtime-fields rt)))
    (if (fx< index (vector-length fields))
      (vector-ref fields index)
      (make-awk-string ""))))

(def (awk-set-field! rt index value)
  "Set field by index"
  (let ((fields (awk-runtime-fields rt))
        (nf (awk-runtime-nf rt)))
    ;; Extend fields vector if necessary
    (when (fx>= index (vector-length fields))
      (let ((new-fields (make-vector (fx+ index 1) (make-awk-string ""))))
        (vector-copy! new-fields 0 fields)
        (set! (awk-runtime-fields rt) new-fields)))
    (vector-set! (awk-runtime-fields rt) index (->awk value))
    ;; Update NF if necessary
    (when (fx> index nf)
      (set! (awk-runtime-nf rt) index))))

(def (awk-set-record! rt record-str)
  "Set $0 (the whole record) and recompute fields"
  (set! (awk-runtime-fields rt) (vector (make-awk-string record-str)))
  (set! (awk-runtime-nf rt) 0))

;;; Predefined variable accessors

(def (awk-FS rt)
  (awk->string (awk-get-var rt 'FS (make-awk-string " "))))

(def (awk-RS rt)
  (awk->string (awk-get-var rt 'RS (make-awk-string "\n"))))

(def (awk-OFS rt)
  (awk->string (awk-get-var rt 'OFS (make-awk-string " "))))

(def (awk-ORS rt)
  (awk->string (awk-get-var rt 'ORS (make-awk-string "\n"))))

(def (awk-IGNORECASE rt)
  (awk->number (awk-get-var rt 'IGNORECASE (make-awk-number 0))))

(def (awk-SUBSEP rt)
  (awk->string (awk-get-var rt 'SUBSEP (make-awk-string "\x1c"))))

;;; Environment initialization

(def (awk-init-environ! rt)
  "Initialize ENVIRON array from process environment"
  (let ((environ (awk-runtime-environ rt))
        (env-array (awk-get-array rt 'ENVIRON)))
    (for-each
     (lambda (pair)
       (let ((key (car pair))
             (val (cdr pair)))
         (hash-put! env-array key (make-awk-string val))))
     (getenv-all))))

(def (getenv-all)
  "Get all environment variables as alist"
  (let ((port (open-process "env")))
    (let loop ((vars '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
          (begin (close-port port) (reverse vars))
          (let ((idx (string-index line #\=)))
            (if idx
              (loop (cons (cons (substring line 0 idx)
                               (substring line (fx+ idx 1)))
                         vars))
              (loop vars))))))))

;;; ARGV initialization

(def (awk-init-argv! rt args)
  "Initialize ARGV and ARGC from argument list"
  (let ((argc (length args))
        (argv (list->vector args)))
    (set! (awk-runtime-argc rt) argc)
    (set! (awk-runtime-argv rt) argv)
    (awk-set-var! rt 'ARGC (make-awk-number argc))
    (let ((argv-array (awk-get-array rt 'ARGV)))
      (for-each
       (lambda (i)
         (hash-put! argv-array (number->string i) (make-awk-string (vector-ref argv i))))
       (iota argc)))))

;;; Control flow exceptions

(defstruct awk-exit (code) transparent: #t)
(defstruct awk-next () transparent: #t)
(defstruct awk-nextfile () transparent: #t)
(defstruct awk-break () transparent: #t)
(defstruct awk-continue () transparent: #t)
(defstruct awk-return (value) transparent: #t)

(def (awk-signal-exit! code)
  (raise (make-awk-exit code)))

(def (awk-signal-next!)
  (raise (make-awk-next)))

(def (awk-signal-nextfile!)
  (raise (make-awk-nextfile)))

(def (awk-signal-break!)
  (raise (make-awk-break)))

(def (awk-signal-continue!)
  (raise (make-awk-continue)))

(def (awk-signal-return! value)
  (raise (make-awk-return value)))

;;; Format strings

(def (awk-OFMT rt)
  (awk->string (awk-get-var rt 'OFMT (make-awk-string "%.6g"))))

(def (awk-CONVFMT rt)
  (awk->string (awk-get-var rt 'CONVFMT (make-awk-string "%.6g"))))
