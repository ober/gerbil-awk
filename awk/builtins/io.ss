;; -*- Gerbil -*-
;;;; AWK I/O Built-in Functions

(import ../value ../runtime)
(export #t)

;;; close(file [, how])
(def (awk-builtin-close rt args)
  (match args
    ([name]
     (close-file-or-pipe rt (awk->string name) #f))
    ([name how]
     (close-file-or-pipe rt (awk->string name) (awk->string how)))
    (_ (error "awk: close: wrong number of arguments"))))

(def (close-file-or-pipe rt name how)
  (let ((input-ports (awk-runtime-input-ports rt))
        (output-ports (awk-runtime-output-ports rt)))
    (cond
      ((and (hash-key? output-ports name) (or (not how) (string=? how "to")))
       (let ((port (hash-ref output-ports name)))
         (close-port port)
         (hash-remove! output-ports name)
         (make-awk-number 0)))
      ((and (hash-key? input-ports name) (or (not how) (string=? how "from")))
       (let ((port (hash-ref input-ports name)))
         (close-port port)
         (hash-remove! input-ports name)
         (make-awk-number 0)))
      (else
       (make-awk-number -1)))))

;;; system(command)
(def (awk-builtin-system rt args)
  (match args
    ([cmd]
     (let ((status (system (awk->string cmd))))
       (make-awk-number status)))
    (_ (error "awk: system: wrong number of arguments"))))

;;; fflush([file])
(def (awk-builtin-fflush rt args)
  (match args
    ([]
     ;; Flush all output
     (let ((output-ports (awk-runtime-output-ports rt)))
       (hash-for-each
        (lambda (name port)
          (force-output port))
        output-ports))
     (force-output (current-output-port))
     (make-awk-number 0))
    ([name]
     (let ((name-str (awk->string name)))
       (cond
         ((string=? name-str "")
          (force-output (current-output-port))
          (make-awk-number 0))
         ((hash-key? (awk-runtime-output-ports rt) name-str)
          (force-output (hash-ref (awk-runtime-output-ports rt) name-str))
          (make-awk-number 0))
         (else
          (make-awk-number -1)))))
    (_ (error "awk: fflush: wrong number of arguments"))))

;;; getline variants

(def (awk-builtin-getline rt args)
  "Handle all getline variants"
  (match args
    ([] (getline-into-$0 rt))
    ([var] (getline-into-var rt var))
    ([var '< file] (getline-from-file rt var file))
    (_ (error "awk: getline: invalid arguments"))))

(def (getline-into-$0 rt)
  "getline - read next record into $0"
  (let ((result (read-next-record rt)))
    (if result
      (begin
        (awk-set-$0! rt result)
        (set! (awk-runtime-nr rt) (+ (awk-runtime-nr rt) 1))
        (set! (awk-runtime-fnr rt) (+ (awk-runtime-fnr rt) 1))
        (make-awk-number 1))
      (if (eof-object? result)
        (make-awk-number 0)
        (make-awk-number -1)))))

(def (getline-into-var rt var)
  "getline var - read next record into variable"
  (let ((result (read-next-record rt)))
    (if result
      (begin
        (awk-set-var! rt var (make-awk-string result))
        (set! (awk-runtime-nr rt) (+ (awk-runtime-nr rt) 1))
        (set! (awk-runtime-fnr rt) (+ (awk-runtime-fnr rt) 1))
        (make-awk-number 1))
      (if (eof-object? result)
        (make-awk-number 0)
        (make-awk-number -1)))))

(def (getline-from-file rt var file)
  "getline var < file - read from file into variable"
  (let* ((filename (awk->string file))
         (port (get-or-open-input-port rt filename)))
    (if port
      (let ((line (read-line port)))
        (if (eof-object? line)
          (begin
            (close-input-port port)
            (hash-remove! (awk-runtime-input-ports rt) filename)
            (make-awk-number 0))
          (begin
            (awk-set-var! rt var (make-awk-string line))
            (make-awk-number 1))))
      (make-awk-number -1))))

(def (read-next-record rt)
  "Read next record from current input"
  ;; This is a simplified version - full version handles RS properly
  (let ((line (read-line (current-input-port))))
    (if (eof-object? line)
      #f
      line)))

(def (get-or-open-input-port rt filename)
  "Get existing input port or open new one"
  (let ((input-ports (awk-runtime-input-ports rt)))
    (if (hash-key? input-ports filename)
      (hash-ref input-ports filename)
      (let ((port (open-input-file filename)))
        (when port
          (hash-put! input-ports filename port))
        port))))

;;; Output redirection helpers

(def (get-or-open-output-port rt filename append?)
  "Get existing output port or open new one"
  (let ((output-ports (awk-runtime-output-ports rt))
        (key filename))
    (if (hash-key? output-ports key)
      (hash-ref output-ports key)
      (let ((port (open-output-file filename
                                    [create: 'always
                                     append: append?])))
        (when port
          (hash-put! output-ports key port))
        port))))

(def (get-or-open-pipe rt command)
  "Open a pipe to a command"
  (let ((output-ports (awk-runtime-output-ports rt)))
    (if (hash-key? output-ports command)
      (hash-ref output-ports command)
      (let ((port (open-process command)))
        (when port
          (hash-put! output-ports command port))
        port))))

;;; Print output

(def (awk-print rt args)
  "AWK print statement implementation"
  (let-values (((redirect append?) (values #f #f)))
    (awk-print-to-port rt args (current-output-port))))

(def (awk-print-to-port rt args port)
  (let ((ofs (awk-OFS rt))
        (ors (awk-ORS rt)))
    (if (null? args)
      (display (awk->string (awk-get-field rt 0)) port)
      (let loop ((args args) (first? #t))
        (unless (null? args)
          (unless first?
            (display ofs port))
          (display (awk->string (car args)) port)
          (loop (cdr args) #f))))
    (display ors port)
    (force-output port)))

;;; Printf output

(def (awk-printf rt fmt args)
  "AWK printf statement implementation"
  (let ((str (awk-printf-format fmt args rt)))
    (display str (current-output-port))))

;;; File I/O for input processing

(def (open-input-sources rt files)
  "Set up input processing from file list"
  ;; files is list of filename strings
  (values))

(def (read-record-from-file rt filename)
  "Read a record from a file"
  (let ((port (get-or-open-input-port rt filename)))
    (if port
      (let ((line (read-line port)))
        (if (eof-object? line)
          (begin
            (close-input-port port)
            (hash-remove! (awk-runtime-input-ports rt) filename)
            #f)
          line))
      #f)))
