;; -*- Gerbil -*-
;;;; AWK Field Splitting
;;
;; Handles $0, $1-$NF, and field splitting according to FS/FPAT/FIELDWIDTHS.

(import ./value ./runtime)
(export #t)

;;; Field splitting modes

(def FIELD-MODE-DEFAULT 0)   ; FS = " "
(def FIELD-MODE-CHAR 1)      ; FS = single char
(def FIELD-MODE-REGEX 2)     ; FS = regex
(def FIELD-MODE-NULL 3)      ; FS = ""
(def FIELD-MODE-FPAT 4)      ; FPAT pattern
(def FIELD-MODE-FWIDTH 5)    ; FIELDWIDTHS

;;; Main field splitting

(def (split-fields! rt record-str)
  "Split record into fields according to current FS/FIELDWIDTHS/FPAT"
  (let ((fs (awk-FS rt)))
    (cond
      ;; FIELDWIDTHS mode
      ((hash-key? (awk-runtime-variables rt) 'FIELDWIDTHS)
       (let ((fw (awk->string (awk-get-var rt 'FIELDWIDTHS))))
         (when (> (string-length fw) 0)
           (split-fields-fixed-width! rt record-str fw)
           (return))))
      ;; FPAT mode
      ((hash-key? (awk-runtime-variables rt) 'FPAT)
       (let ((fpat (awk->string (awk-get-var rt 'FPAT))))
         (split-fields-fpat! rt record-str fpat)
         (return))))
    ;; FS mode
    (cond
      ;; Default: FS = " " (split on runs of whitespace)
      ((string=? fs " ")
       (split-fields-default! rt record-str))
      ;; Empty FS: each char is a field
      ((string=? fs "")
       (split-fields-null! rt record-str))
      ;; Single char FS
      ((= (string-length fs) 1)
       (split-fields-char! rt record-str (string-ref fs 0)))
      ;; Regex FS
      (else
       (split-fields-regex! rt record-str fs)))))

(def (split-fields-default! rt record-str)
  "Split on runs of whitespace (FS = ' ')"
  (let* ((trimmed (string-trim record-str))
         (len (string-length trimmed)))
    (if (= len 0)
      (begin
        (set! (awk-runtime-fields rt) (vector (make-awk-string record-str)))
        (set! (awk-runtime-nf rt) 0))
      (let loop ((i 0) (fields '()) (start 0) (in-field? #f))
        (if (>= i len)
          (let* ((all-fields (cons (make-awk-string (substring trimmed start len)) fields))
                 (vec (list->vector (cons (make-awk-string record-str) (reverse all-fields)))))
            (set! (awk-runtime-fields rt) vec)
            (set! (awk-runtime-nf rt) (length all-fields)))
          (let ((c (string-ref trimmed i)))
            (if (char-whitespace? c)
              (if in-field?
                (loop (+ i 1) (cons (make-awk-string (substring trimmed start i)) fields) i #f)
                (loop (+ i 1) fields start #f))
              (if in-field?
                (loop (+ i 1) fields start #t)
                (loop (+ i 1) fields i #t)))))))))

(def (split-fields-char! rt record-str sep-char)
  "Split on single character separator"
  (let ((parts (string-split record-str sep-char)))
    (let* ((fields (map make-awk-string parts))
           (vec (list->vector (cons (make-awk-string record-str) fields))))
      (set! (awk-runtime-fields rt) vec)
      (set! (awk-runtime-nf rt) (length fields)))))

(def (split-fields-null! rt record-str)
  "Split each character into a field (FS = '')"
  (let* ((len (string-length record-str))
         (fields (for/collect ((i (in-range len)))
                   (make-awk-string (string (string-ref record-str i)))))
         (vec (list->vector (cons (make-awk-string record-str) fields))))
    (set! (awk-runtime-fields rt) vec)
    (set! (awk-runtime-nf rt) len)))

(def (split-fields-regex! rt record-str pattern)
  "Split using regex separator"
  (let* ((rx (make-regex pattern (awk-IGNORECASE rt)))
         (parts (pregexp-split rx record-str)))
    (let* ((fields (map make-awk-string parts))
           (vec (list->vector (cons (make-awk-string record-str) fields))))
      (set! (awk-runtime-fields rt) vec)
      (set! (awk-runtime-nf rt) (length fields)))))

(def (split-fields-fixed-width! rt record-str widths-str)
  "Split using fixed field widths"
  (let* ((widths (map string->number (string-split widths-str #\space)))
         (len (string-length record-str))
         (fields (let loop ((pos 0) (widths widths) (fields '()))
                   (if (or (null? widths) (>= pos len))
                     (reverse (cons (make-awk-string (substring record-str pos len)) fields))
                     (let* ((w (car widths))
                            (end (min (+ pos w) len))
                            (field (substring record-str pos end)))
                       (loop end (cdr widths) (cons (make-awk-string field) fields)))))))
    (let ((vec (list->vector (cons (make-awk-string record-str) fields))))
      (set! (awk-runtime-fields rt) vec)
      (set! (awk-runtime-nf rt) (length fields)))))

(def (split-fields-fpat! rt record-str pattern)
  "Split using field content pattern (FPAT)"
  (let* ((rx (make-regex pattern (awk-IGNORECASE rt)))
         (matches (pregexp-match-positions-all rx record-str))
         (fields (map (lambda (m)
                        (make-awk-string (substring record-str (car m) (cdr m))))
                      matches)))
    (let ((vec (list->vector (cons (make-awk-string record-str) fields))))
      (set! (awk-runtime-fields rt) vec)
      (set! (awk-runtime-nf rt) (length fields)))))

(def (pregexp-match-positions-all rx str)
  "Find all matches of regex in string"
  (let loop ((pos 0) (matches '()))
    (let ((m (pregexp-match-positions rx str pos)))
      (if (not m)
        (reverse matches)
        (let ((start (caar m))
              (end (cdar m)))
          (if (= start end)
            ;; Zero-width match, advance by 1
            (loop (+ end 1) matches)
            (loop end (cons (car m) matches))))))))

;;; Field rebuild ($0 after field modification)

(def (rebuild-record! rt)
  "Rebuild $0 from fields using OFS"
  (let* ((fields (awk-runtime-fields rt))
         (nf (awk-runtime-nf rt))
         (ofs (awk-OFS rt)))
    (if (= nf 0)
      (vector-set! fields 0 (make-awk-string ""))
      (let ((parts (for/collect ((i (in-range 1 (+ nf 1))))
                     (if (< i (vector-length fields))
                       (awk->string (vector-ref fields i))
                       ""))))
        (vector-set! fields 0 (make-awk-string (string-join parts ofs)))))))

;;; NF assignment

(def (awk-set-nf! rt new-nf)
  "Handle NF assignment (truncate or expand fields)"
  (let ((old-nf (awk-runtime-nf rt))
        (fields (awk-runtime-fields rt)))
    (cond
      ((< new-nf 0)
       (error "awk: NF set to negative value"))
      ((= new-nf old-nf)
       (void))
      ((< new-nf old-nf)
       ;; Truncate: clear fields beyond new-nf
       (set! (awk-runtime-nf rt) new-nf)
       (rebuild-record! rt))
      (else
       ;; Expand: add empty fields
       (let* ((new-vec (make-vector (+ new-nf 1) (make-awk-string ""))))
         (vector-copy! new-vec 0 fields)
         (set! (awk-runtime-fields rt) new-vec)
         (set! (awk-runtime-nf rt) new-nf)
         (rebuild-record! rt))))))

;;; $0 assignment

(def (awk-set-$0! rt value)
  "Set $0 and re-split fields"
  (let ((str (awk->string (->awk value))))
    (set! (awk-runtime-fields rt) (vector (make-awk-string str)))
    (set! (awk-runtime-nf rt) 0)
    (split-fields! rt str)))

;;; Field assignment

(def (awk-set-field-index! rt index value)
  "Set field by index, update NF if necessary, mark $0 invalid"
  (let ((nf (awk-runtime-nf rt)))
    ;; Extend if beyond NF
    (when (> index nf)
      (set! (awk-runtime-nf rt) index)
      ;; Extend fields vector if needed
      (let ((fields (awk-runtime-fields rt)))
        (when (>= index (vector-length fields))
          (let ((new-fields (make-vector (+ index 1) (make-awk-string ""))))
            (vector-copy! new-fields 0 fields)
            (set! (awk-runtime-fields rt) new-fields)))))
    (vector-set! (awk-runtime-fields rt) index (->awk value))
    (rebuild-record! rt)))

;;; split() built-in implementation

(def (awk-split rt str array-name (separator #f))
  "AWK split() function implementation"
  (let* ((s (awk->string (->awk str)))
         (arr (awk-get-array rt array-name))
         (sep (if separator
                (awk->string (->awk separator))
                (awk-FS rt)))
         (parts (cond
                  ((or (not sep) (string=? sep " "))
                   (string-split-default s))
                  ((= (string-length sep) 1)
                   (string-split s (string-ref sep 0)))
                  (else
                   (pregexp-split (make-regex sep (awk-IGNORECASE rt)) s)))))
    ;; Clear array
    (hash-clear! arr)
    ;; Populate with 1-indexed values
    (for-each
     (lambda (i)
       (let ((part (list-ref parts (- i 1))))
         (hash-put! arr (number->string i) (make-awk-strnum part))))
     (iota (length parts) 1))
    (length parts)))

(def (string-split-default s)
  "Split string on runs of whitespace (AWK default FS behavior)"
  (let* ((trimmed (string-trim s))
         (len (string-length trimmed)))
    (if (= len 0)
      '()
      (let loop ((i 0) (parts '()) (start 0) (in-field? #f))
        (if (>= i len)
          (reverse (cons (substring trimmed start len) parts))
          (let ((c (string-ref trimmed i)))
            (if (char-whitespace? c)
              (if in-field?
                (loop (+ i 1) (cons (substring trimmed start i) parts) i #f)
                (loop (+ i 1) parts start #f))
              (if in-field?
                (loop (+ i 1) parts start #t)
                (loop (+ i 1) fields i #t)))))))))

;;; Regex helpers

(def (make-regex pattern ignorecase?)
  "Create regex from pattern, respecting IGNORECASE"
  (if (or ignorecase? (> (awk-IGNORECASE #f) 0))
    (pregexp (string-append "(?i)" pattern))
    (pregexp pattern)))
