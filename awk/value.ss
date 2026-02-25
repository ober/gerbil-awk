;; -*- Gerbil -*-
;;;; AWK value type with string/number duality
;;
;; AWK values can be both string and number simultaneously.
;; This module implements lazy caching of both representations.

(export #t)

(defstruct awk-value
  (str      ; cached string representation (or #f)
   num      ; cached number representation (or #f)
   flags    ; bit flags: STRCUR, NUMCUR, STRNUM, etc.
   array)   ; hash-table if this is an array (or #f)
  transparent: #t)

;;; Flag bits
(def STRCUR 1)    ; string value is current
(def NUMCUR 2)    ; number value is current
(def STRNUM 4)    ; string from input that looks like a number
(def NUMINT 8)    ; numeric value is known integer
(def REGEX  16)   ; compiled regex value
(def BOOL   32)   ; boolean value (gawk extension)

;;; Constructors

(def (make-awk-uninit)
  "Create uninitialized AWK value (empty string, zero number)"
  (make-awk-value "" 0.0 STRCUR #f))

(def (make-awk-number n)
  "Create AWK value from number"
  (make-awk-value #f n (fxior NUMCUR (if (integer? n) NUMINT 0)) #f))

(def (make-awk-string s)
  "Create AWK value from string"
  (make-awk-value s #f STRCUR #f))

(def (make-awk-strnum s (n #f))
  "Create AWK strnum (string from input that looks like a number)"
  (let ((num (or n (string->number-default s))))
    (make-awk-value s num (fxior STRCUR NUMCUR STRNUM) #f)))

(def (make-awk-array)
  "Create AWK array (associative array)"
  (make-awk-value #f #f 0 (make-hash-table string=? string-hash)))

(def (make-awk-regex pattern)
  "Create AWK regex value (gawk extension)"
  (make-awk-value pattern #f (fxior STRCUR REGEX) #f))

;;; Coercion helpers

(def (string->number-default s)
  "Convert string to number using AWK rules.
   Leading whitespace stripped, parse longest numeric prefix.
   Returns 0.0 for non-numeric strings."
  (let* ((trimmed (string-trim s))
         (len (string-length trimmed)))
    (if (fx= len 0)
      0.0
      (let loop ((i 0) (seen-digit? #f) (has-dot? #f))
        (if (fx>= i len)
          (if seen-digit?
            (string->number trimmed)
            0.0)
          (let ((c (string-ref trimmed i)))
            (cond
              ((and (char>=? c #\0) (char<=? c #\9))
               (loop (fx+ i 1) #t has-dot?))
              ((and (char=? c #\.) (not has-dot?))
               (loop (fx+ i 1) seen-digit? #t))
              ((and (fx= i 0) (or (char=? c #\+) (char=? c #\-)))
               (loop (fx+ i 1) seen-digit? has-dot?))
              ((and (fx= i 1) (char=? c #\x) (char=? (string-ref trimmed 0) #\0))
               ;; Hex number: 0x...
               (let ((hex (string->number trimmed 16)))
                 (or hex 0.0)))
              (else
               (if seen-digit?
                 (string->number (substring trimmed 0 i))
                 0.0)))))))))

(def (number->string-awk n (fmt "%.6g"))
  "Convert number to string using AWK format"
  (cond
    ((not (number? n)) "")
    ((nan? n) "nan")
    ((infinite? n) (if (positive? n) "inf" "-inf"))
    ((integer? n) (number->string (inexact->exact n)))
    (else (format fmt n))))

;;; Main accessors

(def (awk->string v)
  "Get string representation of AWK value"
  (cond
    ((not (awk-value? v)) (if (string? v) v (->string v)))
    ((awk-value-array v) (error "awk: attempt to use array as scalar"))
    ((fxbit-set? (awk-value-flags v) STRCUR) (awk-value-str v))
    (else
     (let* ((num (awk-value-num v))
            (s (number->string-awk num)))
       (set! (awk-value-str v) s)
       (set! (awk-value-flags v) (fxior (awk-value-flags v) STRCUR))
       s))))

(def (awk->number v)
  "Get numeric representation of AWK value"
  (cond
    ((not (awk-value? v)) (if (number? v) v 0.0))
    ((awk-value-array v) (error "awk: attempt to use array as scalar"))
    ((fxbit-set? (awk-value-flags v) NUMCUR) (awk-value-num v))
    (else
     (let ((n (string->number-default (awk-value-str v))))
       (set! (awk-value-num v) n)
       (set! (awk-value-flags v) (fxior (awk-value-flags v) NUMCUR))
       n))))

(def (awk->bool v)
  "Get boolean value: non-zero/non-empty is true"
  (cond
    ((not (awk-value? v)) (if v #t #f))
    ((awk-value-array v) #t)
    ((fxbit-set? (awk-value-flags v) NUMCUR)
     (not (zero? (awk-value-num v))))
    ((fxbit-set? (awk-value-flags v) STRCUR)
     (let ((s (awk-value-str v)))
       (and (> (string-length s) 0)
            (not (string=? s "0")))))
    (else (awk->bool (make-awk-string (awk->string v))))))

;;; Type predicates

(def (awk-array? v)
  "Is value an AWK array?"
  (and (awk-value? v) (hash-table? (awk-value-array v))))

(def (awk-number? v)
  "Does value have a valid numeric representation?"
  (and (awk-value? v)
       (fxbit-set? (awk-value-flags v) NUMCUR)))

(def (awk-string? v)
  "Does value have a valid string representation?"
  (and (awk-value? v)
       (fxbit-set? (awk-value-flags v) STRCUR)))

(def (awk-strnum? v)
  "Is value a strnum (from input, looks like number)?"
  (and (awk-value? v)
       (fxbit-set? (awk-value-flags v) STRNUM)))

(def (awk-uninit? v)
  "Is value uninitialized?"
  (and (awk-value? v)
       (not (awk-value-array v))
       (string=? (or (awk-value-str v) "") "")
       (zero? (or (awk-value-num v) 0.0))))

;;; Comparison (critical for AWK semantics)

(def (awk-compare a b)
  "Compare two AWK values. Returns -1, 0, or 1.
   Comparison semantics depend on operand types:
   - Both strings → string comparison
   - Both numbers → numeric comparison
   - One strnum + one string → string comparison
   - Otherwise → numeric comparison"
  (let* ((a-strnum? (awk-strnum? a))
         (b-strnum? (awk-strnum? b))
         (a-num? (awk-number? a))
         (b-num? (awk-number? b))
         (a-str? (awk-string? a))
         (b-str? (awk-string? b)))
    (cond
      ;; Both are arrays → error
      ((and (awk-array? a) (awk-array? b))
       (error "awk: attempt to compare arrays"))
      ;; Both strings (and at least one is NOT a strnum) → string comparison
      ((and a-str? b-str? (not (and a-strnum? b-strnum?)))
       (string-compare (awk->string a) (awk->string b)))
      ;; Otherwise → numeric comparison
      (else
       (let ((na (awk->number a))
             (nb (awk->number b)))
         (cond
           ((< na nb) -1)
           ((> na nb) 1)
           (else 0)))))))

(def (string-compare sa sb)
  "Compare two strings"
  (cond
    ((string<? sa sb) -1)
    ((string>? sa sb) 1)
    (else 0)))

(def (awk-equal? a b)
  "Test equality of AWK values"
  (fxzero? (awk-compare a b)))

(def (awk<? a b) (fxnegative? (awk-compare a b)))
(def (awk<=? a b) (fx<=? (awk-compare a b) 0))
(def (awk>? a b) (fxpositive? (awk-compare a b)))
(def (awk>=? a b) (fx>=? (awk-compare a b) 0))
(def (awk!=? a b) (fxnonzero? (awk-compare a b)))

;;; Arithmetic operations (coerce to number)

(def (awk+ a b) (make-awk-number (fl+ (awk->number a) (awk->number b))))
(def (awk- a b) (make-awk-number (fl- (awk->number a) (awk->number b))))
(def (awk* a b) (make-awk-number (fl* (awk->number a) (awk->number b))))
(def (awk/ a b)
  (let ((nb (awk->number b)))
    (if (zero? nb)
      (error "awk: division by zero")
      (make-awk-number (fl/ (awk->number a) nb)))))
(def (awk% a b)
  (let ((na (awk->number a))
        (nb (awk->number b)))
    (if (zero? nb)
      (error "awk: division by zero")
      (make-awk-number (flmod na nb)))))
(def (awk^ a b) (make-awk-number (expt (awk->number a) (awk->number b))))

;;; String concatenation

(def (awk-concat a b)
  "Concatenate two AWK values as strings"
  (make-awk-string (string-append (awk->string a) (awk->string b))))

;;; Unary operations

(def (awk-negate v) (make-awk-number (fl- (awk->number v))))
(def (awk-plus v) (make-awk-number (awk->number v)))
(def (awk-not v) (if (awk->bool v) (make-awk-number 0.0) (make-awk-number 1.0)))

;;; Conversion utilities

(def (->awk v)
  "Convert any value to AWK value"
  (cond
    ((awk-value? v) v)
    ((number? v) (make-awk-number v))
    ((string? v) (make-awk-string v))
    ((boolean? v) (make-awk-number (if v 1.0 0.0)))
    ((hash-table? v)
     (let ((av (make-awk-array)))
       (hash-for-each (lambda (k val) (hash-put! (awk-value-array av) k (->awk val))) v)
       av))
    (else (make-awk-string (->string v)))))
