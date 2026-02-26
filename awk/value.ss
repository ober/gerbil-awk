;; -*- Gerbil -*-
;;;; AWK value type with string/number duality

(import :std/format :std/srfi/13)
(export #t)

(defstruct awk-value
  (str      ; cached string representation (or #f)
   num      ; cached number representation (or #f)
   flags)   ; bit flags
  transparent: #t)

;;; Flag bits (used as bitmasks with fxand)
(def FLAG-STRCUR 1)    ; string value is current
(def FLAG-NUMCUR 2)    ; number value is current
(def FLAG-STRNUM 4)    ; string from input that looks like a number
(def FLAG-NUMINT 8)    ; numeric value is known integer

(def (flag-set? flags mask)
  (not (fxzero? (fxand flags mask))))

;;; Constructors

(def (make-awk-uninit)
  (make-awk-value "" 0.0 (fxior FLAG-STRCUR FLAG-NUMCUR)))

(def (make-awk-number n)
  (let ((n (if (exact? n) (inexact n) n)))
    (make-awk-value #f n (fxior FLAG-NUMCUR (if (and (number? n) (finite? n) (= n (floor n))) FLAG-NUMINT 0)))))

(def (make-awk-string s)
  (make-awk-value s #f FLAG-STRCUR))

(def (make-awk-strnum s)
  (let ((num (string->number-awk s)))
    (make-awk-value s num (fxior FLAG-STRCUR FLAG-NUMCUR FLAG-STRNUM))))

;;; Coercion helpers

(def (string->number-awk s)
  "Convert string to number using AWK rules.
   Leading whitespace stripped, parse longest numeric prefix.
   Returns 0.0 for non-numeric strings."
  (let* ((trimmed (string-trim s))
         (len (string-length trimmed)))
    (if (= len 0)
      0.0
      ;; Parse longest valid numeric prefix
      (let loop ((i 0) (seen-digit? #f) (has-dot? #f) (has-exp? #f))
        (if (>= i len)
          (if seen-digit?
            (or (string->number trimmed) 0.0)
            0.0)
          (let ((c (string-ref trimmed i)))
            (cond
              ((char-numeric? c)
               (loop (+ i 1) #t has-dot? has-exp?))
              ((and (char=? c #\.) (not has-dot?) (not has-exp?))
               (loop (+ i 1) seen-digit? #t has-exp?))
              ((and (= i 0) (or (char=? c #\+) (char=? c #\-)))
               (loop (+ i 1) seen-digit? has-dot? has-exp?))
              ((and (char-ci=? c #\e) (not has-exp?) seen-digit?)
               (if (and (< (+ i 1) len)
                        (let ((nc (string-ref trimmed (+ i 1))))
                          (or (char-numeric? nc) (char=? nc #\+) (char=? nc #\-))))
                 (loop (+ i 2) seen-digit? has-dot? #t)
                 ;; e not followed by valid exponent char — stop
                 (if seen-digit?
                   (or (string->number (substring trimmed 0 i)) 0.0)
                   0.0)))
              (else
               (if seen-digit?
                 (or (string->number (substring trimmed 0 i)) 0.0)
                 0.0)))))))))

(def (number->string-awk n (ofmt "%.6g"))
  "Convert number to string using AWK OFMT/CONVFMT format"
  (cond
    ((not (number? n)) "")
    ((nan? n) "nan")
    ((infinite? n) (if (positive? n) "inf" "-inf"))
    ((and (finite? n) (= n (floor n)) (<= (abs n) 1e15))
     (number->string (inexact->exact (floor n))))
    (else (c-format ofmt n))))

;;; C-style format for a single float
(def (c-format fmt n)
  "Minimal C-style printf for %.Ng format (used by OFMT/CONVFMT)"
  ;; Parse precision from format like "%.6g"
  (let ((prec (parse-g-precision fmt)))
    (format-g-number n prec)))

(def (parse-g-precision fmt)
  "Extract precision from %.Ng format string, default 6"
  (let ((len (string-length fmt)))
    (if (and (>= len 3)
             (char=? (string-ref fmt 0) #\%)
             (char=? (string-ref fmt 1) #\.))
      (let loop ((i 2) (n 0))
        (if (>= i len)
          (max n 1)
          (let ((c (string-ref fmt i)))
            (if (char-numeric? c)
              (loop (+ i 1) (+ (* n 10) (- (char->integer c) (char->integer #\0))))
              (max n 1)))))
      6)))

(def (format-g-number n prec)
  "Format number with %g semantics: prec significant digits, strip trailing zeros"
  (cond
    ((zero? n) "0")
    ((nan? n) "nan")
    ((infinite? n) (if (positive? n) "inf" "-inf"))
    (else (format-g-nonzero n prec))))

(def (strip-zeros s)
  "Strip trailing zeros after decimal point; remove dot if nothing follows"
  (if (not (string-contains s "."))
    s
    (let loop ((i (- (string-length s) 1)))
      (cond
        ((char=? (string-ref s i) #\0) (loop (- i 1)))
        ((char=? (string-ref s i) #\.) (substring s 0 i))
        (else (substring s 0 (+ i 1)))))))

(def (format-g-nonzero n prec)
  (let* ((sign (if (< n 0) "-" ""))
         (abs-n (abs n))
         (exp (inexact->exact (floor (/ (log abs-n) (log 10)))))
         (exp (if (>= (/ abs-n (expt 10.0 (+ exp 1))) 1.0) (+ exp 1) exp)))
    (if (and (>= exp -4) (< exp prec))
      (format-g-f-style sign abs-n exp prec)
      (format-g-e-style sign abs-n exp prec))))

(def (format-g-f-style sign abs-n exp prec)
  (let* ((frac-digits (max 0 (- prec exp 1)))
         (factor (expt 10 frac-digits))
         (rounded (/ (round (* abs-n factor)) factor))
         (int-part (inexact->exact (truncate rounded)))
         (frac-part (abs (- rounded int-part))))
    (if (= frac-digits 0)
      (string-append sign (number->string int-part))
      (let* ((frac-int (inexact->exact (round (* frac-part factor))))
             (frac-str (number->string frac-int))
             (frac-str (string-append
                        (make-string (max 0 (- frac-digits (string-length frac-str))) #\0)
                        frac-str))
             (full (string-append sign (number->string int-part) "." frac-str)))
        (strip-zeros full)))))

(def (format-g-e-style sign abs-n exp prec)
  (let* ((mantissa (/ abs-n (expt 10.0 exp)))
         (mant-digits (- prec 1))
         (factor (expt 10 mant-digits))
         (rounded-m (/ (round (* mantissa factor)) factor)))
    ;; Adjust if rounding pushed mantissa to 10
    (when (>= rounded-m 10.0)
      (set! rounded-m (/ rounded-m 10.0))
      (set! exp (+ exp 1)))
    (let* ((mant-int (inexact->exact (truncate rounded-m)))
           (mant-frac (abs (- rounded-m mant-int)))
           (exp-sign (if (>= exp 0) "+" "-"))
           (exp-str (number->string (abs exp)))
           (exp-str (if (< (string-length exp-str) 2)
                      (string-append "0" exp-str) exp-str)))
      (if (= mant-digits 0)
        (string-append sign (number->string mant-int) "e" exp-sign exp-str)
        (let* ((frac-int (inexact->exact (round (* mant-frac factor))))
               (frac-str (number->string frac-int))
               (frac-str (string-append
                          (make-string (max 0 (- mant-digits (string-length frac-str))) #\0)
                          frac-str))
               (mant-full (string-append (number->string mant-int) "." frac-str)))
          (string-append sign (strip-zeros mant-full) "e" exp-sign exp-str))))))

;;; Main accessors

(def (awk->string v)
  "Get string representation of AWK value"
  (cond
    ((string? v) v)
    ((number? v) (number->string-awk v))
    ((not (awk-value? v)) (format "~a" v))
    ((flag-set? (awk-value-flags v) FLAG-STRCUR)
     (awk-value-str v))
    (else
     (let* ((num (awk-value-num v))
            (s (number->string-awk num)))
       (set! (awk-value-str v) s)
       (set! (awk-value-flags v) (fxior (awk-value-flags v) FLAG-STRCUR))
       s))))

(def (awk->number v)
  "Get numeric representation of AWK value"
  (cond
    ((number? v) (if (exact? v) (inexact v) v))
    ((string? v) (string->number-awk v))
    ((not (awk-value? v)) 0.0)
    ((flag-set? (awk-value-flags v) FLAG-NUMCUR)
     (awk-value-num v))
    (else
     (let ((n (string->number-awk (or (awk-value-str v) ""))))
       (set! (awk-value-num v) n)
       (set! (awk-value-flags v) (fxior (awk-value-flags v) FLAG-NUMCUR))
       n))))

(def (awk->bool v)
  "Get boolean value: non-zero number or non-empty string is true"
  (cond
    ((not (awk-value? v))
     (cond ((number? v) (not (zero? v)))
           ((string? v) (> (string-length v) 0))
           (else (if v #t #f))))
    ((flag-set? (awk-value-flags v) FLAG-NUMCUR)
     (not (zero? (awk-value-num v))))
    ((flag-set? (awk-value-flags v) FLAG-STRCUR)
     (> (string-length (awk-value-str v)) 0))
    (else #f)))

;;; Type predicates

(def (awk-strnum? v)
  (and (awk-value? v)
       (flag-set? (awk-value-flags v) FLAG-STRNUM)))

(def (awk-numcur? v)
  (and (awk-value? v)
       (flag-set? (awk-value-flags v) FLAG-NUMCUR)))

(def (awk-strcur? v)
  (and (awk-value? v)
       (flag-set? (awk-value-flags v) FLAG-STRCUR)))

;;; Comparison (critical for AWK semantics)

(def (awk-compare a b)
  "Compare two AWK values. Returns -1, 0, or 1."
  (let ((a (if (awk-value? a) a (->awk a)))
        (b (if (awk-value? b) b (->awk b))))
    (cond
      ;; If both are strnum, compare numerically
      ((and (awk-strnum? a) (awk-strnum? b))
       (num-cmp (awk->number a) (awk->number b)))
      ;; If both have string but not both strnum, compare as strings
      ((and (awk-strcur? a) (awk-strcur? b)
            (not (and (awk-strnum? a) (awk-strnum? b))))
       (string-cmp (awk->string a) (awk->string b)))
      ;; If one is a constant string (not strnum) and the other is anything
      ((and (awk-strcur? a) (not (awk-numcur? a)))
       (string-cmp (awk->string a) (awk->string b)))
      ((and (awk-strcur? b) (not (awk-numcur? b)))
       (string-cmp (awk->string a) (awk->string b)))
      ;; Otherwise numeric
      (else
       (num-cmp (awk->number a) (awk->number b))))))

(def (num-cmp a b)
  (cond ((< a b) -1) ((> a b) 1) (else 0)))

(def (string-cmp a b)
  (cond ((string<? a b) -1) ((string>? a b) 1) (else 0)))

(def (awk-equal? a b) (= (awk-compare a b) 0))
(def (awk<? a b)  (< (awk-compare a b) 0))
(def (awk<=? a b) (<= (awk-compare a b) 0))
(def (awk>? a b)  (> (awk-compare a b) 0))
(def (awk>=? a b) (>= (awk-compare a b) 0))
(def (awk!=? a b) (not (= (awk-compare a b) 0)))

;;; Arithmetic operations

(def (awk+ a b) (make-awk-number (+ (awk->number a) (awk->number b))))
(def (awk- a b) (make-awk-number (- (awk->number a) (awk->number b))))
(def (awk* a b) (make-awk-number (* (awk->number a) (awk->number b))))
(def (awk/ a b)
  (let ((na (awk->number a))
        (nb (awk->number b)))
    (if (zero? nb)
      (make-awk-number (/ (inexact na) 0.0))
      (make-awk-number (/ na nb)))))
(def (awk-mod a b)
  (let ((na (awk->number a))
        (nb (awk->number b)))
    (if (zero? nb)
      (make-awk-number (/ (inexact na) 0.0))
      (make-awk-number (- na (* (truncate (/ na nb)) nb))))))
(def (awk-pow a b)
  (make-awk-number (expt (awk->number a) (awk->number b))))

;;; String concatenation

(def (awk-concat a b)
  (make-awk-string (string-append (awk->string a) (awk->string b))))

;;; Unary operations

(def (awk-negate v) (make-awk-number (- (awk->number v))))
(def (awk-plus v) (make-awk-number (awk->number v)))
(def (awk-not v) (make-awk-number (if (awk->bool v) 0.0 1.0)))

;;; Conversion utilities

(def (->awk v)
  "Convert any Scheme value to AWK value"
  (cond
    ((awk-value? v) v)
    ((number? v) (make-awk-number v))
    ((string? v) (make-awk-string v))
    ((boolean? v) (make-awk-number (if v 1.0 0.0)))
    (else (make-awk-string (format "~a" v)))))
