;; -*- Gerbil -*-
;;;; AWK String Built-in Functions

(import ../value ../runtime ../fields)
(export #t)

;;; length([s])
(def (awk-builtin-length rt args)
  (match args
    ([] (make-awk-number (string-length (awk->string (awk-get-field rt 0)))))
    ([v]
     (cond
       ((awk-array? v)
        (make-awk-number (hash-length (awk-value-array v))))
       (else
        (make-awk-number (string-length (awk->string v))))))
    (_ (error "awk: length: wrong number of arguments"))))

;;; substr(s, start [, len])
(def (awk-builtin-substr rt args)
  (match args
    ([s start]
     (let* ((str (awk->string s))
            (pos (inexact->exact (awk->number start)))
            (pos (max 1 pos))
            (len (string-length str)))
       (if (> pos len)
         (make-awk-string "")
         (make-awk-string (substring str (- pos 1) len)))))
    ([s start len]
     (let* ((str (awk->string s))
            (pos (inexact->exact (awk->number start)))
            (len (inexact->exact (awk->number len)))
            (pos (max 1 pos))
            (str-len (string-length str)))
       (if (> pos str-len)
         (make-awk-string "")
         (let ((end (min (+ (- pos 1) len) str-len)))
           (make-awk-string (substring str (- pos 1) end))))))
    (_ (error "awk: substr: wrong number of arguments"))))

;;; index(s, t)
(def (awk-builtin-index rt args)
  (match args
    ([s t]
     (let* ((str (awk->string s))
            (target (awk->string t))
            (pos (string-contains str target)))
       (make-awk-number (if pos (+ pos 1) 0))))
    (_ (error "awk: index: wrong number of arguments"))))

;;; split(s, a [, fs [, seps]])
(def (awk-builtin-split rt args)
  (match args
    ([s arr-name]
     (let ((n (awk-split rt s (awk->string arr-name) #f)))
       (make-awk-number n)))
    ([s arr-name fs]
     (let ((n (awk-split rt s (awk->string arr-name) fs)))
       (make-awk-number n)))
    ([s arr-name fs seps]
     ;; TODO: populate seps array with separators
     (let ((n (awk-split rt s (awk->string arr-name) fs)))
       (make-awk-number n)))
    (_ (error "awk: split: wrong number of arguments"))))

;;; sub(re, repl [, target])
(def (awk-builtin-sub rt args)
  (match args
    ([re repl]
     (awk-builtin-sub-impl rt re repl (awk-get-field rt 0) #f))
    ([re repl target]
     (awk-builtin-sub-impl rt re repl target #t))
    (_ (error "awk: sub: wrong number of arguments"))))

(def (awk-builtin-sub-impl rt re repl target modify-target?)
  (let* ((pattern (if (awk-expr-regex? re)
                     (awk-expr-regex-pattern re)
                     (awk->string re)))
         (repl-str (awk->string repl))
         (target-str (awk->string target))
         (rx (make-regex pattern (awk-IGNORECASE rt)))
         (result (pregexp-replace rx target-str (make-replacement-fn repl-str))))
    (if modify-target?
      (let ((result-val (make-awk-string result)))
        ;; Determine if target is a field or variable
        ;; For now, just return the result
        result-val)
      (begin
        ;; Update $0
        (awk-set-$0! rt result)
        (make-awk-number 1)))))

;;; gsub(re, repl [, target])
(def (awk-builtin-gsub rt args)
  (match args
    ([re repl]
     (awk-builtin-gsub-impl rt re repl (awk-get-field rt 0) #f))
    ([re repl target]
     (awk-builtin-gsub-impl rt re repl target #t))
    (_ (error "awk: gsub: wrong number of arguments"))))

(def (awk-builtin-gsub-impl rt re repl target modify-target?)
  (let* ((pattern (if (awk-expr-regex? re)
                     (awk-expr-regex-pattern re)
                     (awk->string re)))
         (repl-str (awk->string repl))
         (target-str (awk->string target))
         (rx (make-regex pattern (awk-IGNORECASE rt)))
         (result (pregexp-replace* rx target-str (make-replacement-fn repl-str))))
    (if modify-target?
      (make-awk-string result)
      (begin
        (awk-set-$0! rt result)
        (make-awk-number 1)))))

;;; match(s, re [, arr])
(def (awk-builtin-match rt args)
  (match args
    ([s re]
     (let* ((str (awk->string s))
            (pattern (if (awk-expr-regex? re)
                        (awk-expr-regex-pattern re)
                        (awk->string re)))
            (rx (make-regex pattern (awk-IGNORECASE rt)))
            (m (pregexp-match-positions rx str)))
       (if m
         (let ((start (caar m))
               (end (cdar m)))
           (awk-set-var! rt 'RSTART (make-awk-number (+ start 1)))
           (awk-set-var! rt 'RLENGTH (make-awk-number (- end start)))
           (make-awk-number (+ start 1)))
         (begin
           (awk-set-var! rt 'RSTART (make-awk-number 0))
           (awk-set-var! rt 'RLENGTH (make-awk-number -1))
           (make-awk-number 0)))))
    ([s re arr-name]
     (let* ((str (awk->string s))
            (pattern (if (awk-expr-regex? re)
                        (awk-expr-regex-pattern re)
                        (awk->string re)))
            (rx (make-regex pattern (awk-IGNORECASE rt)))
            (m (pregexp-match-positions rx str))
            (arr (awk-get-array rt (awk->string arr-name))))
       (hash-clear! arr)
       (if m
         (begin
           (for-each
            (lambda (i pair)
              (when pair
                (hash-put! arr (number->string i)
                          (make-awk-string (substring str (car pair) (cdr pair))))))
            (iota (length m))
            m)
           (let ((start (caar m))
                 (end (cdar m)))
             (awk-set-var! rt 'RSTART (make-awk-number (+ start 1)))
             (awk-set-var! rt 'RLENGTH (make-awk-number (- end start)))
             (make-awk-number (+ start 1))))
         (begin
           (awk-set-var! rt 'RSTART (make-awk-number 0))
           (awk-set-var! rt 'RLENGTH (make-awk-number -1))
           (make-awk-number 0)))))
    (_ (error "awk: match: wrong number of arguments"))))

;;; gensub(re, repl, how [, target])
(def (awk-builtin-gensub rt args)
  (match args
    ([re repl how]
     (awk-builtin-gensub-impl rt re repl how (awk-get-field rt 0)))
    ([re repl how target]
     (awk-builtin-gensub-impl rt re repl how target))
    (_ (error "awk: gensub: wrong number of arguments"))))

(def (awk-builtin-gensub-impl rt re repl how target)
  (let* ((pattern (if (awk-expr-regex? re)
                     (awk-expr-regex-pattern re)
                     (awk->string re)))
         (repl-str (awk->string repl))
         (target-str (awk->string target))
         (rx (make-regex pattern (awk-IGNORECASE rt))))
    (cond
      ((or (string=? (awk->string how) "g")
           (string=? (awk->string how) "G"))
       (make-awk-string (pregexp-replace* rx target-str (make-replacement-fn repl-str))))
      (else
       (let ((n (inexact->exact (awk->number how))))
         (make-awk-string (pregexp-replace-n rx target-str n (make-replacement-fn repl-str))))))))

(def (pregexp-replace-n rx str n repl-fn)
  "Replace the nth match"
  (let loop ((pos 0) (count 0))
    (let ((m (pregexp-match-positions rx str pos)))
      (if (not m)
        str
        (if (= count (- n 1))
          (string-append (substring str 0 (caar m))
                        (repl-fn m str)
                        (substring str (cdar m)))
          (loop (cdar m) (+ count 1)))))))

;;; sprintf(fmt, ...)
(def (awk-builtin-sprintf rt args)
  (match args
    ([fmt . vals]
     (make-awk-string (awk-printf-format (awk->string fmt) vals rt)))
    (_ (error "awk: sprintf: wrong number of arguments"))))

;;; tolower(s)
(def (awk-builtin-tolower rt args)
  (match args
    ([s] (make-awk-string (string-downcase (awk->string s))))
    (_ (error "awk: tolower: wrong number of arguments"))))

;;; toupper(s)
(def (awk-builtin-toupper rt args)
  (match args
    ([s] (make-awk-string (string-upcase (awk->string s))))
    (_ (error "awk: toupper: wrong number of arguments"))))

;;; strtonum(s) - gawk extension
(def (awk-builtin-strtonum rt args)
  (match args
    ([s]
     (let* ((str (string-trim (awk->string s)))
            (n (cond
                 ((string-prefix? "0x" str)
                  (string->number str 16))
                 ((string-prefix? "0X" str)
                  (string->number str 16))
                 ((string-prefix? "0" str)
                  (string->number str 8))
                 (else (string->number str)))))
       (make-awk-number (or n 0.0))))
    (_ (error "awk: strtonum: wrong number of arguments"))))

;;; Replacement string processing

(def (make-replacement-fn repl-str)
  "Create replacement function for pregexp-replace"
  (lambda (matches str)
    (let loop ((i 0) (result '()))
      (if (>= i (string-length repl-str))
        (list->string (reverse result))
        (let ((c (string-ref repl-str i)))
          (cond
            ((char=? c #\\)
             (if (< (+ i 1) (string-length repl-str))
               (let ((next (string-ref repl-str (+ i 1))))
                 (cond
                   ((and (char>=? next #\1) (char<=? next #\9))
                    (let* ((group-num (- (char->integer next) (char->integer #\0)))
                           (group-pair (list-ref matches group-num)))
                      (if group-pair
                        (loop (+ i 2)
                              (append (reverse (string->list
                                               (substring str (car group-pair) (cdr group-pair))))
                                     result))
                        (loop (+ i 2) result))))
                   ((char=? next #\&)
                    (loop (+ i 2)
                          (append (reverse (string->list
                                           (substring str (caar matches) (cdar matches))))
                                 result)))
                   ((char=? next #\\)
                    (loop (+ i 2) (cons #\\ result)))
                   (else
                    (loop (+ i 2) (cons next (cons #\\ result))))))
               (loop (+ i 1) (cons #\\ result))))
            ((char=? c #\&)
             ;; & means entire match
             (loop (+ i 1)
                   (append (reverse (string->list
                                    (substring str (caar matches) (cdar matches))))
                          result)))
            (else
             (loop (+ i 1) (cons c result)))))))))

;;; Printf formatting

(def (awk-printf-format fmt args rt)
  "Format string using AWK printf rules"
  (let loop ((i 0) (args args) (result '()))
    (if (>= i (string-length fmt))
      (list->string (reverse result))
      (let ((c (string-ref fmt i)))
        (if (char=? c #\%)
          (if (< (+ i 1) (string-length fmt))
            (let ((next (string-ref fmt (+ i 1))))
              (if (char=? next #\%)
                (loop (+ i 2) args (cons #\% result))
                (let-values (((spec new-i new-args) (parse-format-spec fmt i args)))
                  (loop new-i new-args (append (reverse (string->list spec)) result)))))
            (loop (+ i 1) args (cons #\% result)))
          (loop (+ i 1) args (cons c result)))))))

(def (parse-format-spec fmt start args)
  "Parse printf format specifier, return formatted string and remaining args"
  (let loop ((i (+ start 1)) (width #f) (prec #f) (flags "") (args args))
    (if (>= i (string-length fmt))
      (values "" i args)
      (let ((c (string-ref fmt i)))
        (cond
          ;; Flags
          ((memq c '(#\# #\0 #\- #\  #\+ #\'))
           (loop (+ i 1) width prec (string-append flags (string c)) args))
          ;; Width
          ((and (char-numeric? c) (not width))
           (let num-loop ((j i) (n 0))
             (if (and (< j (string-length fmt))
                      (char-numeric? (string-ref fmt j)))
               (num-loop (+ j 1) (+ (* n 10) (- (char->integer (string-ref fmt j))
                                                  (char->integer #\0))))
               (loop j (or width n) prec flags args))))
          ;; Precision
          ((char=? c #\.)
           (let num-loop ((j (+ i 1)) (n 0))
             (if (and (< j (string-length fmt))
                      (char-numeric? (string-ref fmt j)))
               (num-loop (+ j 1) (+ (* n 10) (- (char->integer (string-ref fmt j))
                                                  (char->integer #\0))))
               (loop j width (or n 0) flags args))))
          ;; Format specifier
          ((memq c '(#\d #\i #\o #\x #\X #\u #\c #\s #\f #\e #\E #\g #\G #\a #\A))
           (let ((arg (car args))
                 (rest (cdr args)))
             (values (format-value c arg width prec flags) (+ i 1) rest)))
          (else
           (values (string c) (+ i 1) args)))))))

(def (format-value spec arg width prec flags)
  "Format a single value according to printf spec"
  (case spec
    ((#\d #\i #\u)
     (format-integer (inexact->exact (awk->number arg)) width flags))
    ((#\o)
     (format-octal (inexact->exact (awk->number arg)) width flags))
    ((#\x)
     (format-hex (inexact->exact (awk->number arg)) width flags #f))
    ((#\X)
     (format-hex (inexact->exact (awk->number arg)) width flags #t))
    ((#\c)
     (format-char arg))
    ((#\s)
     (format-string (awk->string arg) width prec))
    ((#\f)
     (format-float (awk->number arg) width prec flags))
    ((#\e)
     (format-sci (awk->number arg) width prec flags #f))
    ((#\E)
     (format-sci (awk->number arg) width prec flags #t))
    ((#\g #\G)
     (format-general (awk->number arg) width prec flags (char=? spec #\G)))
    (else "?")))

(def (format-integer n width flags)
  (let* ((s (number->string n))
         (pad (if width (max 0 (- width (string-length s))) 0)))
    (if (string-contains flags "-")
      (string-append s (make-string pad #\space))
      (if (string-contains flags "0")
        (string-append (make-string pad #\0) s)
        (string-append (make-string pad #\space) s)))))

(def (format-octal n width flags)
  (let* ((s (number->string n 8))
         (s (if (string-contains flags "#") (string-append "0" s) s))
         (pad (if width (max 0 (- width (string-length s))) 0)))
    (string-append (make-string pad #\0) s)))

(def (format-hex n width flags upper?)
  (let* ((s (number->string n 16))
         (s (if upper? (string-upcase s) s))
         (s (if (string-contains flags "#")
               (string-append (if upper? "0X" "0x") s)
               s))
         (pad (if width (max 0 (- width (string-length s))) 0)))
    (string-append (make-string pad #\0) s)))

(def (format-char arg)
  (let ((s (awk->string arg)))
    (if (> (string-length s) 0)
      (string (string-ref s 0))
      " ")))

(def (format-string s width prec)
  (let ((s (if prec (substring s 0 (min prec (string-length s))) s))
        (pad (if width (max 0 (- width (string-length s))) 0)))
    (string-append (make-string pad #\space) s)))

(def (format-float n width prec flags)
  (let* ((prec (or prec 6))
         (s (format (string-append "~," (number->string prec) "f") n))
         (pad (if width (max 0 (- width (string-length s))) 0)))
    (if (string-contains flags "-")
      (string-append s (make-string pad #\space))
      (string-append (make-string pad #\space) s))))

(def (format-sci n width prec flags upper?)
  (let* ((prec (or prec 6))
         (s (format (string-append "~," (number->string prec) (if upper? "E" "e")) n))
         (pad (if width (max 0 (- width (string-length s))) 0)))
    (string-append (make-string pad #\space) s)))

(def (format-general n width prec flags upper?)
  (let* ((prec (or prec 6))
         (s (format (string-append "~," (number->string prec) (if upper? "G" "g")) n))
         (pad (if width (max 0 (- width (string-length s))) 0)))
    (string-append (make-string pad #\space) s)))
