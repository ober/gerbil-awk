;; -*- Gerbil -*-
;;;; AWK Math Built-in Functions

(import ../value)
(export #t)

;;; sin(x)
(def (awk-builtin-sin rt args)
  (match args
    ([x] (make-awk-number (sin (awk->number x))))
    (_ (error "awk: sin: wrong number of arguments"))))

;;; cos(x)
(def (awk-builtin-cos rt args)
  (match args
    ([x] (make-awk-number (cos (awk->number x))))
    (_ (error "awk: cos: wrong number of arguments"))))

;;; atan2(y, x)
(def (awk-builtin-atan2 rt args)
  (match args
    ([y x] (make-awk-number (atan (awk->number y) (awk->number x))))
    (_ (error "awk: atan2: wrong number of arguments"))))

;;; exp(x)
(def (awk-builtin-exp rt args)
  (match args
    ([x] (make-awk-number (exp (awk->number x))))
    (_ (error "awk: exp: wrong number of arguments"))))

;;; log(x)
(def (awk-builtin-log rt args)
  (match args
    ([x] (make-awk-number (log (awk->number x))))
    (_ (error "awk: log: wrong number of arguments"))))

;;; sqrt(x)
(def (awk-builtin-sqrt rt args)
  (match args
    ([x] (make-awk-number (sqrt (awk->number x))))
    (_ (error "awk: sqrt: wrong number of arguments"))))

;;; int(x)
(def (awk-builtin-int rt args)
  (match args
    ([x]
     (let ((n (awk->number x)))
       (make-awk-number (inexact->exact (truncate n)))))
    (_ (error "awk: int: wrong number of arguments"))))

;;; rand()
(def *awk-rng-state* #f)

(def (awk-builtin-rand rt args)
  (match args
    ([]
     (unless *awk-rng-state*
       (set! *awk-rng-state* (make-random-source)))
     (make-awk-number ((random-source-make-reals *awk-rng-state*))))
    (_ (error "awk: rand: wrong number of arguments"))))

;;; srand([x])
(def (awk-builtin-srand rt args)
  (match args
    ([]
     (let ((old-seed (if *awk-rng-state*
                        (random-source-state-ref *awk-rng-state*)
                        0)))
       (set! *awk-rng-state* (make-random-source))
       (random-source-randomize! *awk-rng-state*)
       (make-awk-number old-seed)))
    ([x]
     (let ((old-seed (if *awk-rng-state*
                        (random-source-state-ref *awk-rng-state*)
                        0)))
       (set! *awk-rng-state* (make-random-source))
       (random-source-pseudo-randomize! *awk-rng-state*
                                         (inexact->exact (awk->number x))
                                         0)
       (make-awk-number old-seed)))
    (_ (error "awk: srand: wrong number of arguments"))))

;;; Bitwise functions (gawk extension)

(def (awk-builtin-and rt args)
  (match args
    ([x y]
     (make-awk-number (logand (inexact->exact (awk->number x))
                              (inexact->exact (awk->number y)))))
    (_ (error "awk: and: wrong number of arguments"))))

(def (awk-builtin-or rt args)
  (match args
    ([x y]
     (make-awk-number (logior (inexact->exact (awk->number x))
                              (inexact->exact (awk->number y)))))
    (_ (error "awk: or: wrong number of arguments"))))

(def (awk-builtin-xor rt args)
  (match args
    ([x y]
     (make-awk-number (logxor (inexact->exact (awk->number x))
                              (inexact->exact (awk->number y)))))
    (_ (error "awk: xor: wrong number of arguments"))))

(def (awk-builtin-compl rt args)
  (match args
    ([x]
     (make-awk-number (lognot (inexact->exact (awk->number x)))))
    (_ (error "awk: compl: wrong number of arguments"))))

(def (awk-builtin-lshift rt args)
  (match args
    ([x n]
     (make-awk-number (arithmetic-shift (inexact->exact (awk->number x))
                                         (inexact->exact (awk->number n)))))
    (_ (error "awk: lshift: wrong number of arguments"))))

(def (awk-builtin-rshift rt args)
  (match args
    ([x n]
     (make-awk-number (arithmetic-shift (inexact->exact (awk->number x))
                                         (- (inexact->exact (awk->number n))))))
    (_ (error "awk: rshift: wrong number of arguments"))))
