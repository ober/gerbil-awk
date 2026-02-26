;; -*- Gerbil -*-
;;;; AWK Math Built-in Functions

(import ../value)
(export #t)

(def (awk-builtin-sin env args)
  (make-awk-number (sin (awk->number (car args)))))

(def (awk-builtin-cos env args)
  (make-awk-number (cos (awk->number (car args)))))

(def (awk-builtin-atan2 env args)
  (make-awk-number (atan (awk->number (car args))
                         (awk->number (cadr args)))))

(def (awk-builtin-exp env args)
  (make-awk-number (exp (awk->number (car args)))))

(def (awk-builtin-log env args)
  (make-awk-number (log (awk->number (car args)))))

(def (awk-builtin-sqrt env args)
  (make-awk-number (sqrt (awk->number (car args)))))

(def (awk-builtin-int env args)
  (make-awk-number (truncate (awk->number (car args)))))

;;; rand() / srand()

(def *awk-rand-seed* 1)
(def *awk-rand-state* #f)

(def (ensure-rand-state!)
  (unless *awk-rand-state*
    (set! *awk-rand-state* (make-random-source))
    (random-source-pseudo-randomize! *awk-rand-state* *awk-rand-seed* 0)))

(def (awk-builtin-rand env args)
  (ensure-rand-state!)
  (make-awk-number ((random-source-make-reals *awk-rand-state*))))

(def (awk-builtin-srand env args)
  (let ((old-seed *awk-rand-seed*))
    (if (null? args)
      (begin
        (set! *awk-rand-seed* (inexact->exact (floor (time->seconds (current-time)))))
        (set! *awk-rand-state* (make-random-source))
        (random-source-pseudo-randomize! *awk-rand-state* *awk-rand-seed* 0)
        (make-awk-number old-seed))
      (begin
        (set! *awk-rand-seed* (inexact->exact (floor (awk->number (car args)))))
        (set! *awk-rand-state* (make-random-source))
        (random-source-pseudo-randomize! *awk-rand-state* *awk-rand-seed* 0)
        (make-awk-number old-seed)))))
