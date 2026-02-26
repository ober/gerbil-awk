;; -*- Gerbil -*-
;;;; AWK I/O Built-in Functions

(import ../value ../runtime)
(export #t)

;;; close(file)
(def (awk-builtin-close env args)
  (make-awk-number (env-close-file! env (awk->string (car args)))))

;;; system(command)
(def (awk-builtin-system env args)
  (let* ((cmd (awk->string (car args)))
         (proc (open-process (list path: "/bin/sh"
                                   arguments: (list "-c" cmd)
                                   stdin-redirection: #f
                                   stdout-redirection: #f
                                   stderr-redirection: #f)))
         (status (process-status proc)))
    (close-port proc)
    ;; Return exit status (shell convention)
    (make-awk-number (if (integer? status) status -1))))

;;; fflush([file])
(def (awk-builtin-fflush env args)
  (if (null? args)
    (begin (force-output (current-output-port))
           (make-awk-number 0))
    (let ((name (awk->string (car args))))
      (if (string=? name "")
        (begin (force-output (current-output-port))
               (make-awk-number 0))
        (cond
          ((hash-key? (awk-env-output-files env) name)
           (force-output (hash-ref (awk-env-output-files env) name))
           (make-awk-number 0))
          (else (make-awk-number -1)))))))
