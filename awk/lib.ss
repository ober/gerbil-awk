;;; lib.ss — Library API for embedding gerbil-gawk in other Gerbil projects
;;;
;;; Quick start:
;;;   (import :gerbil-awk/awk/lib)
;;;   (run-awk '("-F" "," "{print $1}" "data.csv"))
;;;
;;; For advanced use, import individual modules directly:
;;;   (import :gerbil-awk/awk/parser :gerbil-awk/awk/runtime :gerbil-awk/awk/value)

(export run-awk
        ;; Parser
        parse-awk-string
        ;; Runtime environment
        make-initial-env
        env-get env-set!
        env-get-str env-get-array
        env-array-ref env-array-set! env-array-exists? env-array-delete!
        env-get-field env-set-field!
        env-split-fields
        env-init-argv! env-init-environ!
        env-close-all!
        ;; Value system
        make-awk-number make-awk-string make-awk-strnum make-awk-uninit
        awk->number awk->string awk->bool
        ;; AST types
        awk-program? awk-program-rules awk-program-functions)

(import ./lexer ./parser ./ast ./value ./runtime
        ./builtins/string ./builtins/math ./builtins/io
        ./main)
