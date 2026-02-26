#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script
        :std/make)

;; Library-only mode: compile modules without linking the executable
(def lib-only? (and (getenv "GERBAWK_LIB_ONLY" #f) #t))

;; Static binary build: add -static to linker flags
(def static? (and (getenv "GERBAWK_STATIC" #f) #t))

(def static-ld-opts (if static? "-static " ""))

;; Core modules — always compiled regardless of build mode
(def core-modules
  '("awk/value"
    "awk/ast"
    "awk/lexer"
    "awk/parser"
    "awk/runtime"
    "awk/fields"
    "awk/builtins/string"
    "awk/builtins/math"
    "awk/builtins/io"))

;; Full module list depends on build mode
(def all-modules
  (if lib-only?
    ;; Library mode: core modules + main (as library) + lib entry point, no exe
    (append core-modules '("awk/main" "awk/lib"))
    ;; Executable mode: core modules + exe
    (append core-modules
            `((exe: "awk/main" bin: "gerbawk"
                    "-cc-options" ,(cppflags "libpcre2-8" "")
                    "-ld-options" ,(string-append static-ld-opts
                                                  (ldflags "libpcre2-8" "-lpcre2-8")))))))

(defbuild-script
  all-modules
  libdir: (path-normalize (path-directory (this-source-file))))
