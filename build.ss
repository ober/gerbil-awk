#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script
        :std/make)

(defbuild-script
  `("awk/value"
    "awk/ast"
    "awk/lexer"
    "awk/parser"
    "awk/runtime"
    "awk/fields"
    "awk/builtins/string"
    "awk/builtins/math"
    "awk/builtins/io"
    (exe: "awk/main" bin: "gawk"
          "-cc-options" ,(cppflags "libpcre2-8" "")
          "-ld-options" ,(ldflags "libpcre2-8" "-lpcre2-8")))
  libdir: (path-normalize (path-directory (this-source-file))))
