#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :gerbil/compiler
        :std/build-script)

(defbuild-script
  '("awk/value"
    "awk/ast"
    "awk/lexer"
    "awk/parser"
    "awk/runtime"
    "awk/fields"
    "awk/builtins/string"
    "awk/builtins/math"
    "awk/builtins/io"
    (exe: "awk/main" bin: "gawk"))
  libdir: (path-normalize (path-directory (this-source-file))))
