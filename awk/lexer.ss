;; -*- Gerbil -*-
;;;; AWK Lexer

(export #t)

;;; Token

(defstruct tok
  (type value line column)
  transparent: #t)

;;; Lexer state

(defstruct lexer
  (input pos len line column peeked last-type)
  transparent: #t)

(def (make-awk-lexer input)
  (make-lexer input 0 (string-length input) 1 1 #f #f))

;;; Keywords

(def +keywords+
  '(BEGIN END if else while for do break continue
    function return delete exit next nextfile
    print printf getline in))

;;; Character utilities

(def (lex-ch lex)
  (if (>= (lexer-pos lex) (lexer-len lex))
    #f
    (string-ref (lexer-input lex) (lexer-pos lex))))

(def (lex-ch+ lex offset)
  (let ((p (+ (lexer-pos lex) offset)))
    (if (>= p (lexer-len lex))
      #f
      (string-ref (lexer-input lex) p))))

(def (lex-advance! lex)
  (let ((c (lex-ch lex)))
    (when c
      (set! (lexer-pos lex) (+ (lexer-pos lex) 1))
      (if (char=? c #\newline)
        (begin (set! (lexer-line lex) (+ (lexer-line lex) 1))
               (set! (lexer-column lex) 1))
        (set! (lexer-column lex) (+ (lexer-column lex) 1))))
    c))

(def (lex-skip-ws! lex)
  "Skip whitespace except newlines"
  (let loop ()
    (let ((c (lex-ch lex)))
      (when (and c (char-whitespace? c) (not (char=? c #\newline)))
        (lex-advance! lex) (loop)))))

(def (lex-skip-line! lex)
  "Skip to end of line (for comments)"
  (let loop ()
    (let ((c (lex-ch lex)))
      (when (and c (not (char=? c #\newline)))
        (lex-advance! lex) (loop)))))

(def (lex-skip! lex)
  "Skip whitespace and comments (not newlines unless after backslash)"
  (let loop ()
    (lex-skip-ws! lex)
    (let ((c (lex-ch lex)))
      (cond
        ((and c (char=? c #\#))
         (lex-skip-line! lex) (loop))
        ((and c (char=? c #\\) (let ((n (lex-ch+ lex 1))) (and n (char=? n #\newline))))
         ;; Line continuation
         (lex-advance! lex) (lex-advance! lex) (loop))
        (else (void))))))

;;; Token reading helpers

(def (value-token? type)
  "Can this token type precede division (vs regex)?"
  (and type (memq type '(NUMBER STRING NAME RPAREN RBRACKET DOLLAR PLUSPLUS MINUSMINUS))))

(def (make-tok! lex type value)
  (let ((t (make-tok type value (lexer-line lex) (lexer-column lex))))
    (set! (lexer-last-type lex) type)
    t))

;;; Number reading

(def (read-number lex)
  (let ((line (lexer-line lex))
        (col (lexer-column lex))
        (c (lex-ch lex)))
    ;; Hex: 0x...
    (if (and (char=? c #\0) (let ((n (lex-ch+ lex 1))) (and n (char-ci=? n #\x))))
      (begin (lex-advance! lex) (lex-advance! lex)
        (let loop ((chars '()))
          (let ((c (lex-ch lex)))
            (if (and c (or (char-numeric? c) (and (char-ci>=? c #\a) (char-ci<=? c #\f))))
              (begin (lex-advance! lex) (loop (cons c chars)))
              (let* ((s (list->string (reverse chars)))
                     (n (or (string->number (string-append "0x" s) 16) 0)))
                (set! (lexer-last-type lex) 'NUMBER)
                (make-tok 'NUMBER n line col))))))
      ;; Regular number
      (let loop ((chars '()) (has-dot? #f) (has-exp? #f))
        (let ((c (lex-ch lex)))
          (cond
            ((and c (char-numeric? c))
             (lex-advance! lex) (loop (cons c chars) has-dot? has-exp?))
            ((and c (char=? c #\.) (not has-dot?) (not has-exp?))
             (lex-advance! lex) (loop (cons c chars) #t has-exp?))
            ((and c (char-ci=? c #\e) (not has-exp?))
             (lex-advance! lex)
             (let ((c2 (lex-ch lex)))
               (if (and c2 (or (char=? c2 #\+) (char=? c2 #\-)))
                 (begin (lex-advance! lex)
                        (loop (cons c2 (cons #\e chars)) has-dot? #t))
                 (loop (cons #\e chars) has-dot? #t))))
            (else
             (let* ((s (list->string (reverse chars)))
                    (n (or (string->number s) 0)))
               (set! (lexer-last-type lex) 'NUMBER)
               (make-tok 'NUMBER n line col)))))))))

;;; String reading

(def (read-string lex)
  (let ((line (lexer-line lex))
        (col (lexer-column lex)))
    (lex-advance! lex) ;; skip opening "
    (let loop ((chars '()))
      (let ((c (lex-ch lex)))
        (cond
          ((not c) (error "unterminated string" line))
          ((char=? c #\")
           (lex-advance! lex)
           (set! (lexer-last-type lex) 'STRING)
           (make-tok 'STRING (list->string (reverse chars)) line col))
          ((char=? c #\\)
           (lex-advance! lex)
           (let ((e (lex-ch lex)))
             (if e
               (begin (lex-advance! lex)
                      (loop (cons (escape-char e) chars)))
               (error "unterminated string escape" line))))
          (else
           (lex-advance! lex)
           (loop (cons c chars))))))))

(def (escape-char c)
  (case c
    ((#\a) #\alarm)
    ((#\b) #\backspace)
    ((#\f) #\page)
    ((#\n) #\newline)
    ((#\r) #\return)
    ((#\t) #\tab)
    ((#\v) #\vtab)
    ((#\\) #\\)
    ((#\") #\")
    ((#\/) #\/)
    (else
     ;; Octal escape \NNN
     (if (and (char>=? c #\0) (char<=? c #\7))
       (integer->char (- (char->integer c) (char->integer #\0)))
       c))))

;;; Regex reading

(def (read-regex lex)
  (let ((line (lexer-line lex))
        (col (lexer-column lex)))
    (lex-advance! lex) ;; skip opening /
    (let loop ((chars '()))
      (let ((c (lex-ch lex)))
        (cond
          ((not c) (error "unterminated regex" line))
          ((char=? c #\/)
           (lex-advance! lex)
           (set! (lexer-last-type lex) 'REGEX)
           (make-tok 'REGEX (list->string (reverse chars)) line col))
          ((char=? c #\\)
           (lex-advance! lex)
           (let ((e (lex-ch lex)))
             (when e (lex-advance! lex))
             (loop (cons (or e #\\) (cons #\\ chars)))))
          (else
           (lex-advance! lex)
           (loop (cons c chars))))))))

;;; Name reading

(def (read-name lex)
  (let ((line (lexer-line lex))
        (col (lexer-column lex)))
    (let loop ((chars '()))
      (let ((c (lex-ch lex)))
        (if (and c (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)))
          (begin (lex-advance! lex) (loop (cons c chars)))
          (let ((name (string->symbol (list->string (reverse chars)))))
            (if (memq name +keywords+)
              (begin (set! (lexer-last-type lex) name)
                     (make-tok name name line col))
              (begin (set! (lexer-last-type lex) 'NAME)
                     (make-tok 'NAME name line col)))))))))

;;; Main lexer

(def (lex-next! lex)
  "Get next token"
  (if (lexer-peeked lex)
    (let ((t (lexer-peeked lex)))
      (set! (lexer-peeked lex) #f)
      (set! (lexer-last-type lex) (tok-type t))
      t)
    (begin
      (lex-skip! lex)
      (let ((c (lex-ch lex))
            (line (lexer-line lex))
            (col (lexer-column lex)))
        (cond
          ((not c) (make-tok 'EOF #f line col))
          ((char=? c #\newline)
           (lex-advance! lex)
           (make-tok 'NEWLINE #f line col))
          ((char-numeric? c) (read-number lex))
          ((char=? c #\.)
           ;; Could be .5 style number
           (let ((n (lex-ch+ lex 1)))
             (if (and n (char-numeric? n))
               (read-number lex)
               (begin (lex-advance! lex) (make-tok! lex 'DOT ".")))))
          ((char=? c #\") (read-string lex))
          ((or (char-alphabetic? c) (char=? c #\_)) (read-name lex))
          ((char=? c #\$)
           (lex-advance! lex) (make-tok! lex 'DOLLAR #f))
          ((char=? c #\()
           (lex-advance! lex) (make-tok! lex 'LPAREN "("))
          ((char=? c #\))
           (lex-advance! lex) (make-tok! lex 'RPAREN ")"))
          ((char=? c #\[)
           (lex-advance! lex) (make-tok! lex 'LBRACKET "["))
          ((char=? c #\])
           (lex-advance! lex) (make-tok! lex 'RBRACKET "]"))
          ((char=? c #\{)
           (lex-advance! lex) (make-tok! lex 'LBRACE "{"))
          ((char=? c #\})
           (lex-advance! lex) (make-tok! lex 'RBRACE "}"))
          ((char=? c #\;)
           (lex-advance! lex) (make-tok! lex 'SEMI ";"))
          ((char=? c #\,)
           (lex-advance! lex) (make-tok! lex 'COMMA ","))
          ((char=? c #\?)
           (lex-advance! lex) (make-tok! lex 'QUESTION "?"))
          ((char=? c #\:)
           (lex-advance! lex) (make-tok! lex 'COLON ":"))
          ((char=? c #\+)
           (lex-advance! lex)
           (let ((n (lex-ch lex)))
             (cond
               ((and n (char=? n #\+)) (lex-advance! lex) (make-tok! lex 'PLUSPLUS "++"))
               ((and n (char=? n #\=)) (lex-advance! lex) (make-tok! lex 'PLUSEQ "+="))
               (else (make-tok! lex 'PLUS "+")))))
          ((char=? c #\-)
           (lex-advance! lex)
           (let ((n (lex-ch lex)))
             (cond
               ((and n (char=? n #\-)) (lex-advance! lex) (make-tok! lex 'MINUSMINUS "--"))
               ((and n (char=? n #\=)) (lex-advance! lex) (make-tok! lex 'MINUSEQ "-="))
               (else (make-tok! lex 'MINUS "-")))))
          ((char=? c #\*)
           (lex-advance! lex)
           (let ((n (lex-ch lex)))
             (cond
               ((and n (char=? n #\*)) (lex-advance! lex) (make-tok! lex 'STARSTAR "**"))
               ((and n (char=? n #\=)) (lex-advance! lex) (make-tok! lex 'STAREQ "*="))
               (else (make-tok! lex 'STAR "*")))))
          ((char=? c #\/)
           (if (value-token? (lexer-last-type lex))
             ;; Division
             (begin
               (lex-advance! lex)
               (let ((n (lex-ch lex)))
                 (if (and n (char=? n #\=))
                   (begin (lex-advance! lex) (make-tok! lex 'SLASHEQ "/="))
                   (make-tok! lex 'SLASH "/"))))
             ;; Regex
             (read-regex lex)))
          ((char=? c #\%)
           (lex-advance! lex)
           (let ((n (lex-ch lex)))
             (if (and n (char=? n #\=))
               (begin (lex-advance! lex) (make-tok! lex 'PERCENTEQ "%="))
               (make-tok! lex 'PERCENT "%"))))
          ((char=? c #\^)
           (lex-advance! lex)
           (let ((n (lex-ch lex)))
             (if (and n (char=? n #\=))
               (begin (lex-advance! lex) (make-tok! lex 'CARETEQ "^="))
               (make-tok! lex 'CARET "^"))))
          ((char=? c #\<)
           (lex-advance! lex)
           (let ((n (lex-ch lex)))
             (if (and n (char=? n #\=))
               (begin (lex-advance! lex) (make-tok! lex 'LE "<="))
               (make-tok! lex 'LT "<"))))
          ((char=? c #\>)
           (lex-advance! lex)
           (let ((n (lex-ch lex)))
             (cond
               ((and n (char=? n #\>)) (lex-advance! lex) (make-tok! lex 'APPEND ">>"))
               ((and n (char=? n #\=)) (lex-advance! lex) (make-tok! lex 'GE ">="))
               (else (make-tok! lex 'GT ">")))))
          ((char=? c #\=)
           (lex-advance! lex)
           (let ((n (lex-ch lex)))
             (if (and n (char=? n #\=))
               (begin (lex-advance! lex) (make-tok! lex 'EQ "=="))
               (make-tok! lex 'ASSIGN "="))))
          ((char=? c #\!)
           (lex-advance! lex)
           (let ((n (lex-ch lex)))
             (cond
               ((and n (char=? n #\=)) (lex-advance! lex) (make-tok! lex 'NE "!="))
               ((and n (char=? n #\~)) (lex-advance! lex) (make-tok! lex 'NOMATCH "!~"))
               (else (make-tok! lex 'NOT "!")))))
          ((char=? c #\~)
           (lex-advance! lex) (make-tok! lex 'MATCH "~"))
          ((char=? c #\&)
           (lex-advance! lex)
           (let ((n (lex-ch lex)))
             (if (and n (char=? n #\&))
               (begin (lex-advance! lex) (make-tok! lex 'AND "&&"))
               (error "unexpected &" line col))))
          ((char=? c #\|)
           (lex-advance! lex)
           (let ((n (lex-ch lex)))
             (if (and n (char=? n #\|))
               (begin (lex-advance! lex) (make-tok! lex 'OR "||"))
               (make-tok! lex 'PIPE "|"))))
          (else
           (error "unexpected character" c line col)))))))

(def (lex-peek! lex)
  "Peek at next token"
  (unless (lexer-peeked lex)
    (set! (lexer-peeked lex) (lex-next! lex)))
  (lexer-peeked lex))

(def (lex-skip-newlines! lex)
  "Skip any newline tokens"
  (let loop ()
    (let ((t (lex-peek! lex)))
      (when (eq? (tok-type t) 'NEWLINE)
        (lex-next! lex)
        (loop)))))
