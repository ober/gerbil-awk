;; -*- Gerbil -*-
;;;; AWK Lexer
;;
;; Tokenizes AWK source code. Handles:
;; - Keywords
;; - Operators (including multi-char like +=, &&, etc.)
;; - Numbers (decimal, hex, octal)
;; - Strings (with escape sequences)
;; - Regex literals (/pattern/)
;; - Regex vs division disambiguation

(export #t)

;;; Token types

(defstruct tok
  (type        ; symbol
   value       ; token value (string, number, etc.)
   line        ; line number
   column)     ; column number
  transparent: #t)

;;; Lexer state

(defstruct lexer
  (input       ; string
   pos         ; current position
   len         ; input length
   line        ; current line
   column      ; current column
   peeked      ; peeked token or #f
   last-token) ; last token returned (for regex/division disambiguation)
  transparent: #t)

(def (make-lexer-from-string input)
  (make-lexer input 0 (string-length input) 1 1 #f #f))

;;; Keywords

(def +keywords+
  '(BEGIN END BEGINFILE ENDFILE
    if else while for do break continue
    function return delete exit next nextfile
    switch case default
    print printf getline in))

;;; Token type predicates

(def (value-producing-token? tok)
  "Check if token can precede division (vs regex)"
  (and tok
       (memq (tok-type tok)
             '(NUMBER STRING NAME REGEX RPAREN RBRACKET DOLLAR))))

(def (keyword? name)
  (memq name +keywords+))

;;; Character utilities

(def (lexer-char lex)
  (let ((pos (lexer-pos lex)))
    (if (fx>= pos (lexer-len lex))
      #f
      (string-ref (lexer-input lex) pos))))

(def (lexer-char-at lex offset)
  (let ((pos (fx+ (lexer-pos lex) offset)))
    (if (fx>= pos (lexer-len lex))
      #f
      (string-ref (lexer-input lex) pos))))

(def (lexer-advance! lex)
  (let ((c (lexer-char lex)))
    (when c
      (set! (lexer-pos lex) (fx+ (lexer-pos lex) 1))
      (if (char=? c #\newline)
        (begin
          (set! (lexer-line lex) (fx+ (lexer-line lex) 1))
          (set! (lexer-column lex) 1))
        (set! (lexer-column lex) (fx+ (lexer-column lex) 1))))
    c))

(def (lexer-skip-whitespace! lex)
  "Skip whitespace but NOT newlines (newlines are significant in AWK)"
  (let loop ()
    (let ((c (lexer-char lex)))
      (when (and c (char? c) (char-whitespace? c) (not (char=? c #\newline)))
        (lexer-advance! lex)
        (loop)))))

(def (lexer-skip-comment! lex)
  "Skip from # to end of line"
  (let loop ()
    (let ((c (lexer-char lex)))
      (when (and c (not (char=? c #\newline)))
        (lexer-advance! lex)
        (loop)))))

(def (lexer-skip-whitespace-and-comments! lex (skip-newlines? #f))
  "Skip whitespace and comments. If skip-newlines?, skip newlines too."
  (let loop ()
    (let ((c (lexer-char lex)))
      (cond
        ((not c) #f)
        ((and (char-whitespace? c)
              (or skip-newlines? (not (char=? c #\newline))))
         (lexer-advance! lex)
         (loop))
        ((char=? c #\#)
         (lexer-skip-comment! lex)
         (loop))
        (else #f)))))

;;; Token reading

(def (read-number lex)
  "Read a number literal (decimal, hex, or octal)"
  (let* ((start-pos (lexer-pos lex))
         (start-line (lexer-line lex))
         (start-col (lexer-column lex))
         (c (lexer-char lex)))
    (cond
      ;; Hex number: 0x...
      ((and (char=? c #\0)
            (let ((next (lexer-char-at lex 1)))
              (and next (char-ci=? next #\x))))
       (lexer-advance! lex) ; 0
       (lexer-advance! lex) ; x
       (let loop ((chars '(#\x #\0)))
         (let ((c (lexer-char lex)))
           (if (and c (or (char-numeric? c)
                          (and (char-ci>=? c #\a) (char-ci<=? c #\f))))
             (begin (lexer-advance! lex) (loop (cons c chars)))
             (let ((hex-str (list->string (reverse chars))))
               (make-tok 'NUMBER (string->number hex-str 16) start-line start-col))))))
      ;; Regular number
      (else
       (let loop ((chars '())
                  (has-dot? #f)
                  (has-exp? #f))
         (let ((c (lexer-char lex)))
           (cond
             ((and c (char-numeric? c))
              (lexer-advance! lex)
              (loop (cons c chars) has-dot? has-exp?))
             ((and c (char=? c #\.) (not has-dot?) (not has-exp?))
              (lexer-advance! lex)
              (loop (cons c chars) #t has-exp?))
             ((and c (or (char-ci=? c #\e)) (not has-exp?))
              (lexer-advance! lex)
              (let ((c (lexer-char lex)))
                (if (and c (or (char=? c #\+) (char=? c #\-)))
                  (begin
                    (lexer-advance! lex)
                    (loop (cons c (cons #\e chars)) has-dot? #t))
                  (loop (cons #\e chars) has-dot? #t))))
             (else
              (let ((num-str (list->string (reverse chars))))
                (make-tok 'NUMBER (string->number num-str) start-line start-col))))))))))

(def (read-string lex)
  "Read a double-quoted string literal"
  (let ((start-line (lexer-line lex))
        (start-col (lexer-column lex)))
    (lexer-advance! lex) ; opening "
    (let loop ((chars '()))
      (let ((c (lexer-char lex)))
        (cond
          ((not c) (error "unterminated string" start-line))
          ((char=? c #\")
           (lexer-advance! lex)
           (make-tok 'STRING (list->string (reverse chars)) start-line start-col))
          ((char=? c #\\)
           (lexer-advance! lex)
           (let ((escaped (lexer-char lex)))
             (lexer-advance! lex)
             (loop (cons (escape-char escaped) chars))))
          (else
           (lexer-advance! lex)
           (loop (cons c chars))))))))

(def (escape-char c)
  "Convert escape sequence character"
  (case c
    ((#\a) #\alarm)
    ((#\b) #\backspace)
    ((#\f) #\page)
    ((#\n) #\newline)
    ((#\r) #\return)
    ((#\t) #\tab)
    ((#\v) #\vtab)
    ((#\") #\")
    ((#\\) #\\)
    ((#\/) #\/)
    (else c)))

(def (read-regex lex)
  "Read a regex literal /pattern/"
  (let ((start-line (lexer-line lex))
        (start-col (lexer-column lex)))
    (lexer-advance! lex) ; opening /
    (let loop ((chars '()))
      (let ((c (lexer-char lex)))
        (cond
          ((not c) (error "unterminated regex" start-line))
          ((char=? c #\/)
           (lexer-advance! lex)
           (make-tok 'REGEX (list->string (reverse chars)) start-line start-col))
          ((char=? c #\\)
           (lexer-advance! lex)
           (let ((escaped (lexer-char lex)))
             (lexer-advance! lex)
             (loop (cons escaped (cons c chars)))))
          (else
           (lexer-advance! lex)
           (loop (cons c chars))))))))

(def (read-name lex)
  "Read an identifier (variable/function name)"
  (let ((start-line (lexer-line lex))
        (start-col (lexer-column lex)))
    (let loop ((chars '()))
      (let ((c (lexer-char lex)))
        (if (and c (or (char-alphabetic? c)
                       (char-numeric? c)
                       (char=? c #\_)))
          (begin
            (lexer-advance! lex)
            (loop (cons c chars)))
          (let ((name (string->symbol (list->string (reverse chars)))))
            (if (keyword? name)
              (make-tok name name start-line start-col)
              (make-tok 'NAME name start-line start-col))))))))

;;; Main lexer functions

(def (lexer-next-token! lex)
  "Get the next token from the lexer"
  (when (lexer-peeked lex)
    (set! (lexer-peeked lex) #f)
    (let ((tok (lexer-peeked lex)))
      (when tok (set! (lexer-last-token lex) tok))
      (return tok)))
  
  (lexer-skip-whitespace-and-comments! lex)
  
  (let* ((c (lexer-char lex))
         (line (lexer-line lex))
         (col (lexer-column lex)))
    (cond
      ((not c) (make-tok 'EOF #f line col))
      
      ;; Newline (significant in AWK)
      ((char=? c #\newline)
       (lexer-advance! lex)
       (make-tok 'NEWLINE #f line col))
      
      ;; Number
      ((char-numeric? c)
       (let ((tok (read-number lex)))
         (set! (lexer-last-token lex) tok)
         tok))
      
      ;; String
      ((char=? c #\")
       (let ((tok (read-string lex)))
         (set! (lexer-last-token lex) tok)
         tok))
      
      ;; Name or keyword
      ((or (char-alphabetic? c) (char=? c #\_))
       (let ((tok (read-name lex)))
         (set! (lexer-last-token lex) tok)
         tok))
      
      ;; @directive (@include, @load, @namespace, @/regex/)
      ((char=? c #\@)
       (lexer-advance! lex)
       (let ((next (lexer-char lex)))
         (cond
           ((char=? next #\/)
            (let ((tok (read-regex lex)))
              (set! (tok-type tok) 'TYPED-REGEX)
              (set! (lexer-last-token lex) tok)
              tok))
           ((or (char-alphabetic? next) (char=? next #\_))
            (let ((tok (read-name lex)))
              (set! (tok-type tok) 'AT-DIRECTIVE)
              (set! (lexer-last-token lex) tok)
              tok))
           (else (error "invalid @ directive" line)))))
      
      ;; $ (field reference)
      ((char=? c #\$)
       (lexer-advance! lex)
       (let ((tok (make-tok 'DOLLAR #f line col)))
         (set! (lexer-last-token lex) tok)
         tok))
      
      ;; Operators
      ((char=? c #\+)
       (lexer-advance! lex)
       (if (char=? (lexer-char lex) #\+)
         (begin (lexer-advance! lex)
                (make-tok-and-save lex 'PLUSPLUS "++" line col))
         (if (char=? (lexer-char lex) #\=)
           (begin (lexer-advance! lex)
                  (make-tok-and-save lex 'PLUSEQ "+=" line col))
           (make-tok-and-save lex 'PLUS "+" line col))))
      
      ((char=? c #\-)
       (lexer-advance! lex)
       (if (char=? (lexer-char lex) #\-)
         (begin (lexer-advance! lex)
                (make-tok-and-save lex 'MINUSMINUS "--" line col))
         (if (char=? (lexer-char lex) #\=)
           (begin (lexer-advance! lex)
                  (make-tok-and-save lex 'MINUSEQ "-=" line col))
           (make-tok-and-save lex 'MINUS "-" line col))))
      
      ((char=? c #\*)
       (lexer-advance! lex)
       (if (char=? (lexer-char lex) #\*)
         (begin (lexer-advance! lex)
                (make-tok-and-save lex 'STARSTAR "**" line col))
         (if (char=? (lexer-char lex) #\=)
           (begin (lexer-advance! lex)
                  (make-tok-and-save lex 'STAREQ "*=" line col))
           (make-tok-and-save lex 'STAR "*" line col))))
      
      ((char=? c #\/)
       ;; CRITICAL: Regex vs division disambiguation
       ;; After a value-producing token, / is division
       ;; Otherwise, / starts a regex
       (if (value-producing-token? (lexer-last-token lex))
         (begin
           (lexer-advance! lex)
           (if (char=? (lexer-char lex) #\=)
             (begin (lexer-advance! lex)
                    (make-tok-and-save lex 'SLASHEQ "/=" line col))
             (make-tok-and-save lex 'SLASH "/" line col)))
         (let ((tok (read-regex lex)))
           (set! (lexer-last-token lex) tok)
           tok)))
      
      ((char=? c #\%)
       (lexer-advance! lex)
       (if (char=? (lexer-char lex) #\=)
         (begin (lexer-advance! lex)
                (make-tok-and-save lex 'PERCENTEQ "%=" line col))
         (make-tok-and-save lex 'PERCENT "%" line col)))
      
      ((char=? c #\^)
       (lexer-advance! lex)
       (if (char=? (lexer-char lex) #\=)
         (begin (lexer-advance! lex)
                (make-tok-and-save lex 'CARETEQ "^=" line col))
         (make-tok-and-save lex 'CARET "^" line col)))
      
      ((char=? c #\<)
       (lexer-advance! lex)
       (cond
         ((char=? (lexer-char lex) #\<) (lexer-advance! lex) (make-tok-and-save lex 'LSHIFT "<<" line col))
         ((char=? (lexer-char lex) #\=) (lexer-advance! lex) (make-tok-and-save lex 'LE "<=" line col))
         (else (make-tok-and-save lex 'LT "<" line col))))
      
      ((char=? c #\>)
       (lexer-advance! lex)
       (cond
         ((char=? (lexer-char lex) #\>) (lexer-advance! lex) (make-tok-and-save lex 'RSHIFT ">>" line col))
         ((char=? (lexer-char lex) #\=) (lexer-advance! lex) (make-tok-and-save lex 'GE ">=" line col))
         (else (make-tok-and-save lex 'GT ">" line col))))
      
      ((char=? c #\=)
       (lexer-advance! lex)
       (if (char=? (lexer-char lex) #\=)
         (begin (lexer-advance! lex)
                (make-tok-and-save lex 'EQ "==" line col))
         (make-tok-and-save lex 'ASSIGN "=" line col)))
      
      ((char=? c #\!)
       (lexer-advance! lex)
       (cond
         ((char=? (lexer-char lex) #\=) (lexer-advance! lex) (make-tok-and-save lex 'NE "!=" line col))
         ((char=? (lexer-char lex) #\~) (lexer-advance! lex) (make-tok-and-save lex 'NOMATCH "!~" line col))
         (else (make-tok-and-save lex 'NOT "!" line col))))
      
      ((char=? c #\~)
       (lexer-advance! lex)
       (make-tok-and-save lex 'MATCH "~" line col))
      
      ((char=? c #\&)
       (lexer-advance! lex)
       (if (char=? (lexer-char lex) #\&)
         (begin (lexer-advance! lex)
                (make-tok-and-save lex 'AND "&&" line col))
         (error "unexpected &" line)))
      
      ((char=? c #\|)
       (lexer-advance! lex)
       (cond
         ((char=? (lexer-char lex) #\|) (lexer-advance! lex) (make-tok-and-save lex 'OR "||" line col))
         ((char=? (lexer-char lex) #\&) (lexer-advance! lex) (make-tok-and-save lex 'PIPEAMP "|&" line col))
         (else (make-tok-and-save lex 'PIPE "|" line col))))
      
      ((char=? c #\?)
       (lexer-advance! lex)
       (make-tok-and-save lex 'QUESTION "?" line col))
      
      ((char=? c #\:)
       (lexer-advance! lex)
       (make-tok-and-save lex 'COLON ":" line col))
      
      ((char=? c #\,)
       (lexer-advance! lex)
       (make-tok-and-save lex 'COMMA "," line col))
      
      ((char=? c #\;)
       (lexer-advance! lex)
       (make-tok-and-save lex 'SEMI ";" line col))
      
      ((char=? c #\()
       (lexer-advance! lex)
       (make-tok-and-save lex 'LPAREN "(" line col))
      
      ((char=? c #\))
       (lexer-advance! lex)
       (let ((tok (make-tok 'RPAREN ")" line col)))
         (set! (lexer-last-token lex) tok)
         tok))
      
      ((char=? c #\[)
       (lexer-advance! lex)
       (make-tok-and-save lex 'LBRACKET "[" line col))
      
      ((char=? c #\])
       (lexer-advance! lex)
       (let ((tok (make-tok 'RBRACKET "]" line col)))
         (set! (lexer-last-token lex) tok)
         tok))
      
      ((char=? c #\{)
       (lexer-advance! lex)
       (make-tok-and-save lex 'LBRACE "{" line col))
      
      ((char=? c #\})
       (lexer-advance! lex)
       (make-tok-and-save lex 'RBRACE "}" line col))
      
      (else
       (error "unexpected character" c line col)))))

(def (make-tok-and-save lex type value line col)
  (let ((tok (make-tok type value line col)))
    (set! (lexer-last-token lex) tok)
    tok))

(def (lexer-peek-token! lex)
  "Peek at the next token without consuming it"
  (unless (lexer-peeked lex)
    (set! (lexer-peeked lex) (lexer-next-token! lex)))
  (lexer-peeked lex))

(def (lexer-skip-newlines! lex)
  "Skip any pending newlines"
  (let loop ()
    (let ((tok (lexer-peek-token! lex)))
      (when (and tok (eq? (tok-type tok) 'NEWLINE))
        (lexer-next-token! lex)
        (loop)))))
