;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (documentation render-markdown)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-11)
  #:use-module (chip-remote utilities)
  #:export (list->markdown))

(define (output-mdwn-req-args args)
  (for-each (lambda (x) (format #t " ~a" x))
            args))

(define (output-mdwn-opt-args args)
  (for-each (lambda (x) (format #t " [~a]" x))
            args))

(define (output-mdwn-kw-args args)
  (for-each (lambda (x) (format #t " ~a ~a" x (symbol-upcase (keyword->symbol x))))
            (map car args)))

(define (output-mdwn-rest-args args)
  (when args (format #t " . ~a" args)))

(define (decode-args args)
  (values (assq-ref args 'required)
          (assq-ref args 'optional)
          (assq-ref args 'keyword)
          (assq-ref args 'allow-other-keys?)
          (assq-ref args 'rest)))

(define (verbatim? str)
  (and (> (string-length str) 3)
       (string= (substring str 0 4)
                "    ")))

(define (maybe->string symbol)
  (or (and (symbol? symbol)
           (symbol->string symbol))
      symbol))

(define (deparagraph docstring)
  (define (combine acc)
    (cond ((null? acc) acc)
          ((= 1 (length acc)) acc)
          (else (list (string-join acc " ")))))
  (define (committer? str)
    (or (string-null? str)
        (verbatim? str)))
  (let loop ((input (string-split (maybe->string docstring) #\newline))
             (output '())
             (acc '()))
    (if (null? input)
        (append output (combine acc))
        (let ((this (car input))
              (rest (cdr input)))
          (if (committer? this)
              (loop rest (append output (combine acc) (list this)) '())
              (loop rest output (append acc (list this))))))))

(define (reformat-word word)
  (let* ((pm (string-match "^[(:;,.!?]*" word))
         (prefix (match:substring pm))
         (sm (string-match "[:;,.!?)]*$" word))
         (suffix (match:substring sm))
         (word-len (string-length word))
         (prefix-len (string-length prefix))
         (suffix-len (string-length suffix))
         (rest-len (- word-len suffix-len))
         (real-word (if (zero? rest-len)
                        word
                        (substring word prefix-len rest-len))))
    (format (current-error-port) "DEBUG: reformat-word ~a~%" real-word)
    (cat prefix
         (cond ((string-prefix? "#:" real-word) (cat "`" real-word "`"))
               ((string= "#t" real-word) (cat "`" real-word "`"))
               ((string= "#f" real-word) (cat "`" real-word "`"))
               ((and (string-prefix? "~" real-word)
                     (string-suffix? "~" real-word))
                (cat "`"
                     (substring real-word 1 (- (string-length real-word) 1))
                     "`"))
               ((and (string-prefix? "‘" real-word)
                     (string-suffix? "’" real-word))
                (cat "‘`"
                     (substring real-word 1 (- (string-length real-word) 1))
                     "`’"))
               (else real-word))
         suffix)))

(define (reformat-line line)
  (if (verbatim? line)
      line
      (string-join (map reformat-word (string-split line #\space)) " ")))

(define (reformat-docstring docstring)
  (let loop ((input (map reformat-line (deparagraph docstring)))
             (acc '())
             (first? #t))
    (if (null? input)
        (reverse acc)
        (loop (cdr input)
              (cons (format #f "~c   ~a" (if first? #\: #\space) (car input))
                    acc)
              #f))))

(define (docstring->markdown str)
  (let ((docs (reformat-docstring str)))
    (for-each (lambda (x)
                (display x)
                (newline))
              docs)))

(define (output-mdwn-procedure name data)
  (match data
    ((docstring args arity)
     (let-values (((req-args opt-args kw-args other? rest) (decode-args args)))
       (format #t "Procedure: `(~a" name)
       (output-mdwn-req-args req-args)
       (output-mdwn-opt-args opt-args)
       (output-mdwn-kw-args kw-args)
       (output-mdwn-rest-args rest)
       (format #t ")`~%~%")
       (docstring->markdown docstring)))))

(define (output-mdwn-macro name data)
  (match data
    ((docstring args arity)
     (format #t "Macro: `(~a ...)`~%" name)
     (docstring->markdown docstring))))

(define (output-mdwn-integer name data)
  (match data
    ((docstring)
     (format #t "Integer: `~a`~%" name)
     (docstring->markdown docstring))))

(define (output-markdown item)
  (match item
    ((name 'procedure . rest) (output-mdwn-procedure name rest))
    ((name 'macro . rest) (output-mdwn-macro name rest))
    ((name 'integer . rest) (output-mdwn-integer name rest))
    (else (format #t "Unknown Type: `~a`~%:   undocumented~%" item))))

(define* (list->markdown source
                         #:key
                         (entry-hook identity)
                         (exit-hook identity)
                         (interitem-hook (lambda (item) (newline))))
  (entry-hook source)
  (let loop ((rest source))
    (if (null? rest)
        (exit-hook source)
        (let ((this (car rest))
              (rest (cdr rest)))
          (output-markdown this)
          (unless (null? rest)
            (interitem-hook this))
          (loop rest)))))
