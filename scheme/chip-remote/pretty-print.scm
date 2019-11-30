;; Copyright (c) 2019 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote pretty-print)
  #:use-module (ice-9 optargs)
  #:use-module ((ice-9 pretty-print) #:prefix i9pp:)
  #:use-module (chip-remote item)
  #:use-module (chip-remote manufacturer)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register-map)
  #:export (pp-indent
            pp-record
            make-printer
            make-printer/assoc
            make-printer/list
            make-printer/object
            make-printer/pp
            make-printer/record))

(define* (pp-indent #:optional (kind 'default))
  (case kind
    ((default) 4)
    ((complex) 2)
    ((list)    1)
    (else      0)))

(define (pp-prefix port)
  (display "#<" port))

(define (pp-suffix port)
  (write-char #\> port))

(define (pp-type port name)
  (write-char #\< port)
  (display name port)
  (write-char #\> port))

(define (pp-do-indent port indent)
  (let loop ((n indent))
    (unless (zero? n)
      (write-char #\space port)
      (loop (1- n)))))

(define (pp-key-value port indent key value)
  (write-char #\newline port)
  (pp-do-indent port indent)
  (display key port)
  (write-char #\: port)
  (if (thunk? value)
      (value)
      (begin (write-char #\space port)
             (write value port))))

(define (pp-record port name cb)
  (pp-prefix port)
  (pp-type port name)
  (cb)
  (pp-suffix port))

(define (make-printer port indent)
  (lambda (key value)
    (pp-key-value port indent key value)))

(define (i9pp port indent obj)
  (i9pp:pretty-print obj
                     #:port port
                     #:per-line-prefix (make-string indent #\space)
                     #:width 120
                     #:max-expr-width 60))

(define (pp-pp port indent key value)
  (write-char #\newline port)
  (pp-do-indent port indent)
  (display key port)
  (write-char #\: port)
  (write-char #\newline port)
  (i9pp port (+ (pp-indent 'complex) indent) value))

(define (make-printer/pp port indent)
  (lambda (key value)
    (pp-pp port indent key value)))

(define (make-printer/record port indent)
  (lambda (key value printer)
    (pp-key-value port indent key
                  (lambda ()
                    (write-char #\newline port)
                    (let ((base (+ (pp-indent 'complex) indent)))
                      (pp-do-indent port base)
                      (printer port (+ base (pp-indent 'complex)) value))))))

(define (atom? obj)
  (not (or (pair? obj)
           (record? obj))))

(define (pp-pair port indent key value simple first?)
  ;;(format port "{indent: ~a}" indent)
  (unless first? (pp-do-indent port indent))
  (write-char #\( port)
  (display key port)
  (display " ." port)
  (if (or (atom? value)
          (member key simple))
      (begin (write-char #\space port)
             ;;(write-char #\< port)
             ;;(write-char #\> port)
             (display value port))
      (begin (write-char #\newline port)
             ;;(write-char #\# port)
             (pp-object port
                        (+ (pp-indent 'complex)
                           (pp-indent 'list)
                           indent)
                        value)))
  (write-char #\) port))

(define* (pp-assoc port indent key value #:key (simple '()))
  (write-char #\newline port)
  (pp-do-indent port indent)
  (display key port)
  (write-char #\: port)
  (write-char #\newline port)
  (pp-do-indent port (+ (pp-indent 'complex) indent))
  (write-char #\( port)
  (let loop ((rest value)
             (indent (+ indent (pp-indent 'complex) (pp-indent 'list)))
             (first? #t))
    (unless (null? rest)
      (let ((this (car rest)))
        (unless first?
          (write-char #\newline port))
        (pp-pair port indent (car this) (cdr this) simple first?))
      (loop (cdr rest) indent #f)))
  (write-char #\) port)
  )

(define* (make-printer/assoc port indent #:key (simple '()))
  (lambda (key value)
    (pp-assoc port indent key value #:simple simple)))

(define (pp-list port indent key value)
  (write-char #\newline port)
  (pp-do-indent port indent)
  (display key port)
  (write-char #\: port)
  (write-char #\newline port)
  (pp-do-indent port (+ (pp-indent 'complex) indent))
  (write-char #\( port)
  (let loop ((rest value)
             (indent (+ indent (pp-indent 'complex) (pp-indent 'list)))
             (first? #t))
    (unless (null? rest)
      (let ((this (car rest)))
        (unless first?
          (write-char #\newline port))
        (pp-object port indent this #:first? first?))
      (loop (cdr rest) indent #f)))
  (write-char #\) port))

(define (make-printer/list port indent)
  (lambda (key value)
    (pp-list port indent key value)))

(define* (pp-object port indent obj #:key (first? #f))
  ;;(format port "[indent: ~a]" indent)
  (unless first? (pp-do-indent port indent))
  (cond
   ((manufacturer? obj)
    ((@@ (chip-remote manufacturer) pp-manufacturer) port (+ indent (pp-indent 'complex)) obj))
   ((item? obj)
    ((@@ (chip-remote item) pp-item) port (+ indent (pp-indent 'complex)) obj))
   ((register? obj)
    ((@@ (chip-remote register) pp-register) port (+ indent (pp-indent 'complex)) obj))
   ((register-map? obj)
    ((@@ (chip-remote register-map) pp-register-map) port (+ indent (pp-indent 'complex)) obj))
   ((page-map? obj)
    ((@@ (chip-remote page-map) pp-page-map) port (+ indent (pp-indent 'complex)) obj))
   (else (display obj port))))

(define (make-printer/object port indent)
  ;;(format port "[indent: ~a]" indent)
  (lambda (key value)
    (format port "[key: ~a] [value: ~a] [indent: ~a]~%" key value indent)
    (write-char #\newline port)
    (pp-do-indent port indent)
    (display key port)
    (write-char #\: port)
    (write-char #\newline port)
    (pp-do-indent port (+ (pp-indent 'complex) indent))
    (pp-object port indent value #:first? #f)))
