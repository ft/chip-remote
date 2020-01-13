;; Copyright (c) 2019 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote pretty-print)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9 gnu)
  #:export (pp-indent
            pp-do-indent
            pp-eval*
            pp-eval
            pp-script
            pp-dispatch))

(define-immutable-record-type <pp-script>
  (pp-script script)
  pp-script?
  (script pp-get-script))

(define* (pp-indent #:optional (kind 'default))
  (if (integer? kind)
      kind
      (case kind
        ((default) 4)
        ((complex) 2)
        ((list)    1)
        (else      0))))

(define (pp-do-indent port indent)
  (let loop ((n indent))
    (unless (zero? n)
      (write-char #\space port)
      (loop (1- n)))))

(define (eval-indent port prg)
  (for-each (lambda (kind)
              (pp-do-indent port (pp-indent kind)))
            (if (list? prg)
                prg
                (list prg))))

(define (pp-eval* port script last-op indent)
  (match (if (pp-script? script)
             (pp-get-script script)
             script)
    (('wrap open close . rest)
     (format port "~a" open)
     (pp-eval* port rest last-op indent)
     (format port "~a~%" close)
     'wrap)

    (('indent kind . rest)
     (if (eq? last-op 'newline)
         (eval-indent port kind))
     (pp-eval* port rest last-op (cons kind indent))
     'indent)

    (('key key)
     (and (eq? last-op 'indent)
          (eval-indent port indent))
     (format port "~a:" key) 'key)

    (('space value)
     (display #\space port)
     (if (pp-script? value)
         (pp-eval* port value last-op (cons 10 indent))
         (format port "~a" value))
     'space)

    (('type type)
     (format port "<~a>" type)
     'type)

    ((expr . rest)
     (pp-eval* port rest
               (pp-eval* port expr last-op indent)
               indent))

    (() last-op)

    ('newline
     (newline port)
     (eval-indent port indent)
     'newline)

    (else (format #t "not-handled-yet ~a~%" prg)
          'unknown-instruction)))

(define (pp-eval port prg)
  (pp-eval* port prg 'nop '()))

(define-syntax pp-dispatch-table
  (lambda (x)
    (syntax-case x ()
      ((_ value (mod type? pp-type) ...)
       #'(cond (((@ (chip-remote mod) type?) value)
                ((@ (chip-remote mod) pp-type) value)) ...
                (else value))))))

(define (pp-dispatch x)
  (pp-dispatch-table x
                     (device device? pp-device)
                     (named-value named-value? pp-named-value)))
