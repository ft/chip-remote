;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote level-3)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (chip-remote bitops)
  #:use-module (chip-remote decode)
  #:use-module (chip-remote decode to-text)
  #:export (define-bit-field-frontends
            device-decoder
            replay-register
            to-tty?
            value-decoder
            split-word))

(define-syntax replay-register
  (lambda (x)
    (syntax-case x ()
      ((_ c a l2 (args ...) pp)
       #'(let ((conn c) (addr a))
           (write-register conn addr
                           (pp (l2 (read-register conn addr) args ...)))))
      ((_ c a l2 (args ...))
       #'(let ((conn c) (addr a))
           (write-register conn addr (l2 (read-register conn addr) args ...))))
      ((_ c a l2 pp)
       #'(replay-register c a l2 () pp))
      ((_ c a l2)
       #'(replay-register c a l2 ())))))

(define-syntax define-bit-field-frontend
  (lambda (x)
    (syntax-case x ()
      ((_ name addr lvl2 (args ...) pp)
       #'(define-public (name conn args ...)
           (replay-register conn addr lvl2 (args ...) pp)))
      ((_ name addr lvl2 (args ...))
       #'(define-public (name conn args ...)
           (replay-register conn addr lvl2 (args ...))))
      ((_ name addr lvl2 pp)
       #'(define-bit-field-frontend name addr lvl2 () pp))
      ((_ name addr lvl2)
       #'(define-bit-field-frontend name addr lvl2 ())))))

(define-syntax define-bit-field-frontends
  (lambda (x)
    (syntax-case x ()
      ((_ (entry ...) ...)
       #'(begin (define-bit-field-frontend entry ...) ...)))))

(define (value-decoder regmap width addr value colour?)
  (display-list (register->text #:register-map regmap
                                #:address addr
                                #:width width
                                #:value value
                                #:colour? colour?)))

(define* (device-decoder #:key
                         register-map
                         reader
                         decoder
                         width
                         (colour? to-tty?)
                         (interconnections '())
                         (filter-predicate #f))
  (let ((data (registers->text #:register-map register-map
                               #:reader reader
                               #:decoder decoder
                               #:width width
                               #:colour? colour?
                               #:interconnections interconnections
                               #:filter-predicate filter-predicate)))
    (fold (lambda (x acc)
            (+ 1 x acc))
          0
          (map (lambda (x)
                 (let ((ret (display-list x)))
                   (newline)
                   ret))
               data))))

(define (to-tty?)
  (isatty? (current-output-port)))

(define (split-word data . widths)
  (cdr (fold (lambda (x acc)
               (let ((offset (car acc))
                     (rest (cdr acc)))
                 (cons (+ x offset)
                       (cons (bit-extract-width data offset x)
                             rest))))
             (list 0)
             (reverse widths))))
