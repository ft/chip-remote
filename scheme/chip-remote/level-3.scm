;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote level-3)
  #:export (define-bit-field-frontend
            define-bit-field-frontends))

(define-syntax define-bit-field-frontend
  (lambda (x)
    (syntax-case x ()
      ((_ name addr lvl2 (args ...))
       #'(define-public (name conn args ...)
           (write-register conn addr
                           (lvl2 (read-register conn addr) args ...))))
      ((_ name addr lvl2)
       #'(define-bit-field-frontend name addr lvl2 ())))))

(define-syntax define-bit-field-frontends
  (lambda (x)
    (syntax-case x ()
      ((_ (entry ...) ...)
       #'(begin (define-bit-field-frontend entry ...) ...)))))
