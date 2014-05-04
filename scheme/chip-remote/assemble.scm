;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote assemble)
  #:export (with-constraints

            value->bits
            value->twos-complement
            set-logic-active-high
            set-logic-active-low
            unset-logic-active-high
            unset-logic-active-low))

(define-syntax with-constraints
  (lambda (x)
    (syntax-case x ()
      ((_ (v (pred lim ...) ...) exp0 exp ...)
       #'(if (not (and (pred v lim ...) ...))
             (throw 'cr-out-of-limit
                    `(v . ,v) (quasiquote (pred v ,lim ...)) ...)
             (begin exp0 exp ...))))))

(define (value->bits tab sym)
  (let ((val (assq sym tab)))
    (if (not val)
        (throw 'cr-unknown-value sym))
    (cdr val)))

(define (set-logic-active-high cb regval)
  (cb regval 1))

(define (unset-logic-active-high cb regval)
  (cb regval 0))

(define (set-logic-active-low cb regval)
  (cb regval 0))

(define (unset-logic-active-low cb regval)
  (cb regval 1))

(define (value->twos-complement value width)
  (let ((base (ash 1 width)))
    (if (< value 0)
        (logior base (logxor (- base 1)
                             (- (* -1 value) 1)))
        value)))
