;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 format)
             (test tap)
             (chip-remote bitops)
             (chip-remote legacy ti cdce72010 prg))
(primitive-load "tests/test-tap-cfg.scm")

;; Since the `set-div-bits' test already checks against non-trivial bit
;; patterns, we'll assume that the involved bit operations work properly.

(define values '(#b000000000000001
                 #b100000000000000
                 #b011111111111111
                 #b010101010101010
                 #b001010101010101
                 #b011001100110011
                 #b000110011001100))

(define functions `((,set-bits-mdiv . 4)
                    (,set-bits-ndiv . 18)))

(define value-width 14)

(define (test-gen fcn shifts)
  (let nextval ((v values))
    (cond
     ((null? v) #t)
     (else
      (let* ((exp (car v))
             (result (fcn #xffffffff exp))
             ;; Read the bits from the result and add 1, because the bits
             ;; store the configured divider value minus 1.
             (got (1+ (bit-extract-width result shifts value-width))))
        (define-test (format #f
                             "~a, exp: ~s, got: ~s"
                             fcn
                             (number->string exp 16)
                             (number->string got 16))
          (pass-if-= exp got))
        (nextval (cdr v)))))))

(with-fs-test-bundle
 (plan (* (length functions)
          (length values)))
 (for-each (lambda (x)
             (test-gen (car x) (cdr x)))
           functions))
