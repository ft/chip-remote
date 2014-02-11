;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 format)
             (test tap)
             (bitops)
             (chip-remote devices ti cdce72010 prg))
(primitive-load "tests/test-tap-cfg.scm")

(load "divider-samples.scm")

;; The width of the divider bit field (needed for extraction).
(define bit-width 7)

;; We'll test against different register values, to make sure the operation
;; didn't just succeed because the old register value was "just-ones" or
;; "just-zeros".
(define target-register-values
  '(#x00000000
    #xffffffff
    #x33333333
    #xcccccccc
    #xaaaaaaaa))

;; The CDCE72010 uses the complex divider settings for output- and feedback-
;; dividers. They use the same bits but in different places.
(define functions `((,set-bits-fbdiv . 9)
                    (,set-bits-odiv . 17)))

(define (test-gen fnc sh val div exp)
  (let* ((got (fnc val div))
         (bits (bit-extract-width got sh bit-width)))
    (define-test (format #f
                         "div(~d@~d), regval: ~s, exp: ~s, got: ~s"
                         div
                         sh
                         (number->string got 16)
                         (number->string exp 2)
                         (number->string bits 2))
      (pass-if-= bits exp))))

(define (loop-over-values fnc shifts)
  ;; loop over register value list
  (let nextregval ((values target-register-values))
    (cond
     ((null? values) #t)
     (else
      (let ((value (car values)))
        ;; loop over sample divider values
        (let nextdiv ((divlist divider-samples))
          (cond
           ((null? divlist) #t)
           (else
            (let ((div (caar divlist))
                  (expected (cadar divlist)))
              (if (not (test-gen fnc shifts value div expected))
                  (quit 1))
              (nextdiv (cdr divlist)))))))
      (nextregval (cdr values))))))

(with-fs-test-bundle
 (plan (* (length functions)
          (length divider-samples)
          (length target-register-values)))
 (for-each (lambda (x)
             (loop-over-values (car x)
                               (cdr x)))
           functions))
