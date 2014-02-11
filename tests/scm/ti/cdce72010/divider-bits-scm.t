;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 format)
             (test tap)
             (chip-remote devices ti cdce72010 tables))
(primitive-load "tests/test-tap-cfg.scm")

(load "divider-samples.scm")

(with-fs-test-bundle
 (plan (length divider-samples))
 (let next ((c divider-samples))
   (cond ((null? c)
          (quit 0))
         (else
          (let ((got (get-bits-for-divider (caar c)))
                (exp (cadar c))
                (div (caar c)))
            (define-test (format #f
                                 "div(~d), exp: ~s, got: ~s."
                                 div
                                 (number->string exp 2)
                                 (number->string got 2))
              (pass-if-= exp got))
            (next (cdr c)))))))
