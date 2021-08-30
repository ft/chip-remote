;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote units))

(primitive-load "tests/test-tap-cfg.scm")

(define-unit minute
  #:symbol 'min
  #:dimension time
  #:to (lambda (s) (/ s 60))
  #:from (lambda (m) (* m 60)))

(with-fs-test-bundle
 (plan 10)
 (for-each (lambda (u)
             (define-test (format #f "~a is a fundamental unit" (unit-name u))
               (pass-if-true (fundamental-unit? u))))
           (list metre gram second ampere kelvin mole candela))
 (let ((t (put-unit 1 minute)))
   (define-test "1 minute is 60 seconds"
     (pass-if-= 60 (vu-value (convert t second)))))

 (let* ((km (si-combine kilo metre))
        (l (put-unit 1 km)))
   (define-test "1 km is 1000 metres"
     (pass-if-= 1000 (vu-value (convert l metre)))))

 (let* ((mmin (si-combine milli minute))
        (t (put-unit 1000 mmin)))
   (define-test "1000 milli-minutes are 60 seconds"
     (pass-if-= 60 (vu-value (convert t second))))))
