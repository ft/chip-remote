;; -*- scheme -*-

;; Copyright (c) 2025 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (srfi srfi-1)
             (test tap)
             (test setup)
             (chip-remote bit-operations))

(init-test-tap!)

(with-fs-test-bundle
  (plan 12)
  (let ((width-tests '((  0 0 0)
                       (  1 1 1)
                       (  2 2 1)
                       (  3 2 1)
                       (  4 3 1)
                       (  7 3 1)
                       (  8 4 1)
                       ( 10 4 1)
                       ( 15 4 1)
                       ( 16 5 2)
                       (127 7 2)
                       (128 8 2))))
    (for-each (lambda (t)
                (let ((v (first t))
                      (w (second t))
                      (h (third t)))
                  (define-test (format #f "(bit-width ~a) → ~a" v w)
                    (pass-if-= w (bit-width v)))
                  (define-test (format #f "(hex-width ~a) → ~a" v h)
                    (pass-if-= h (hex-width v)))))
              width-tests)))
