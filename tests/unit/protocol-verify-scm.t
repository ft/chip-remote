;; -*- scheme -*-

;; Copyright (c) 2011-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (srfi srfi-1))

(init-test-tap!)

(define v (@@ (chip-remote protocol) verify-and-convert))

(define a '(("VERBOSE" . "VERBOSE")
            ("ca" . int)
            ("cat" . int)))

(define b '((#t . "VERBOSE")
            (#t . 202)
            (#f . "cat")))

(with-fs-test-bundle
 (plan (length a))
 (map (lambda (x y) (define-test (format #f "verify ~a != ~a" x y)
                      (pass-if-equal? x y)))
      (fold (lambda (x y)
              (append y (list (v x)))) '() a)
      b))
