;; -*- scheme -*-

;; Copyright (c) 2011-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (srfi srfi-1))

(init-test-tap!)

(define zip2 (@@ (chip-remote protocol) zip2))
(define pcs (@@ (chip-remote protocol) protocol-char-set))

(define charset-tests
  '(("single word" "FOO123BAR"
     "FOO123BAR")
    ("single word with dash" "FOO-BAR"
     "FOO-BAR")
    ("three simple words" "FOO BAR BAZ"
     "FOO" "BAR" "BAZ")
    ("three simple words, two with dash" "FOO-BAR BAZ-QUUZ DING"
     "FOO-BAR" "BAZ-QUUZ" "DING")))

(with-fs-test-bundle
 (plan (+ 6 (length charset-tests)))

 (define-test "zip2: normal zip"
   (pass-if-equal? (zip2 '(a c e) '(b d f))
                   '((a . b) (c . d) (e . f))))

 (define-test "zip2: first short"
   (pass-if-equal? (zip2 '(a c) '(b d f))
                   '((a . b) (c . d))))

 (define-test "zip2: second short"
   (pass-if-equal? (zip2 '(a c e) '(b d))
                   '((a . b) (c . d))))

 (define-test "zip2: first empty"
   (pass-if-true (null? (zip2 '() '(b d f)))))

 (define-test "zip2: second empty"
   (pass-if-true (null? (zip2 '(a c e) '()))))

 (define-test "zip2: both empty"
   (pass-if-true (null? (zip2 '() '()))))

 (map (lambda (x)
        (define-test (format #f "charset: ~a" (car x))
          (pass-if-equal? (string-tokenize (cadr x) pcs)
                          (cddr x))))
      charset-tests))
