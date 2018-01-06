;; -*- scheme -*-

;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote validate))

(primitive-load "tests/test-tap-cfg.scm")
(define-validator foobar ∈ foo bar)

(with-fs-test-bundle
 (plan 14)
 (let ((trivial (predicates))
       (below (predicates (< 100)))
       (above (predicates (> 100)))
       (inclusive (predicates (>= 100) (<= 200))))
   (define-test "Trivial range test works"
     (pass-if-true (trivial 123)))
   (define-test "Below test works #1"
     (pass-if-false (below 100)))
   (define-test "Below test works #2"
     (pass-if-true (below 99)))
   (define-test "Above test works #1"
     (pass-if-true (above 101)))
   (define-test "Above test works #2"
     (pass-if-false (above 100)))
   (define-test "Inclusive range works #1"
     (pass-if-true (inclusive 100)))
   (define-test "Inclusive range works #2"
     (pass-if-true (inclusive 200)))
   (define-test "Inclusive range works #3"
     (pass-if-true (inclusive 150)))
   (define-test "Inclusive range works #4"
     (pass-if-false (inclusive 99)))
   (define-test "Inclusive range works #5"
     (pass-if-false (inclusive 201)))
   )

 (let* ((v (generate-validator ∉ foo bar))
        (p (validator-predicate v)))
   (define-test "not-element-of validator works #1"
     (pass-if-false (or (p 'foo)
                        (p 'bar))))
   (define-test "not-element-of validator works #2"
     (pass-if-true (and (p 'baz)
                        (p 'quux))))
   (define-test "not-element-of expression is correct"
     (pass-if-equal? (validator-expression v)
                     '(foo bar))))

 (define-test "Validators can be identified"
   (pass-if-eq? (validator-name foobar) 'foobar)))
