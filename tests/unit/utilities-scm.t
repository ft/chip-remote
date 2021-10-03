;; -*- scheme -*-

;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (srfi srfi-11)
             (test tap)
             (test setup)
             (chip-remote utilities))

(init-test-tap!)

(define-syntax-rule (test-addr< a b r)
  (define-test (format #f "addr< ~a ~a → ~a" a b r)
    (pass-if-eq? (addr< a b) r)))

(define (test-structure r a b)
  (define-test (format #f "~a structure: ~s ~s" (if r 'same 'different) a b)
    (pass-if-eq? r (structurally-equal? a b))))

(define* (test-diff r a b #:optional expected)
  (let-values (((diff? diffs) (diff a b)))
    (define-test (format #f "(diff ~a ~a) → [~a ~a]" a b diff? diffs)
      (pass-if-true (and (eq? r diff?)
                         (if expected
                             (equal? expected diffs)
                             #t))))))

(with-fs-test-bundle
    (plan 30)
  (test-addr< '() '() #f)
  (test-addr< '(0) '(0) #f)
  (test-addr< '(1) '(0) #f)
  (test-addr< '(0) '(1) #t)
  (test-addr< '(0 1) '(0 1) #f)
  (test-addr< '(0 1) '(0 2) #t)
  (test-addr< '(0 2) '(0 1) #f)
  (test-addr< '(2 2 0) '(0 2 0) #f)
  (test-addr< '(2 2 0) '(2 2 1) #t)
  (test-addr< '(#f 2 0) '(#f 0 0) #f)
  (test-addr< '(#f 0 0) '(#f 2 0) #t)
  (test-addr< '(#f #f 0) '(#f #f 2) #t)
  (test-addr< '(#f #f 2) '(#f #f 1) #f)
  (test-addr< '(#f) '(#f) #f)
  (test-addr< '(#f #f) '(#f #f) #f)
  (test-addr< '(#f #f #f) '(#f #f #f) #f)
  (test-structure #t 1 1)
  (test-structure #t 1 'a)
  (test-structure #f 1 '(a))
  (test-structure #t '(1) '(a))
  (test-structure #t '(1 2 3 4 5) '(a b c d e))
  (test-structure #f '(1 2 3 4) '(a b c d e))
  (test-structure #t '((1 2) 3 4 5) '((a b) c d e))
  (test-structure #f '(1 2 3 4 5) '(a b c d . e))
  (test-structure #t
                  '((1 2 3) 4 (5 (6 (7)) 8))
                  '(("abc" b c) #u8(1 2 3) (d (e ("foo")) bar)))
  (define-test "diff doesn't operate on structurally incompatible operands"
    (pass-if-exception 'diff:unsupported-arguments
                       (diff '((1 2 3) (11 22 33) (111 222 333))
                             '((1 2 3) (11 33) (111 222 333)))))
  (test-diff #f '(1 2 3) '(1 2 3))
  (test-diff #t '(1 2 3) '(1 1 3) (list 1 (make-diff 2 1) 3))
  (test-diff #f
             '((1 2 3) (11 22 33) (111 222 333))
             '((1 2 3) (11 22 33) (111 222 333)))
  (test-diff #t
             '((1 2 3) (11 22 33) (111 222 333))
             '((1 2 3) (11 21 33) (111 222 333))
             (list '(1 2 3)
                   (list 11 (make-diff 22 21) 33)
                   '(111 222 333))))
