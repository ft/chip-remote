;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote bitops)
             (chip-remote))

(primitive-load "tests/test-tap-cfg.scm")

(define (foo x) (set-bits x #b0110 4 0))
(define (bar x) (set-bits x #b1001 4 4))

(define (baz x y) (set-bits x y 4 8))
(define (quz x y) (set-bits x y 4 12))

(define thing 23)

(define *tests*
  `(((label . "no functions 1 (literal)")
     (expansion . ,(chain-fncs 23))
     (expected . 23))
    ((label . "no functions 2 (value)")
     (expansion . ,(chain-fncs thing))
     (expected . 23))
    ((label . "arithmetic 1 (one fnc works)")
     (expansion . ,(chain-fncs 43 1-))
     (expected . 42))
    ((label . "arithmetic 2 (two fncs work)")
     (expansion . ,(chain-fncs 40 1- (+ 3)))
     (expected . 42))
    ((label . "arithmetic 3 (more fncs work; order works)")
     (expansion . ,(chain-fncs 23 1- (* 2) (- 2)))
     (expected . 42))
    ((label . "bits 1")
     (expansion . ,(chain-fncs #xaffe
                               foo
                               (baz #b0101)
                               (quz #b1010)
                               bar))
     (expected . #b1010010110010110))
    ((label . "bits 2")
     (expansion . ,(chain-fncs #xcafe
                               (baz #b0101)
                               bar
                               foo
                               (quz #b1010)))
     (expected . #b1010010110010110))))

(with-fs-test-bundle
 (plan (length *tests*))
 (map (lambda (x)
        (define-test (format #f "chain-fncs: ~a" (assq-ref x 'label))
          (pass-if-= (assq-ref x 'expansion)
                     (assq-ref x 'expected))))
      *tests*))
