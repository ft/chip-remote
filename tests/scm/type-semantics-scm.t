;; -*- scheme -*-

;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote interpreter)
             (chip-remote semantics))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (plan 8)

 (define-test "default semantics for width 1 work (boolean)"
   (pass-if-eq? (semantics-type (deduce-semantics 1 '() #f))
                'boolean))
 (define-test "default semantics for width 2.. work (unsigned-integer)"
   (pass-if-eq? (semantics-type (deduce-semantics 2 '() #f))
                'unsigned-integer))
 (let* ((table '((a . 1) (b . 2)))
        (sem (generate-semantics lookup table)))
   (define-test "table lookup semantics work"
     (pass-if-eq? (semantics-type sem)
                  'table-lookup))
   (define-test "table lookup decoder works"
     (pass-if-eq? 'b ((semantics-decode sem) 2)))
   (define-test "table lookup encoder works"
     (pass-if-= 1 ((semantics-encode sem) 'a))))

 (let ((sem (generate-semantics interpreter
                                #:decode '(lambda (x) (increment x 2))
                                #:encode '(lambda (x) (decrement x 2)))))
   (define-test "interpreter codecs work"
     (pass-if-eq? (semantics-type sem)
                  'interpreter))
   (define-test "interpreter decoder works"
     (pass-if-= 8 ((evaluation-value (semantics-decode sem)) 6)))
   (define-test "interpreter encoder works"
     (pass-if-= 6 ((evaluation-value (semantics-encode sem)) 8)))))
