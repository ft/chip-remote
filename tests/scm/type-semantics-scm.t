;; -*- scheme -*-

;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote semantics))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (plan 5)

 (define-test "default semantics for width 1 work (boolean)"
   (pass-if-eq? (semantics-type (deduce-semantics 1 '() #f))
                'boolean))
 (define-test "default semantics for width 2.. work (unsigned-integer)"
   (pass-if-eq? (semantics-type (deduce-semantics 2 '() #f))
                'unsigned-integer))
 (let* ((table '((a . 1) (b . 2)))
        (sem (deduce-semantics 2 '() (list 'lookup table))))
   (define-test "table lookup semantics work"
     (pass-if-eq? (semantics-type sem)
                  'table-lookup))
   (define-test "table lookup decoder works"
     (pass-if-eq? 'b ((semantics-decode sem) 2)))
   (define-test "table lookup encoder works"
     (pass-if-= 1 ((semantics-encode sem) 'a)))))
