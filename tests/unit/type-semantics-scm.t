;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (chip-remote interpreter)
             (chip-remote semantics))

(init-test-tap!)

(with-fs-test-bundle
    (plan 8)

  (let ((sem (generate-semantics interpreter
                                 #:decode '(lambda (x) (increment x 2))
                                 #:encode '(lambda (x) (decrement x 2)))))
    (define-test "interpreter codec type checks out"
      (pass-if-eq? (semantics-type sem) 'interpreter))
    (define-test "interpreter decoder works"
      (pass-if-= 8 ((evaluation-value (semantics-decode sem)) 6)))
    (define-test "interpreter encoder works"
      (pass-if-= 6 ((evaluation-value (semantics-encode sem)) 8))))

  (let* ((s (generate-semantics lookup '((something . 3)
                                         (more . 5)
                                         (stuff . 23)))))
    (define-test "table lookup semantics type checks out"
      (pass-if-eq? (semantics-type s) 'table-lookup))
    (define-test "table decoding works"
      (pass-if-eq? (s:decode s 4 3) 'something))
    (define-test "table decoding works (undefined)"
      (pass-if-eq? (s:decode s 4 42) 'chip-remote:undefined))
    (define-test "table encoding works"
      (pass-if-= (s:encode s 4 'more) 5))
    (define-test "table encoding works (undefined)"
      (pass-if-exception 'cr/undefined
                         (s:encode s 4 'does-not-exist)))))
