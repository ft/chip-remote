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
  (plan 12)

  (let ((s (semantics (decode (make-evaluation
                               '(lambda (w x) (increment x 2))))
                      (encode (make-evaluation
                               '(lambda (w x) (decrement x 2))))
                      (range (lambda (s w) '(range 0 8)))
                      (default 0))))
    (define-test "interpreter decoder works"
      (pass-if-= 8 (semantics-decode s 'width 6)))
    (define-test "interpreter encoder works"
      (pass-if-= 6 (semantics-encode s 'width 8)))
    (define-test "semantics-in-range? 8 → #t"
      (pass-if-true (semantics-in-range? s 5 8)))
    (define-test "semantics-in-range? 9 → #f"
      (pass-if-false (semantics-in-range? s 5 9))))

  (let* ((table '((something . 3)
                  (more . 5)
                  (stuff . 23)))
         (s (tbl table #:default 'something)))
    (define-test "table lookup range is correct"
      (pass-if-equal? (semantics-range s 5)
                      `(table ,table)))
    (define-test "table decoding works"
      (pass-if-eq? (semantics-decode s 4 3) 'something))
    (define-test "table decoding works (undefined)"
      (pass-if-eq? (semantics-decode s 4 42) 'chip-remote:undefined))
    (define-test "table encoding works"
      (pass-if-= (semantics-encode s 4 'more) 5))
    (define-test "table encoding works (undefined)"
      (pass-if-exception 'cr/undefined
                         (semantics-encode s 4 'does-not-exist)))
    (define-test "semantics-in-range? more → #t"
      (pass-if-true (semantics-in-range? s 5 'more)))
    (define-test "semantics-in-range? does-not-exist → #f"
      (pass-if-false (semantics-in-range? s 5 'does-not-exist)))
    (define-test "semantics-default works"
      (pass-if-eq? 'something (semantics-default s 5)))))
