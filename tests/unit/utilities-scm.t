;; -*- scheme -*-

;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (chip-remote utilities))

(init-test-tap!)

(define-syntax-rule (test-addr< a b r)
  (define-test (format #f "addr< ~a ~a → ~a" a b r)
    (pass-if-eq? (addr< a b) r)))

(with-fs-test-bundle
 (no-plan)
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
 (test-addr< '(#f #f #f) '(#f #f #f) #f))
