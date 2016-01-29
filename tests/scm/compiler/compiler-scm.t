;; Copyright (c) 2016 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (ice-9 pretty-print)
             (chip-remote compiler))

(primitive-load "tests/test-tap-cfg.scm")
(define kw #'foo)
(define state
  (analyse-register-map kw (read-file kw "tests/scm/compiler/fic007.table")))

(with-fs-test-bundle
 (plan 2)

 ;;(pretty-print state)
 (define-test "Compiler found meta data"
   (pass-if-true (< 0 (length (assq-ref state 'meta)))))
 (define-test "Compiler found register definitions"
   (pass-if-true (< 0 (length (assq-ref state 'registers))))))
