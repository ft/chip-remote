;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 format)
             (test tap)
             (chip-remote bitops)
             (chip-remote legacy ti cdce72010 tables)
             (chip-remote legacy ti cdce72010 prg))

(define value-width 7)

(with-fs-test-bundle
 (plan (length output-modes))
 (let nextval ((m output-modes))
   (cond
    ((null? m) #t)
    (else
     (let* ((mode (caar m))
            (exp (if (list? (cadar m))
                     (caadar m)
                     (cadar m)))
            (result (set-bits-output-mode #xffffffff mode))
            (got (bit-extract-width result 25 value-width)))
       (define-test (format #f
                            "mode(~a): expect: ~s"
                            (symbol->string mode)
                            (number->string exp 2))
         (pass-if-= exp got))
       (nextval (cdr m)))))))
