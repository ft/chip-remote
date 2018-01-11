;; Copyright (c) 2011-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test chip-remote)
             (chip-remote protocol)
             (srfi srfi-1))

(define connection (init-connection))

(define (looks-good? p)
  (and (list? p)
       (pair? (car p))
       (fold (lambda (x acc)
               (and (memq (car x) '(ports focus))
                    acc))
             #t
             p)))

(test-with-tag 'broken-ports-reply (looks-good? (ports connection)))

(close-connection connection)
(quit 0)
