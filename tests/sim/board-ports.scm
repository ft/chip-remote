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
