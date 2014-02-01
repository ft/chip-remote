(use-modules (test chip-remote)
             (chip-remote protocol))

(define connection (init-connection))

(define (looks-good? p)
  (and (list? p)
       (list? (car p))
       (pair? (caar p))
       (not (list? (caar p)))))

(test-with-tag 'broken-ports-reply (looks-good? (ports connection)))

(close-connection connection)
(quit 0)
