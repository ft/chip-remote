(use-modules (chip-remote io)
             (chip-remote protocol))

(define device (getenv "CR_BOARD_DEVICE"))
(define connection (make-cr-connection device))

(define (looks-good p)
  (and (list? p)
       (list? (car p))
       (pair? (caar p))
       (not (list? (caar p)))))

(or (io-open connection)
    (throw 'open-failed))
(or (hi connection)
    (throw 'hi-failed))

(let ((p (ports connection)))
  (or (looks-good p)
      (throw 'ports-failed `(ports ,p))))

(or (bye connection)
    (throw 'bye-failed))
(or (io-close connection)
    (throw 'close-failed))

(quit 0)
