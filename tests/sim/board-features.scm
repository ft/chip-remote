(use-modules (chip-remote io)
             (chip-remote protocol)
             (ice-9 pretty-print))

(define device (getenv "CR_BOARD_DEVICE"))
(define connection (make-cr-connection device))

(or (io-open connection)
    (throw 'open-failed))
(or (hi connection)
    (throw 'hi-failed))

(let ((f (features connection)))
  (or f
      (throw 'features-failed `(features ,f)))
  (pretty-print f))

(or (bye connection)
    (throw 'bye-failed))
(or (io-close connection)
    (throw 'close-failed))

(quit 0)
