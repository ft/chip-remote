(use-modules (chip-remote protocol)
             (ice-9 pretty-print))

(define device (getenv "CR_BOARD_DEVICE"))

(or (cr/open device)
    (throw 'open-failed))
(or (hi)
    (throw 'hi-failed))

(let ((f (features)))
  (or f
      (throw 'features-failed `(features ,f)))
  (pretty-print f))

(or (bye)
    (throw 'bye-failed))
(or (cr/close)
    (throw 'close-failed))

(quit 0)
