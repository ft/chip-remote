(use-modules (chip-remote protocol))

(define device (getenv "CR_BOARD_DEVICE"))

(define (looks-good p)
  (and (list? p)
       (list? (car p))
       (pair? (caar p))
       (not (list? (caar p)))))

(or (cr/open device)
    (throw 'open-failed))
(or (hi)
    (throw 'hi-failed))

(let ((p (ports)))
  (or (looks-good p)
      (throw 'ports-failed `(ports ,p))))

(or (bye)
    (throw 'bye-failed))
(or (cr/close)
    (throw 'close-failed))

(quit 0)
