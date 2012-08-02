;; The stuff we want to test is implemented in the following module.
(use-modules ((chip-remote protocol)
              #:renamer (symbol-prefix-proc 'chip-remote/)))

;; The `$CR_BOARD_DEVICE' environment variable is set to the device, the
;; board-simulation program set up for us to connect to.
(define device (getenv "CR_BOARD_DEVICE"))

;; Open it, say hello, query the protocol version, say bye, close up and exit.
(or (cr/open device)
    (throw 'open-failed))
(or (chip-remote/hi)
    (throw 'hi-failed))

(let ((v (chip-remote/version)))
  (or v
      (throw 'version-failed `(version ,v))))

(or (chip-remote/bye)
    (throw 'bye-failed))
(or (cr/close)
    (throw 'close-failed))

(quit 0)
