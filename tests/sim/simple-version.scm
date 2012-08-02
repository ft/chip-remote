;; The stuff we want to test is implemented in the following module.
(use-modules (chip-remote protocol))

;; The `$CR_BOARD_DEVICE' environment variable is set to the device, the
;; board-simulation program set up for us to connect to.
(define device (getenv "CR_BOARD_DEVICE"))

;; Open it, say hello, query the protocol version, say bye, close up and exit.
(cr/open device)
(hi)

(let ((v (version)))
  (or v
      (throw 'version-failed `(version ,v))))

(bye)
(cr/close)
(quit 0)
