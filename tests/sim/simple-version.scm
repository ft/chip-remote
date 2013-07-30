;; The stuff we want to test is implemented in the following module.
(use-modules (chip-remote io)
             (chip-remote protocol))

;; The `$CR_BOARD_DEVICE' environment variable is set to the device, the
;; board-simulation program set up for us to connect to.
(define device (getenv "CR_BOARD_DEVICE"))

;; Prepare a <cr-connection> object, which is handed to the libraries API
;; functions.
(define connection (make-cr-connection device))

;; Open it, say hello, query the protocol version, say bye, close up and exit.
(or (io-open connection)
    (throw 'open-failed))
(or (hi connection)
    (throw 'hi-failed))

(let ((v (protocol-version connection)))
  (or v
      (throw 'version-failed `(version ,v))))

(or (bye connection)
    (throw 'bye-failed))
(or (io-close connection)
    (throw 'close-failed))

(quit 0)
