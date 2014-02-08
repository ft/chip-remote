
(use-modules (test chip-remote)
             (chip-remote protocol))

(define connection (init-connection))

(test-with-tag 'transmit-failed (integer? (transmit connection 123456)))

(close-connection connection)
(quit 0)
