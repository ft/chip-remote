(use-modules (test chip-remote)
             (chip-remote protocol))

(define connection (init-connection))

(test-with-tag 'version-failed (protocol-version connection))

(close-connection connection)
(quit 0)
