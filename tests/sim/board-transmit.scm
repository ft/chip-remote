;; Copyright (c) 2011-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test chip-remote)
             (chip-remote protocol))

(define connection (init-connection))

(test-with-tag 'transmit-failed (integer? (transmit connection 123456)))

(close-connection connection)
(quit 0)
