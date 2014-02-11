;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test chip-remote)
             (chip-remote protocol))

(define connection (init-connection))

(test-with-tag 'version-failed (protocol-version connection))

(close-connection connection)
(quit 0)
