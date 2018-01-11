;; Copyright (c) 2011-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test chip-remote)
             (chip-remote protocol)
             (ice-9 pretty-print))

(define connection (init-connection))

(let ((f (features connection)))
  (test-with-tag 'features-failed f)
  (pretty-print f #:per-line-prefix "# "))

(close-connection connection)
(quit 0)
