(use-modules (test chip-remote)
             (chip-remote protocol)
             (ice-9 pretty-print))

(define connection (init-connection))

(let ((f (features connection)))
  (test-with-tag 'features-failed f)
  (pretty-print f #:per-line-prefix "# "))

(close-connection connection)
(quit 0)
