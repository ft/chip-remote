;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test setup)
  #:use-module (test tap)
  #:export (init-test-tap!))

(define (init-test-tap!)
  (let ((root (format #f "~a/tests" (getcwd))))
    ;;(format #t "# fs-root: [~a]~%" root)
    (tap/set-option 'fs-root root))
  (tap/set-option 'fs-suffix "-scm.t")
  (tap/set-option 'fs-prefix ""))
