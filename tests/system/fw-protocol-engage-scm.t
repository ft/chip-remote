;; -*- scheme -*-

;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test chip-remote)
             (chip-remote protocol))

;; Set to #t to get verbose tracing output.
(define verbose? #f)
(define tio (make-test-io))
(when verbose?
  (tio-push-parm! tio 'trace?))

(with-fw-test-bundle tio (chip-remote firmware engage)
  (plan 6)
  (boot-fw! tio)
  (define-test "Running proto-engange works"
    (pass-if-no-exception (proto-engage! ($ tio))))
  (let ((ifcs (proto-interfaces ($ tio))))
    (define-test "Native firmware offers four interfaces"
      (pass-if-= 4 (length ifcs)))
    (define-test "Native firmware offers two spi busses"
      (pass-if-true (and (memq 'spi-0 ifcs)
                         (memq 'spi-1 ifcs))))
    (define-test "Native firmware offers two i2c busses"
      (pass-if-true (and (memq 'i2c-0 ifcs)
                         (memq 'i2c-1 ifcs)))))
  (define-test "proto-get-ifc-ctrl! spi-0"
    (pass-if-true (list? (proto-get-ifc-ctrl! ($ tio) 'spi-0))))
  (define-test "proto-get-ifc-ctrl! i2c-0"
    (pass-if-true (list? (proto-get-ifc-ctrl! ($ tio) 'i2c-0)))))
