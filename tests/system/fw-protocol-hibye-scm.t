;; -*- scheme -*-

;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test chip-remote)
             (chip-remote io)
             (chip-remote protocol))

;; Set to #t to get verbose tracing output.
(define verbose? #t)
(define tio (make-test-io))
(when verbose?
  (io-opt/set 'trace #t)
  (tio-push-parm! tio 'trace))

(with-test-bundle (chip-remote firmware hi/bye)
  (require (native-firmware-built?))
  (plan 2)
  (boot-fw! tio)
  (define-test "Firmware emits proper reply to ‘hi’ request"
    (pass-if-no-exception (hi ($ tio))))

  (define-test "Firmware emits proper reply to ‘bye’ request"
    (pass-if-no-exception (bye ($ tio))))

  (kill-fw! tio))
