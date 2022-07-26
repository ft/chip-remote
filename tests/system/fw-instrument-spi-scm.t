;; -*- scheme -*-

;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test chip-remote)
             (chip-remote io)
             (chip-remote protocol)
             (chip-remote utilities))

;; Set to #t to get verbose tracing output.
(define verbose? #t)

(define tio (make-test-io))

(define first-set '(#x100 #x200 #x300))
(define second-set '(#x400 #x500))
(define third-set '(#x600 #x700 #x800))

(define (spi-test tio tx rx)
  (define-test (format #f "spi transmission test: ~a → ~a" tx rx)
    (pass-if-= (transmit ($ tio) tx) rx))
  (fw-expect! tio `(spi-tx ,tx) `(spi-rx ,rx)))

(when verbose?
  (io-opt/set 'trace #t)
  (tio-push-parm! tio 'trace))

(with-test-bundle (chip-remote firmware instrumentation spi)
  (require (native-firmware-built?))
  (plan 39)
  (boot-fw! tio)

  (tap/comment "SPI RX list empty, generating from 0 onward")
  (spi-test tio 123 0)
  (spi-test tio 321 1)

  (tap/comment "Loading first-set into SPI RX list")
  (instrument! tio (cons 'load-spi first-set))

  (spi-test tio #xabc (car first-set))

  (tap/comment "Loading second-set into SPI RX list")
  (instrument! tio (cons 'load-spi second-set))

  (for-each (lambda (rx) (spi-test tio #xabc rx))
            (cdr (append first-set second-set)))

  (tap/comment "SPI RX list should be depleted now, generation resumes at 2")
  (spi-test tio 321 2)

  (tap/comment "Loading third-set into SPI RX list")
  (instrument! tio (cons 'load-spi third-set))

  (for-each (lambda (rx) (spi-test tio #xabc rx))
            third-set)

  (tap/comment "SPI RX list should be depleted again, generation resumes at 3")
  (spi-test tio 321 3)
  (spi-test tio 321 4)

  (kill-fw! tio))
