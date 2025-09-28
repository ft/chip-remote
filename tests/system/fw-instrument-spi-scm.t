;; -*- scheme -*-

;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 pretty-print)
             (test tap)
             (test chip-remote)
             (chip-remote protocol)
             (chip-remote utilities))

;; Set to #t to get verbose tracing output.
(define verbose? #f)

(define tio (make-test-io))

(define first-set '(#x100 #x200 #x300))
(define second-set '(#x400 #x500))
(define third-set '(#x600 #x700 #x800))

(define (transmit c . tx)
  (let ((lst (cr:spi-transceive! c 'spi-0 tx)))
    (car lst)))

(define (spi-test tio tx rx)
  (define-test (format #f "spi transmission test: ~a → ~a" tx rx)
    (pass-if-= (transmit ($ tio) tx) rx))
  (fw-expect! tio 'spi-text `(spi-tx ,tx) `(spi-rx ,rx)))

(when verbose?
  (tio-push-parm! tio 'trace?))

(with-fw-test-bundle tio (chip-remote firmware instrumentation spi)
  (no-plan)
  (boot-fw! tio)

  (define-test "Running proto-engange works"
    (pass-if-no-exception (proto-engage! ($ tio))))

  (tap/comment "SPI RX list empty, generating from 0 onward")
  (spi-test tio 123 0)
  (spi-test tio 21 1)
  (spi-test tio 42 2)

  (tap/comment "Loading first-set into SPI RX list")
  (instrument! tio (cons 'load-spi first-set))

  ;;(spi-test tio #xabc (car first-set))

  ;; (tap/comment "Loading second-set into SPI RX list")
  ;; (instrument! tio (cons 'load-spi second-set))

  ;; (for-each (lambda (rx) (spi-test tio #xabc rx))
  ;;           (cdr (append first-set second-set)))

  ;; (tap/comment "SPI RX list should be depleted now, generation resumes at 2")
  ;; (spi-test tio 321 2)

  ;; (tap/comment "Loading third-set into SPI RX list")
  ;; (instrument! tio (cons 'load-spi third-set))

  ;; (for-each (lambda (rx) (spi-test tio #xabc rx))
  ;;           third-set)

  ;; (tap/comment "SPI RX list should be depleted again, generation resumes at 3")
  ;; (spi-test tio 321 3)
  ;; (spi-test tio 321 4)
  )
