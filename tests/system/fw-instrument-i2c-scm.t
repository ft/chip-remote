;; -*- scheme -*-

;; Copyright (c) 2025 chip-remote workers, All rights reserved.
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

(define (transmit c tx)
  (cr:i2c-transceive! c 'i2c-0 tx))

(define (i2c-test tio spec io)
  (let* ((rxs (map (lambda (x) (cddr x))
                   (filter (lambda (x) (eq? (car x) 'i2c-read)) io)))
         (expect (apply append rxs)))
    (define-test (format #f "i2c transmission test: ~a → ~a" spec expect)
      (pass-if-equal? (transmit ($ tio) spec) expect)))
  (fw-expect! tio 'i2c-text #:expect io))

(when verbose?
  (tio-push-parm! tio 'trace?))

(define i2c-address 0)

(with-fw-test-bundle tio (chip-remote firmware instrumentation spi)
  (plan 11)
  (boot-fw! tio)

  (define-test "Running proto-engange works"
    (pass-if-no-exception (proto-engage! ($ tio))))

  (i2c-test tio '(#vu8(0))
            `((i2c-write ,i2c-address 0)))
  (i2c-test tio '(#vu8(1 2) 2)
            `((i2c-write ,i2c-address 1 2)
              (i2c-read  ,i2c-address 0 0)))
  (i2c-test tio '(#vu8(1 2) 2 #vu8(10 20 30 40 50) 32)
            `((i2c-write ,i2c-address 1 2)
              (i2c-read  ,i2c-address 0 0)
              (i2c-write ,i2c-address 10 20 30 40 50)
              (i2c-read  ,i2c-address . ,(make-list 32 0)))))
