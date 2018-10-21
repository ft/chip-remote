;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (chip-remote device spi)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 optargs)
  #:export (make-device-access-spi
            device-access-spi?
            spi-frame-width
            spi-bit-order
            spi-bit-rate
            spi-clk-phase-delay
            spi-clk-polarity
            spi-cs-polarity))

(define-record-type <device-access-spi>
  (make-device-access-spi* frame-width bit-order
                           bit-rate clk-phase-delay
                           clk-polarity cs-polarity)
  device-access-spi?
  (frame-width spi-frame-width)
  (bit-order spi-bit-order)
  (bit-rate spi-bit-rate)
  (clk-phase-delay spi-clk-phase-delay)
  (clk-polarity spi-clk-polarity)
  (cs-polarity spi-cs-polarity))

(define* (make-device-access-spi #:key
                                 (frame-width 8)
                                 (bit-order 'msb-first)
                                 (bit-rate 10000)
                                 (clk-phase-delay #t)
                                 (clk-polarity 'leading-edge)
                                 (cs-polarity 'active-low))
  (make-device-access-spi* frame-width bit-order
                           bit-rate clk-phase-delay
                           clk-polarity cs-polarity))
