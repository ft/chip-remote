;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (chip-remote io)
             (chip-remote protocol))

(define connection (make-cr-connection serial-device))

(define port-index 0)

(define pin-chip-select 0)
(define pin-clock 1)
(define pin-master-out-slave-in 2)
(define pin-master-in-slave-out 3)

(io-open connection)
(hi connection)

(map (lambda (x) (set connection port-index (car x) (cdr x)))
     '((mode . spi)
       (frame-length . 32)
       (bit-order . lsb-first)
       (clk-phase-delay . 1)
       (clk-polarity . rising-edge)
       (cs-polarity . active-low)))

(map (lambda (x) (line connection port-index (cdr x) (car x)))
     `((cs . ,pin-chip-select)
       (clk . ,pin-clock)
       (mosi . ,pin-master-out-slave-in)
       (miso . ,pin-master-in-slave-out)))

(init connection port-index)
(focus connection port-index)

(bye connection)
(io-close connection)
