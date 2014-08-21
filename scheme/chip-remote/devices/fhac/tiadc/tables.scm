;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (chip-remote devices fhac tiadc tables)
  #:export (extended-command-map
            readout-mask-table
            readout-mode-table))

(define extended-command-map
  '((reset . #x00)
    (snapshot . #x01)
    (usb-readout . #x02)
    (usb-abort . #x03)
    (read-register . #x04)))

(define readout-mask-table
  '((none . #b000)
    (A    . #b001)
    (B    . #b010)
    (AB   . #b011)
    (C    . #b100)
    (AC   . #b101)
    (BC   . #b110)
    (ABC  . #b111)))

(define readout-mode-table
  '((interleaved . 0)
    (block-wise  . 1)))
