;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices analog-devices adf4169 tables)
  #:use-module (chip-remote named-value)
  #:export (muxout-cfg
            prescaler-cfg
            charge-pump-cfg
            phase-detect-polarity-map
            lock-detect-precision-map
            ramp-mode-cfg
            negative-bleed-map
            word-select
            clk-divider-mode-map
            ramp-status-map
            sigma-delta-mode-map
            latch-enable-map
            deviation-select-map
            interrupt-ctrl-map
            tx-data-ramp-clock-map
            delay-clock-map))

(define-value muxout-cfg '((three-state         . #b0000)
                           (vdd-digital         . #b0001)
                           (gnd-digital         . #b0010)
                           (r-divider           . #b0011)
                           (reserved            . #b0100)
                           (reserved            . #b0101)
                           (digital-lock-detect . #b0110)
                           (serial-data-output  . #b0111)
                           (reserved            . #b1000)
                           (reserved            . #b1001)
                           (clk-divider-output  . #b1010)
                           (reserved            . #b1011)
                           (reserved            . #b1100)
                           (r-divider/2         . #b1101)
                           (n-divider/2         . #b1110)
                           (muxout-readback     . #b1111)))

(define-value prescaler-cfg '((4/5 . 0)
                              (8/9 . 1)))

;; In mA.
(define-value charge-pump-cfg '(( 31/100 . #b0000)
                                ( 63/100 . #b0001)
                                ( 94/100 . #b0010)
                                (125/100 . #b0011)
                                (157/100 . #b0100)
                                (188/100 . #b0101)
                                (219/100 . #b0110)
                                (250/100 . #b0111)
                                (281/100 . #b1000)
                                (313/100 . #b1001)
                                (344/100 . #b1010)
                                (375/100 . #b1011)
                                (406/100 . #b1100)
                                (438/100 . #b1101)
                                (469/100 . #b1110)
                                (500/100 . #b1111)))

(define-value phase-detect-polarity-map '((negative . 0)
                                          (positive . 1)))

;; In ns.
(define-value lock-detect-precision-map '((14 . 0)
                                          ( 6 . 1)))

(define-value ramp-mode-cfg '((sawtooth          . #b00)
                              (triangular        . #b01)
                              (single-sawtooth   . #b10)
                              (single-triangular . #b11)))

;; In micro-amps
(define-value negative-bleed-map '((  3730/1000 . #b000)
                                   ( 11030/1000 . #b001)
                                   ( 25250/1000 . #b010)
                                   ( 53100/1000 . #b011)
                                   (109700/1000 . #b100)
                                   (224700/1000 . #b101)
                                   (454700/1000 . #b110)
                                   (916400/1000 . #b111)))

(define-value word-select '((word-1 . 0)
                            (word-2 . 1)))

(define-value clk-divider-mode-map '((clock-divider-off  . #b00)
                                     (fast-clock-divider . #b01)
                                     (reserved           . #b10)
                                     (ramp-divider       . #b11)))

(define-value ramp-status-map '((normal-operation        . #b00000)
                                (readback-to-muxout      . #b00010)
                                (ramp-complete-to-muxout . #b00011)
                                (charge-pump-up          . #b10000)
                                (charge-pump-down        . #b10001)))

(define-value sigma-delta-mode-map '((normal-operation     . #b00000)
                                     (disabled-with-frac=0 . #b01110)))

(define-value latch-enable-map '((from-pin        . 0)
                                 (sync-with-refin . 1)))

(define-value interrupt-ctrl-map '((interrupt-off                . #b00)
                                   (interrupt-on-sweep-continues . #b01)
                                   (interrupt-on-sweep-stops     . #b11)))

(define-value tx-data-ramp-clock-map '((clock-divider . 0)
                                       (tx-data       . 1)))

(define-value delay-clock-map '((pfd-clock              . 0)
                                (pfd-clock-times-clock1 . 1)))
