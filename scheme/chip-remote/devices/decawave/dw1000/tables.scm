;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices decawave dw1000 tables)
  #:use-module (chip-remote named-value)
  #:export (agc-prf-tune-map
            gpio-0-modes
            gpio-1-modes
            gpio-2-modes
            gpio-3-modes
            gpio-4-modes
            gpio-5-modes
            gpio-6-modes
            gpio-7-modes
            gpio-8-modes
            gpio-direction-map
            gpio-irq-both-map
            gpio-irq-level/edge-map
            gpio-irq-mode-map
            irq-polarity-map
            phy-header-mode-map
            spi-data-edge-map
            bit-rate-map
            prf-map
            preamble-symbol-rep-map
            system-clock-map))

(define-value irq-polarity-map '((active-low  . 0)
                                 (active-high . 1)))

(define-value spi-data-edge-map '((miso-at-sampling-edge . 0)
                                  (miso-at-inverted-edge . 1)))

(define-value phy-header-mode-map '((standard-frame . #b00)
                                    (reserved       . #b01)
                                    (reserved       . #b10)
                                    (long-frame     . #b11)))

(define-value bit-rate-map '((110kBit/s  . #b00)
                             (850kBit/s  . #b01)
                             (6.81MBit/s . #b10)
                             (reserved   . #b11)))

(define-value prf-map '((4MHz     . #b00)
                        (16MHz    . #b01)
                        (64MHz    . #b10)
                        (reserved . #b11)))

(define-value preamble-symbol-rep-map '((16   . #b0000)
                                        (64   . #b0001)
                                        (128  . #b0101)
                                        (256  . #b1001)
                                        (512  . #b1101)
                                        (1024 . #b0010)
                                        (1536 . #b0110)
                                        (2048 . #b1010)
                                        (4096 . #b0011)))

(define-value gpio-0-modes '((gpio         . #b00)
                             (rx-ok-led    . #b01)
                             (system-clock . #b10)
                             (reserved     . #b11)))

(define-value gpio-1-modes '((gpio     . #b00)
                             (sfd-led  . #b01)
                             (reserved . #b10)
                             (reserved . #b11)))

(define-value gpio-2-modes '((gpio     . #b00)
                             (rx-led   . #b01)
                             (reserved . #b10)
                             (reserved . #b11)))

(define-value gpio-3-modes '((gpio     . #b00)
                             (tx-led   . #b01)
                             (reserved . #b10)
                             (reserved . #b11)))

(define-value gpio-4-modes '((gpio        . #b00)
                             (external-pa . #b01)
                             (reserved    . #b10)
                             (reserved    . #b11)))

(define-value gpio-5-modes '((gpio        . #b00)
                             (external-tx . #b01)
                             (reserved    . #b10)
                             (reserved    . #b11)))

(define-value gpio-6-modes '((gpio        . #b00)
                             (external-rx . #b01)
                             (reserved    . #b10)
                             (reserved    . #b11)))

(define-value gpio-7-modes '((sync-input . #b00)
                             (gpio       . #b01)
                             (reserved   . #b10)
                             (reserved   . #b11)))

(define-value gpio-8-modes '((irq-output . #b00)
                             (gpio       . #b01)
                             (reserved   . #b10)
                             (reserved   . #b11)))

(define-value gpio-direction-map '((output . 0)
                                   (input  . 1)))

(define-value gpio-irq-mode-map '((active-high/rising-edge . 0)
                                  (active-low/falling-edge . 1)))

(define-value gpio-irq-level/edge-map '((level . 0)
                                        (edge  . 1)))

(define-value gpio-irq-both-map '((normal     . 0)
                                  (both-edges . 1)))

(define-value system-clock-map '((auto            . #b00)
                                 (force-xti-clock . #b01)
                                 (force-pll-clock . #b10)
                                 (reserved        . #b11)))

(define-value agc-prf-tune-map '((prf-16mhz . #x8870)
                                 (prf-64mhz . #x889b)))
