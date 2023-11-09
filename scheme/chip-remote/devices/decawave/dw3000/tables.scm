;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices decawave dw3000 tables)
  #:use-module (chip-remote named-value)
  #:export (gpio-0-modes
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
            system-clock-map
            sfd-type-map
            spi-collision-map
            db-diagnostics-map
            aes-mode-map
            aes-key-size-map
            aes-key-source-map
            aes-tag-size-map
            aes-otp-key-source-map))

(define-value irq-polarity-map '((active-low  . 0)
                                 (active-high . 1)))

(define-value spi-data-edge-map '((miso-at-sampling-edge . 0)
                                  (miso-at-inverted-edge . 1)))

(define-value phy-header-mode-map '((standard-frame . #b00)
                                    (reserved       . #b01)
                                    (reserved       . #b10)
                                    (long-frame     . #b11)))

(define-value bit-rate-map '((850kBit/s  . #b0)
                             (6.81MBit/s . #b1)))

(define-value prf-map '((16MHz    . #b01)
                        (64MHz    . #b10)))

(define-value preamble-symbol-rep-map '((64   . #b0001)
                                        (1024 . #b0010)
                                        (4096 . #b0011)
                                        (32   . #b0100)
                                        (128  . #b0101)
                                        (1536 . #b0110)
                                        (256  . #b1001)
                                        (2048 . #b1010)
                                        (512  . #b1101)))

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

(define-value sfd-type-map '((ieee802.15.3-short-8-symbol . #b00)
                             (decawave-8-symbol           . #b01)
                             (decawave-16-symbol          . #b10)
                             (ieee802.15.4z-8-symbol      . #b11)))

(define-value spi-collision-map '((aon     . #x10)
                                  (cia-rom . #x08)
                                  (cia-ram . #x04)
                                  (digi-rx . #x02)
                                  (digi-tx . #x01)))

(define-value db-diagnostics-map '((minimal  . #x1)
                                   (medium   . #x2)
                                   (reserved . #x3)
                                   (full     . #x4)))

(define-value aes-mode-map '((encrypt . #b0)
                             (decrypt . #b1)))

(define-value aes-key-size-map '((128-bits . #x0)
                                 (192-bits . #x1)
                                 (256-bits . #x2)
                                 (reserved . #x3)))

(define-value aes-key-source-map '((aes-key-reg . #b0)
                                   (ram/otp         . #b1)))

(define-value aes-tag-size-map '((0-bytes  . #x0)
                                 (4-bytes  . #x1)
                                 (6-bytes  . #x2)
                                 (8-bytes  . #x3)
                                 (10-bytes . #x4)
                                 (12-bytes . #x5)
                                 (14-bytes . #x6)
                                 (16-bytes . #x7)))

(define-value aes-otp-key-source-map '((aes-ram . #b0)
                                       (otp     . #b1)))


