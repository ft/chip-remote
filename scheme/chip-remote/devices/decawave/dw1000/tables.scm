;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices decawave dw1000 tables)
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
            tx-bit-rate-map
            prf-map
            tx-psr-map))

(define-value irq-polarity-map '((0 . active-low)
                                 (1 . active-high)))

(define-value spi-data-edge-map '((0 . miso-at-sampling-edge)
                                  (1 . miso-at-inverted-edge)))

(define-value phy-header-mode-map '((#b00 . standard-frames)
                                    (#b01 . reserved)
                                    (#b10 . reserved)
                                    (#b11 . long-frames)))

(define-value tx-bit-rate-map '((#b00 . 110kBit/s)
                                (#b01 . 850kBit/s)
                                (#b10 . 6.81MBit/s)
                                (#b11 . reserved)))

(define-value prf-map '((#b00 . 4MHz)
                        (#b01 . 16MHz)
                        (#b10 . 64MHz)
                        (#b11 . reserved)))

(define-value tx-psr-map '((#b0000 . 16)
                           (#b0001 . 64)
                           (#b0101 . 128)
                           (#b1001 . 256)
                           (#b1101 . 512)
                           (#b0010 . 1024)
                           (#b0110 . 1536)
                           (#b1010 . 2048)
                           (#b0011 . 4096)))

(define-value gpio-0-modes '((#b00 . gpio)
                             (#b01 . rx-ok-led)
                             (#b10 . system-clock)
                             (#b11 . reserved)))

(define-value gpio-1-modes '((#b00 . gpio)
                             (#b01 . sfd-led)
                             (#b10 . reserved)
                             (#b11 . reserved)))

(define-value gpio-2-modes '((#b00 . gpio)
                             (#b01 . rx-led)
                             (#b10 . reserved)
                             (#b11 . reserved)))

(define-value gpio-3-modes '((#b00 . gpio)
                             (#b01 . tx-led)
                             (#b10 . reserved)
                             (#b11 . reserved)))

(define-value gpio-4-modes '((#b00 . gpio)
                             (#b01 . external-pa)
                             (#b10 . reserved)
                             (#b11 . reserved)))

(define-value gpio-5-modes '((#b00 . gpio)
                             (#b01 . external-tx)
                             (#b10 . reserved)
                             (#b11 . reserved)))

(define-value gpio-6-modes '((#b00 . gpio)
                             (#b01 . external-rx)
                             (#b10 . reserved)
                             (#b11 . reserved)))

(define-value gpio-7-modes '((#b00 . sync-input)
                             (#b01 . gpio)
                             (#b10 . reserved)
                             (#b11 . reserved)))

(define-value gpio-8-modes '((#b00 . irq-output)
                             (#b01 . gpio)
                             (#b10 . reserved)
                             (#b11 . reserved)))

(define-value gpio-direction-map '((0 . output)
                                   (1 . input)))

(define-value gpio-irq-mode-map '((0 . active-high/rising-edge)
                                  (1 . active-low/falling-edge)))

(define-value gpio-irq-level/edge-map '((0 . level)
                                        (1 . edge)))

(define-value gpio-irq-both-map '((0 . normal)
                                  (1 . both-edges)))
