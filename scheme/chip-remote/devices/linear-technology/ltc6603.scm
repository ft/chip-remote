;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; The LTC6603 is a programmable switched-capacitor low-pass filter with
;; builtin amplifier banks. The device's configuration is particularly simple.
;; It uses one eight bit register for configuration. There is no addressing of
;; registers, let alone memory pages, at all.

(define-module (chip-remote devices linear-technology ltc6603)
  #:use-module (chip-remote device)
  #:use-module (chip-remote manufacturer linear-technology)
  #:use-module (chip-remote register)
  #:export (ltc6603))

;; These two-bit config fields are placed into the final configuration word
;; most-significant bit toward the lower bit offsets. That's why the #b10 and
;; #b01 mappings look different from a first look into the datasheet.

(define low-pass-cfg '((div-by-512 . #b00)
                       (div-by-128 . #b10)
                       (div-by-32  . #b01)
                       (div-by-32  . #b11)))

(define gain-cfg '(( 0dB . #b00)
                   ( 6dB . #b10)
                   (12dB . #b01)
                   (24dB . #b11)))

(define-device ltc6603
  #:manufacturer linear-technology
  #:homepage "http://www.linear.com/product/LTC6603"
  #:datasheet "http://cds.linear.com/docs/en/datasheet/6603fa.pdf"
  #:keywords '(switched capacitor lowpass filter programmable gain)
  #:register-width 8
  #:register (#:contents (output 0 1)
                         (shutdown 1 1)
                         (reserved 2 2)
                         (low-pass-cfg 4 2 #:semantics 'lookup low-pass-cfg)
                         (gain-cfg 6 2 #:semantics 'lookup gain-cfg)))
