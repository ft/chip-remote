;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices analog-devices adf4158)
  #:use-module (chip-remote device)
  #:use-module (chip-remote utilities)
  #:use-module (chip-remote manufacturer analog-devices)
  #:use-module (chip-remote devices analog-devices adf4158 registers)
  ;; The ADF4158's configuration interface is quite similar to the ADF4169.
  ;; This description will recycle as much of it as it can.
  #:use-module ((chip-remote devices analog-devices adf4169 registers)
                #:prefix adf4169:)
  #:export (adf4158))

(define-device adf4158
  #:manufacturer analog-devices
  #:homepage "http://www.analog.com/en/products/adf4158.html"
  #:datasheet "http://www.analog.com/media/en/technical-documentation/data-sheets/ADF4158.pdf"
  #:keywords '(fractional-n pll direct modulation fsk psk fmcw)
  #:bus (spi #:frame-width 32)
  #:transfer (write-only full-table (lambda (value) (reverse (flatten value))))
  #:register-width 32
  #:register-map (#:table* (0 adf4169:reg:frac/int)
                           (1 reg:lsb-frac)
                           (2 adf4169:reg:r-divider)
                           (3 reg:function)
                           (4 reg:test)
                           (5 reg:deviation)
                           (6 adf4169:reg:step)
                           (7 reg:delay)))
