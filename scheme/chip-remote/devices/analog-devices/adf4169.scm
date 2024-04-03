;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices analog-devices adf4169)
  #:use-module (chip-remote device)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote utilities)
  #:use-module (chip-remote manufacturer analog-devices)
  #:use-module (chip-remote devices analog-devices adf4169 registers)
  #:export (adf4169))

(define-device adf4169
  (manufacturer analog-devices)
  (homepage "http://www.analog.com/en/products/adf4169.html")
  (datasheet "http://www.analog.com/media/en/technical-documentation/data-sheets/ADF4169.pdf")
  (keywords '(fractional-n pll direct modulation fsk psk fmcw))
  ;; #:bus (spi #:frame-width 32)
  ;; #:transmit (write-only full-table (lambda (lst)
  ;;                                     (sort lst (lambda (a b)
  ;;                                                 (> (cadr a) (cadr b))))))
  (register-width 32)
  (page-map (pm→ (table (↔ (#f (rm→ (table (↔ (0 reg:frac/int)
                                              (1 reg:lsb-frac)
                                              (2 reg:r-divider)
                                              (3 reg:function)
                                              (4 reg:clock)
                                              (5 reg:deviation)
                                              (6 reg:step)
                                              (7 reg:delay))))))))))
