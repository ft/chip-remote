;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices analog-devices ad9262)
  #:use-module (chip-remote device)
  #:use-module (chip-remote utilities)
  #:use-module (chip-remote manufacturer analog-devices)
  #:use-module (chip-remote devices analog-devices ad9262 registers)
  #:export (ad9262))

(define-device ad9262
  #:manufacturer analog-devices
  #:homepage "http://www.analog.com/en/products/ad9262.html"
  #:datasheet "http://www.analog.com/media/en/technical-documentation/data-sheets/AD9262.pdf"
  #:keywords '(16bit 160msps sigma-delta adc)
  #:register-width 8
  #:register-map (#:table* (#x000 reg:spi-port-cfg)
                           (#x001 reg:chip-id)
                           (#x002 reg:chip-grade)
                           (#x005 reg:channel-index)
                           (#x008 reg:power-modes)
                           (#x009 reg:pll-enable)
                           (#x00a reg:pll)
                           (#x00f reg:analog-input)
                           (#x014 reg:output-modes)
                           (#x015 reg:output-adjust)
                           (#x016 reg:output-clock)
                           (#x018 reg:reference)
                           (#x101 reg:output-data)
                           (#x111 reg:overrange)
                           (#x112 reg:quad-error-correction-1)
                           (#x113 reg:quad-error-correction-2)))
