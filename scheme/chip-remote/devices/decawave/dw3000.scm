;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices decawave dw3000)
  #:use-module (chip-remote device)
  #:use-module (chip-remote register)
  #:use-module (chip-remote manufacturer decawave)
  #:use-module (chip-remote devices decawave dw3000 registers)
  #:export (dw3000))

(define-device dw3000
  #:manufacturer decawave
  #:homepage "https://www.qorvo.com/products/p/DW3220"
  #:datasheet "https://www.qorvo.com/products/d/da008154"
  #:keywords '(uwb tranceiver ieee802.15.4z positioning location)
  #:register-map (#:table* (#x00 reg:general-cfg)
                           (#x01 reg:general-cfg-and-aes)
                           (#x02 reg:sts-cfg)
                           (#x03 reg:rx-tune)
                           (#x04 reg:ext-sync)
                           (#x05 reg:gpio_ctrl)
                           (#x06 reg:drx)
                           (#x07 reg:rf-conf)
                           (#x08 reg:tx-cal)
                           (#x09 reg:fs-ctrl)
                           (#x0a reg:aon)
                           (#x0b reg:otp-if)
                           (#x0c reg:cia-0)
                           (#x0d reg:cia-1)
                           (#x0e reg:cia-2)
                           (#x0f reg:digital-diag)
                           (#x11 reg:pmsc)
                           (#x12 reg:rx-buffer-0)
                           (#x13 reg:rx-buffer-1)
                           (#x14 reg:tx-buffer)
                           (#x15 reg:acc-mem)
                           (#x16 reg:scratch-ram)
                           (#x17 reg:aes-ram)
                           (#x18 reg:set-1/set-2)
                           (#x1d reg:indirect-ptr-a)
                           (#x1e reg:indirect-ptr-b)
                           (#x1f reg:in-ptr-cfg)))
