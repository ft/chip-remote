;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices microchip mcp4351)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote device)
  #:use-module (chip-remote manufacturer microchip)
  #:use-module (chip-remote devices microchip mcp4351 registers)
  #:use-module (chip-remote item)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:export (mcp4351))

(define (register-access cmd ra value)
  ;; AAAA CC0V VVVV VVVV
  (logior (ash ra 12)
          (ash cmd 10)
          (logand value (one-bits 9))))

(define (register-write pa ra value)
  (register-access #b00 ra value))

(define (register-read pa ra)
  (register-access #b11 ra 0))

(define-device mcp4351
  (manufacturer microchip)
  (homepage "https://www.microchip.com/wwwproducts/en/MCP4351")
  (datasheet "http://ww1.microchip.com/downloads/en/DeviceDoc/22242A.pdf")
  (keywords '(digital potentiometer spi))
  (register-width 9)
  ;; #:bus (spi #:frame-width 16
  ;;            #:bit-order 'msb-first)
  ;; #:write register-write
  ;; #:read register-read
  (page-map
   (pm→
    (table
     (↔ (#f (rm→
             (table
              (↔ (#x0 reg:wiper-0)
                 (#x1 reg:wiper-1)
                 (#x4 reg:terminal-ctrl-0)
                 (#x6 reg:wiper-2)
                 (#x7 reg:wiper-3)
                 (#xa reg:terminal-ctrl-1))))))))))
