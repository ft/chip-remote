;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices microchip mcp43xx)
  #:use-module (chip-remote device)
  #:use-module (chip-remote utilities)
  #:use-module (chip-remote manufacturer microchip)
  #:use-module (chip-remote devices microchip mcp43xx registers)
  #:export (mcp43xx))

(define-device mcp43xx
  #:manufacturer microchip
  #:homepage "https://www.microchip.com/wwwproducts/en/MCP4351"
  #:datasheet "http://ww1.microchip.com/downloads/en/DeviceDoc/22242A.pdf"
  #:keywords '(digital potentiometer spi)
  #:bus (spi)
  #:register-map (#:table* (#x0 reg:wiper-0)
                           (#x1 reg:wiper-1)
                           (#x4 reg:terminal-ctrl-0)
                           (#x6 reg:wiper-2)
                           (#x7 reg:wiper-3)
                           (#xa reg:terminal-ctrl-1)))
