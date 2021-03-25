;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices bosch bme280)
  #:use-module (chip-remote device)
  #:use-module (chip-remote manufacturer bosch)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register common)
  #:use-module (chip-remote named-value)
  #:use-module (chip-remote devices bosch bme280 registers)
  #:export (bme280 bme280-reset-key bme280-chip-id))

(define bme280-reset-key #xb6)
(define bme280-chip-id #x60)

(define-device bme280
  #:manufacturer bosch
  #:homepage "https://www.bosch-sensortec.com/bst/products/all_products/bme280"
  #:datasheet "https://www.bosch-sensortec.com/media/boschsensortec/downloads/datasheets/bst-bme280-ds002.pdf"
  #:keywords '(digital humidity pressure temperature sensor)
  #:register-width 8
  #:register-map (#:table* (#xfe humidity-lsb)
                           (#xfd humidity-msb)
                           (#xfc temperature-xlsb)
                           (#xfb temperature-lsb)
                           (#xfa temperature-msb)
                           (#xf9 pressure-xlsb)
                           (#xf8 pressure-lsb)
                           (#xf7 pressure-msb)
                           (#xf5 config)
                           (#xf4 control-a)
                           (#xf3 status)
                           (#xf2 control-b)
                           (#xe0 reset)
                           (#xd0 chip-id))
  #:combinations
  '(humidity    . (concatenate    humidity-msb    humidity-lsb))
  '(temperature . (concatenate temperature-msb temperature-lsb temperature-xlsb))
  '(pressure    . (concatenate    pressure-msb    pressure-lsb    pressure-xlsb)))
