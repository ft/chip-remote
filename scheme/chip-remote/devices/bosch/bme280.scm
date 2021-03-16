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
  #:export (bme280
            bme280-reset-key
            bme280-chip-id))

(define bme280-reset-key #xb6)
(define bme280-chip-id #x60)

(define-value mode-map '((sleep  . #b00)
                         (forced . #b01)
                         (forced . #b10)
                         (normal . #b11)))

(define-value oversampling-map '((disable . #b000)
                                 (      1 . #b001)
                                 (      2 . #b010)
                                 (      4 . #b011)
                                 (      8 . #b100)
                                 (     16 . #b101)
                                 (     16 . #b110)
                                 (     16 . #b111)))

(define-value stand-by-map '((   #e0.5e-3 . #b000)
                             (  #e62.5e-3 . #b001)
                             ( #e125.0e-3 . #b010)
                             ( #e250.0e-3 . #b011)
                             ( #e500.0e-3 . #b100)
                             (#e1000.0e-3 . #b101)
                             (  #e10.0e-3 . #b110)
                             (  #e20.0e-3 . #b111)))

(define-value filter-coefficient-map '((filter-off . #b000)
                                       (         2 . #b001)
                                       (         4 . #b010)
                                       (         8 . #b011)
                                       (        16 . #b100)
                                       (        16 . #b101)
                                       (        16 . #b110)
                                       (        16 . #b111)))

(define-syntax-rule (define-xlsb-register name)
  (define-register name
    #:default 0
    #:contents
    (name 4 4)
    (reserved 0 4)))

(define-u8-register   humidity-lsb     #:default #x00)
(define-u8-register   humidity-msb     #:default #x80)
(define-xlsb-register temperature-xlsb)
(define-u8-register   temperature-lsb  #:default #x00)
(define-u8-register   temperature-msb  #:default #x80)
(define-xlsb-register pressure-xlsb)
(define-u8-register   pressure-lsb     #:default #x00)
(define-u8-register   pressure-msb     #:default #x80)
(define-u8-register   reset            #:default #x00)
(define-u8-register   chip-id          #:default #x60)

(define-register config
  #:default 0
  #:contents
  (stand-by-time 5 3 #:semantics lookup stand-by-map)
  (filter-coefficient 2 3 #:semantics lookup filter-coefficient-map)
  (reserved 1 1)
  (spi-three-wire-enable? 0 1))

(define-register control-a
  #:default 0
  #:contents
  (temperature-oversampling 5 3 #:semantics lookup oversampling-map)
  (pressure-oversampling 2 3 #:semantics lookup oversampling-map)
  (mode 0 2 #:semantics lookup mode-map))

(define-register control-b
  #:default 0
  #:contents
  (reserved 3 5)
  (humidity-oversampling 0 3 #:semantics lookup oversampling-map))

(define-register status
  #:default 0
  #:contents
  (reserved 4 4)
  (measurement-active? 3 1)
  (reserved 1 2)
  (image-register-update-active? 0 1))

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
                           (#xd0 chip-id)))
