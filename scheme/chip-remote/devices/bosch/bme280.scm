;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices bosch bme280)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote device)
  #:use-module (chip-remote devices bosch bme280 registers)
  #:use-module (chip-remote item)
  #:use-module (chip-remote manufacturer bosch)
  #:use-module (chip-remote modify)
  #:use-module (chip-remote named-value)
  #:use-module (chip-remote protocol)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register common)
  #:use-module (chip-remote register-map)
  #:export (bme280 bme280-reset-key bme280-chip-id))

(define bme280-reset-key #xb6)
(define bme280-chip-id #x60)

(define-register ctrl-frame #:contents (read? 15 1) (address 8 7) (data 0 8))

(define (encode-ctrl read? address data)
  ;; In SPI mode, only 7 bits of the register addresses are used; the
  ;; MSB of register address is not used and replaced by a read/write
  ;; bit (RW = ‘0’ for write and RW = ‘1’ for read). Example: address
  ;; 0xF7 is accessed by using SPI register address 0x77.   For write
  ;; access, the byte 0x77 is transferred, for read access, the byte
  ;; 0xF7 is transferred.
  (chain-modify* ctrl-frame
                 `(read?   ,read?)
                 `(address ,(bit-extract-width address 0 7))
                 `(data    ,data)))

(define (write-register c p n v)
  (transmit c (encode-ctrl #f n v)))

(define (read-register c p n)
  (let ((get-data (item-get (register-ref ctrl-frame 'data))))
    (get-data (transmit c (encode-ctrl #t n 0)))))

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
  '(pressure    . (concatenate    pressure-msb    pressure-lsb    pressure-xlsb))
  #:bus (spi #:frame-width 16)
  #:read read-register
  #:write write-register)
