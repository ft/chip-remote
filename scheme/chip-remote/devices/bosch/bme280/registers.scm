;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices bosch bme280 registers)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register common)
  #:use-module (chip-remote devices bosch bme280 tables)
  #:export (humidity-lsb
            humidity-msb
            temperature-xlsb
            temperature-lsb
            temperature-msb
            pressure-xlsb
            pressure-lsb
            pressure-msb
            reset
            chip-id
            config
            control-a
            control-b
            status))

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
