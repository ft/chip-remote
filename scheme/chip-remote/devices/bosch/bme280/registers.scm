;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices bosch bme280 registers)
  #:use-module (chip-remote item)
  #:use-module (chip-remote named-value)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register common)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
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
    (items (list (‣ name     4 4)
                 (‣ reserved 0 4)))))

(define-u8-register   humidity-lsb     (default #x00))
(define-u8-register   humidity-msb     (default #x80))
(define-xlsb-register temperature-xlsb)
(define-u8-register   temperature-lsb  (default #x00))
(define-u8-register   temperature-msb  (default #x80))
(define-xlsb-register pressure-xlsb)
(define-u8-register   pressure-lsb     (default #x00))
(define-u8-register   pressure-msb     (default #x80))
(define-u8-register   reset            (default #x00))
(define-u8-register   chip-id          (default #x60))

(define-semantics stand-by-time
  (range (table-lookup stand-by-map))
  (default (static (caar (value-data stand-by-map)))))

(define-semantics filter-coefficient
  (range (table-lookup filter-coefficient-map))
  (default (static (caar (value-data filter-coefficient-map)))))

(define-semantics oversampling
  (range (table-lookup oversampling-map))
  (default (static (caar (value-data oversampling-map)))))

(define-semantics mode
  (range (table-lookup mode-map))
  (default (static (caar (value-data mode-map)))))

(define-register config
  (items (list
          (‣ stand-by-time      5 3 (semantics stand-by-time))
          (‣ filter-coefficient 2 3 (semantics filter-coefficient))
          (‣ reserved 1 1)
          (‣ spi-three-wire-enable? 0 1))))

(define-register control-a
  (items (list
          (‣ temperature-oversampling 5 3 (semantics oversampling))
          (‣ pressure-oversampling    2 3 (semantics oversampling))
          (‣ mode                     0 2 (semantics mode)))))

(define-register control-b
  (items (list
          (‣ reserved 3 5)
          (‣ humidity-oversampling 0 3 (semantics oversampling)))))

(define-register status
  (items (list
          (‣ reserved 4 4)
          (‣ measurement-active? 3 1)
          (‣ reserved 1 2)
          (‣ image-register-update-active? 0 1))))
