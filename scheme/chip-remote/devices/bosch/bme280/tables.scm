;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices bosch bme280 tables)
  #:use-module (chip-remote named-value)
  #:export (filter-coefficient-map
            mode-map
            oversampling-map
            stand-by-map))

(define-value filter-coefficient-map '((filter-off . #b000)
                                       (         2 . #b001)
                                       (         4 . #b010)
                                       (         8 . #b011)
                                       (        16 . #b100)
                                       (        16 . #b101)
                                       (        16 . #b110)
                                       (        16 . #b111)))

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
