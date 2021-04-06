;; Copyright (c) 2014-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (chip-remote devices fhac tiadc registers)
             (chip-remote decode)
             (chip-remote level-3)
             (chip-remote register-map))

(format #t "Default register-map of FHAC-LNT's TIADC FPGA:~%~%")

(define rm tiadc-register-map)
(define rw tiadc-register-width)
(define ic tiadc-regmap-interconnections)

(device-decoder #:register-map rm
                #:width rw
                #:interconnections ic
                #:colour? #t
                #:decoder (lambda (addr value) (decode rm addr value))
                #:filter-predicate (lambda (x) (<= x 6))
                #:reader (lambda (addr) (regmap->item rm addr 'default-value)))
