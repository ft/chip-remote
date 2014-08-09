;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; This example is very much like the ADS4149 example, except that it also
;; employs a ‘#:filter-predicate’ function to weed out write-only registers.

(use-modules (chip-remote devices ti lmk04828 registers)
             (chip-remote decode)
             (chip-remote level-3)
             (chip-remote register-map))

(format #t "Default register-map of TI's LMK04828 Clock chip:~%~%")

;; Shorthands:
(define rm lmk04828-register-map)
(define rw lmk04828-register-width)
(define ic lmk04828-regmap-interconnections)

(device-decoder #:register-map rm
                #:width rw
                ;; Registers with addresses above #x1000 are write-only on this
                ;; device. The module is able to interpret values for them if
                ;; it is presented with them, but for realism this example
                ;; filters them out, like the real device-decoder function
                ;; would. If you're interested anyway, comment the following
                ;; line:
                #:filter-predicate (lambda (x) (< x #x1000))
                #:interconnections ic
                ;; Colouring takes a lot of time with large output like this.
                ;; Maybe the use of Guile's profiler is warranted, to speed
                ;; things up at bit.
                #:colour? #t
                #:decoder (lambda (addr value) (decode rm addr value))
                #:reader (lambda (addr) (regmap->item rm addr 'default-value)))
