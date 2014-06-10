;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; This example imports the ADS4149's register map from its ‘registers’ module,
;; then uses the ‘register-default’ function (from the ‘register-map’ module)
;; to retrieve default register values from a register-map and then uses the
;; ‘register->test’ procedure to produce human-readable, colourfully decoded
;; for each and every register. Finally, the ‘display-list’ procedure prints
;; the generated data to stdout.

(use-modules (chip-remote devices ti ads4149 registers)
             (chip-remote decode)
             (chip-remote level-3)
             (chip-remote register-map))

(format #t "Default register-map of TI's ADS4149 ADU:~%~%")

;; Shorthands:
(define rm ads4149-register-map)
(define rw ads4149-register-width)
(define ic ads4149-regmap-interconnections)

(device-decoder #:register-map rm
                #:width rw
                #:interconnections ic
                ;; Let's force colour output in this case:
                #:colour? #t
                ;; Decoders are easy: They need a register-map, an address and
                ;; a value, and then they'll do their thing. The results are
                ;; best when the register-map is properly annotated. The
                ;; register-map of the ADS4149 is.
                #:decoder (lambda (addr value) (decode rm addr value))
                ;; The reader function takes one argument: An address. It takes
                ;; that and produces a corresponding register value. It can do
                ;; that by querying a live device on a board. But it doesn't
                ;; have to. Since we want the default values of the registers
                ;; (and the register map of the ADS4149 defines one for every
                ;; register), we can also just look up the default value in the
                ;; register-map and return that, thus fulfilling the purpose of
                ;; this script.
                #:reader (lambda (addr) (regmap->item rm addr 'default-value)))
