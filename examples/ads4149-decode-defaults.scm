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
             (chip-remote decode to-text)
             (chip-remote register-map))

(format #t "Default register-map of TI's ADS4149 ADU:~%")

(map (lambda (x)
       (newline)
       (display-list
        (register->text #:register-map ads4149-register-map
                        #:address x
                        #:width
                        8
                        #:value (register-default ads4149-register-map x)
                        #:colour? #t)))
     (map car ads4149-register-map))
