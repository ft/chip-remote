;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices analog-devices adf4158 tables)
  #:use-module (chip-remote named-value)
  #:export (readback-to-muxout-map
            negative-bleed-current-map))

(define-value readback-to-muxout-map
 '((disabled . #b00)
   (reserved . #b01)
   (enabled  . #b10)
   (reserved . #b11)))

(define-value negative-bleed-current-map
 '((disabled . #b00)
   (reserved . #b01)
   (reserved . #b10)
   (enabled  . #b11)))
