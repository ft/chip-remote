;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices analog-devices adf4158 registers)
  #:use-module (chip-remote item)
  #:use-module (chip-remote item builder)
  #:use-module (chip-remote register)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote devices analog-devices adf4158 tables)
  #:use-module ((chip-remote devices analog-devices adf4169 registers)
                #:prefix adf4169:)
  #:export (reg:lsb-frac
            reg:function
            reg:test
            reg:deviation
            reg:delay))

(define reg:lsb-frac
  (derive-register-from adf4169:reg:lsb-frac
    #:remove '(phase-adjust reserved)
    #:insert (reserved 28 4)))

(define reg:function
  (derive-register-from adf4169:reg:function
    #:remove `((offset ,>= 16))
    #:insert (reserved 16 16)))

(define reg:test
  (derive-register-from adf4169:reg:clock
    #:remove '(clock-divider-select ramp-status reserved)
    #:insert (list (reserved 3 4)
                   (reserved 25 1)
                   (generate-item readback-to-muxout?
                     #:offset 21
                     #:width 2
                     #:default 'disabled
                     #:semantics lookup readback-to-muxout-map
                     #:and-validate ∉ reserved)
                   (generate-item negative-bleed-current?
                     #:offset 23
                     #:width 2
                     #:default 'disabled
                     #:semantics lookup negative-bleed-current-map
                     #:and-validate ∉ reserved))))

(define reg:deviation
  (derive-register-from adf4169:reg:deviation
    #:remove `((offset ,>= 30) reserved)
    #:insert (list (reserved 30 2)
                   (generate-item parabolic-ramp-enable?
                     #:offset 28
                     #:width 1
                     #:default #f))))

(define reg:delay
  (derive-register-from adf4169:reg:delay
    #:remove `((offset ,>= 18))
    #:insert (list (reserved 19 13)
                   (generate-item ramp-delay-fast-lock?
                     #:offset 18
                     #:width 1
                     #:default #f))))
