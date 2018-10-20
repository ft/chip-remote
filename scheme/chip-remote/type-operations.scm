;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote type-operations)
  #:use-module (ice-9 match)
  #:use-module (chip-remote device)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote item)
  #:use-module (chip-remote utilities)
  #:export (value-fits-target?))

(define (value-fits-target? target value)
  (cond ((or (device? target)
             (page-map? target))
         (list-of-list-of-integers? value))
        ((register-map? target)
         (list-of-integers? value))
        ((register? target) (integer? value))
        (else (throw 'unknown-target target))))
