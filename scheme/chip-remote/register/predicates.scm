;; Copyright (c) 2019 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote register predicates)
  #:use-module (chip-remote register)
  #:use-module (chip-remote item)
  #:use-module ((chip-remote item access) #:prefix item:)
  #:use-module (chip-remote utilities)
  #:export (readable? writeable? read+write?))

(define (readable? reg)
  (all item:readable? (register-items reg)))

(define (writeable? reg)
  (all item:writeable? (register-items reg)))

(define (read+write? reg)
  (all item:read+write? (register-items reg)))
