;; Copyright (c) 2019 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote item access)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote utilities)
  #:export (make-access
            none ro wo rw
            readable?
            writeable?
            read+write?))

(define-immutable-record-type <access-ctrl>
  (make-access kind)
  access-ctrl?
  (kind ac-kind))

(define (none) (make-access '()))
(define (ro)   (make-access '(read)))
(define (wo)   (make-access '(write)))
(define (rw)   (make-access '(read write)))

(define (readable? x)   (!! (member 'read  (ac-kind x))))
(define (writeable? x)  (!! (member 'write (ac-kind x))))
(define (read+write? x) (and (readable? x) (writeable? x)))
