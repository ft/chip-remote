;; Copyright (c) 2020 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote register-map utilities)
  #:export (modify offset))

(define (offset n) (lambda (r) (cons (+ (car r) n) (cdr r))))
(define (modify p) (lambda (r) (cons (car r)       (p (cdr r)))))
