;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test validate unique-items)
  #:use-module (srfi srfi-1)
  #:use-module (test tap)
  #:use-module (chip-remote device)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote utilities)
  #:export (unique-items/count
            unique-items/check))

(define (unique-items/check dev cfg)
  (define-test (format #f "~a's item names are unique" (device-name dev))
    (pass-if-null?
     (let loop ((rest (remove (lambda (x)
                                (member x (kwa-ref cfg #:multi-items '())))
                              (device-item-names dev)))
                (done '())
                (multiples '()))
       (if (null? rest)
           multiples
           (let ((multi? (!! (member (car rest) done)))
                 (multi?? (!! (member (car rest) multiples))))
             (loop (cdr rest)
                   (if multi? done (cons (car rest) done))
                   (if (and multi? (not multi??))
                       (cons (car rest) multiples)
                       multiples))))))))

(define (unique-items/count dev cfg)
  1)
