;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote decode)
  #:use-module (ice-9 control)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote interpreter)
  #:use-module (chip-remote device)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote item)
  #:export (decode))

(define decoder-table
  `((,item? . ,item-decode)))

(define (find-decoder description)
  (call/ec (lambda (return)
             (let loop ((rest decoder-table))
               (if (null? rest)
                   (throw 'cannot-decode description)
                   (if ((caar rest) description)
                       (return (cdar rest))
                       (loop (cdr rest))))))))

(define (decode description value)
  (let ((decoder (find-decoder description)))
    (decoder description value)))
