;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote register-window)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote item)
  #:use-module (chip-remote register)
  #:export (make-register-window
            register-window?
            msi-complete?
            lsi-complete?
            window-offset
            window-width
            window-meta
            window-items))

(define-immutable-record-type <register-window>
  (make-register-window* offset width msi-complete lsi-complete items)
  register-window?
  (msi-complete msi-complete?)
  (lsi-complete lsi-complete?)
  (offset window-offset)
  (width window-width)
  (meta window-meta)
  (items window-items))

(define (is-msi-complete? data o w)
  (let ((msi (last data)))
    (= (+ (item-offset msi) (item-width msi))
       (+ o w))))

(define (is-lsi-complete? data o)
  (= (item-offset (first data))
     o))

(define (want-item? i start-w w)
  (let* ((start-i (item-offset i))
         (end-i (+ start-i (item-width i) -1))
         (end-w (+ start-w w -1)))
    (or (and (>= start-i start-w) (<= start-i end-w))
        (and (>= end-i start-w) (<= end-i end-w)))))

(define (make-register-window register offset width)
  (let ((items (register-items:sorted register)))
    (let loop ((rest items) (acc '()))
      (if (null? rest)
          (let ((data (reverse acc)))
            (make-register-window* offset width
                                   (or (null? data)
                                       (is-msi-complete? data offset width))
                                   (or (null? data)
                                       (is-lsi-complete? data offset))
                                   data))
          (let ((this (car rest)) (rest (cdr rest)))
            (loop rest (if (want-item? this offset width)
                           (cons this acc)
                           acc)))))))
