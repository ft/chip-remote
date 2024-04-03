;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices texas-instruments lmh6517)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote device)
  #:use-module (chip-remote manufacturer texas-instruments)
  #:use-module (chip-remote interpreter)
  #:use-module (chip-remote item)
  #:use-module (chip-remote item builder)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:export (lmh6517))

(define lmh6517-fixed-gain 22)
(define lmh6517-attenuation-max 31.5)

(define-semantics lmh6517-gain
  (default (static 0))
  (range (lambda (s w) `(range ,(- lmh6517-fixed-gain
                                   lmh6517-attenuation-max)
                               ,lmh6517-fixed-gain)))
  (encode (make-evaluation
           `(lambda (w x) (multiply 2 (decrement ,lmh6517-fixed-gain x)))))
  (decode (make-evaluation
           `(lambda (w x) (decrement ,lmh6517-fixed-gain (multiply 0.5 x))))))

(define (make-lmh-reg ch)
  (register
   (items (list (reserved 0 1)
                (item (name (symbol-append 'gain- ch))
                      (offset 1)
                      (width 6)
                      (semantics lmh6517-gain))
                (item (name (symbol-append 'enable- ch))
                      (offset 7)
                      (width 1))))))

(define-device lmh6517
  (manufacturer texas-instruments)
  (homepage "http://www.ti.com/product/LMH6517")
  (datasheet "http://www.ti.com/lit/ds/symlink/lmh6517.pdf")
  (keywords '(low-power low-noise dual adc-driver digital vga))
  (register-width 16)
  (page-map
   (pm→
    (table
     (↔ (#f (rm→
             (table (↔ (0 (make-lmh-reg 'a))
                       (1 (make-lmh-reg 'b)))))))))))
