;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote bit-decoders)
  #:use-module (ice-9 format)
  #:export (literal-binary
            logic-active-high
            logic-active-low
            reverse-lookup
            twos-complement
            unsigned-integer))

(define (unsigned-integer n o w value)
  value)

(define (twos-complement name offset width value)
  (let* ((top-bit (ash 1 (- width 1)))
         (rest (- top-bit 1)))
    (if (< value top-bit)
        value
        (* -1 (+ 1 (logxor rest (logand rest value)))))))

(define (literal-binary name offset width value)
  (format #f "~v,'0b" width value))

(define (logic-active-high n o w x)
  (if (= 0 x) #f #t))

(define (logic-active-low n o w x)
  (if (= 0 x) #t #f))

(define (reverse-lookup valmap value)
  (let next ((cur valmap))
    (if (null? cur)
        'undefined
        (let ((k (caar cur))
              (v (cdar cur)))
          (if (= value v)
              k
              (next (cdr cur)))))))
