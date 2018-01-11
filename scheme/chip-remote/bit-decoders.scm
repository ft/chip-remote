;; Copyright (c) 2014-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote bit-decoders)
  #:use-module (ice-9 format)
  #:export (binwidth->hexwidth
            boolean-status
            boolean-status-inverted
            literal-binary
            literal-hex
            logic-active-high
            logic-active-low
            power-of-two
            reverse-lookup
            twos-complement
            unsigned-integer))

(define (binwidth->hexwidth n)
  (let ((mod (modulo n 4)))
    (+ (floor (/ n 4)) (if (> mod 0) 1 0))))

(define (power-of-two n o w value)
  (expt 2 value))

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

(define (literal-hex name offset width value)
  (format #f "0x~v,'0x" (binwidth->hexwidth width) value))

(define (logic-active-high n o w x)
  (if (= 0 x) #f #t))

(define (logic-active-low n o w x)
  (if (= 0 x) #t #f))

(define (boolean-status n o w x)
  (if (zero? x) 'false 'true))

(define (boolean-status-inverted n o w x)
  (if (zero? x) 'true 'false))

(define (reverse-lookup valmap value)
  (let next ((cur valmap))
    (if (null? cur)
        'undefined
        (let ((k (caar cur))
              (v (cdar cur)))
          (if (= value v)
              k
              (next (cdr cur)))))))
