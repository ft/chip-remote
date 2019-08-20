;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote codecs)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote named-value)
  #:use-module (rnrs bytevectors)
  #:export (boolean-true?
            boolean-false?
            decode-boolean
            decode-boolean/active-low
            encode-boolean
            encode-boolean/active-low
            decode-unsigned-integer
            encode-unsigned-integer
            decode-ones-complement
            encode-ones-complement
            decode-twos-complement
            encode-twos-complement
            decode-offset-binary
            encode-offset-binary
            decode-signed-magnitude
            encode-signed-magnitude
            decode-ieee-754-single
            encode-ieee-754-single
            decode-ieee-754-double
            encode-ieee-754-double
            decode-with-table
            make-table-decoder
            encode-with-table
            make-table-encoder))

;; Boolean codecs

(define (invert-bit x)
  (logxor x 1))

(define (boolean-true? x)
  (case x
    ((#t 1 on true enable enabled yes) #t)
    (else #f)))

(define (boolean-false? x)
  (case x
    ((#f 0 off false disable disabled no) #t)
    (else #f)))

(define (decode-boolean x)
  (cond ((zero? x) 'disabled)
        ((= 1 x) 'enabled)
        (else (throw 'invalid-boolean x))))

(define (decode-boolean/active-low x)
  (decode-boolean (invert-bit x)))

(define (encode-boolean x)
  (cond ((boolean-true? x) 1)
        ((boolean-false? x) 0)
        (else (throw 'invalid-boolean x))))

(define (encode-boolean/active-low x)
  (invert-bit (encode-boolean x)))

;; Integer codecs. For a nice summary of common signed integer encodings (un-
;; signed ones are easy, because that's just the identity function) take a look
;; at this application note from Intersil:
;;
;; https://www.intersil.com/content/dam/Intersil/documents/an96/an9657.pdf
;;
;; All encodings need to know the width of an item to work.

(define (encode-unsigned-integer width value)
  (logand (one-bits width) value))

(define decode-unsigned-integer encode-unsigned-integer)

(define (encode-twos-complement width value)
  (if (>= value 0)
      value
      (+ 1 (logxor (one-bits width) (* -1 value)))))

(define (decode-twos-complement width value)
  (let* ((top-bit (ash 1 (- width 1)))
         (rest (- top-bit 1)))
    (if (< value top-bit)
        value
        (* -1 (+ 1 (logxor rest (logand rest value)))))))

(define (encode-ones-complement width value)
  (if (>= value 0)
      value
      (logxor (one-bits width) (* -1 value))))

(define (decode-ones-complement width value)
  (let* ((top-bit (ash 1 (- width 1)))
         (rest (- top-bit 1)))
    (if (< value top-bit)
        value
        (* -1 (logxor rest (logand rest value))))))

(define (encode-signed-magnitude width value)
  (if (negative? value)
      value
      (let ((top-bit (ash 1 (- width 1))))
        (logior top-bit value))))

(define (decode-signed-magnitude width value)
  (let* ((w (- width 1))
         (v (bit-extract-width value 0 w)))
    (if (zero? (bit-extract-width value w 1))
        (* -1 v)
        v)))

(define (encode-offset-binary width value)
  (let ((half (ash 1 (- width 1))))
    (+ value half)))

(define (decode-offset-binary width value)
  (let ((half (ash 1 (- width 1))))
    (- value half)))

(define (ensure-width! tag actual required)
  (unless (= actual required)
    (throw 'invalid-width tag required actual)))

(define (encode-ieee-754-single width value)
  (ensure-width! 'ieee-754-single width 32)
  (let ((bv (make-bytevector 4 0)))
    (bytevector-ieee-single-set! bv 0 value 'big)
    (bytevector-u32-ref bv 0 'big)))

(define (decode-ieee-754-single width value)
  (ensure-width! 'ieee-754-single width 32)
  (let ((bv (make-bytevector 4 0)))
    (bytevector-u32-set! bv 0 value 'big)
    (bytevector-ieee-single-ref bv 0 'big)))

(define (encode-ieee-754-double width value)
  (ensure-width! 'ieee-754-double width 64)
  (let ((bv (make-bytevector 8 0)))
    (bytevector-ieee-double-set! bv 0 value 'big)
    (bytevector-u64-ref bv 0 'big)))

(define (decode-ieee-754-double width value)
  (ensure-width! 'ieee-754-double width 64)
  (let ((bv (make-bytevector 8 0)))
    (bytevector-u64-set! bv 0 value 'big)
    (bytevector-ieee-double-ref bv 0 'big)))

;; Table lookup based codecs

(define (make-table-decoder table)
  (lambda (x) (decode-with-table table x)))

(define (decode-with-table table value)
  (let loop ((rest (if (named-value? table) (value-data table) table)))
    (if (null? rest)
        'undefined
        (let ((k (caar rest))
              (v (cdar rest)))
          (if (= value v)
              k
              (loop (cdr rest)))))))

(define (make-table-encoder table)
  (lambda (x) (encode-with-table table x)))

(define (encode-with-table table key)
  (let ((value (assoc key (if (named-value? table)
                              (value-data table)
                              table))))
    (if value
        (cdr value)
        'undefined)))
