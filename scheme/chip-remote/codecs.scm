;; Copyright (c) 2017-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote codecs)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote interpreter)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:use-module (rnrs bytevectors)
  #:export (boolean                     ; boolean semantics
            boolean/active-low
            unsigned-integer            ; integer semantics
            ones-complement
            twos-complement
            signed-magnitude
            offset-binary
            zig-zag
            ieee-754-single             ; floating point semantics
            ieee-754-double
            boolean-false?              ; utilities
            boolean-true?))

;; Boolean codecs

(define boolean-true  '(#t 1  on true  enable  enabled  yes))
(define boolean-false '(#f 0 off false disable disabled no))
(define true-and-false (append boolean-true boolean-false))

(define (boolean-true? x)
  (!! (member x boolean-true)))

(define (boolean-false? x)
  (!! (member x boolean-false)))

(define (boolean-range s w)
  (cons 'enumeration true-and-false))

(define (boolean-encode w x)
  (cond ((boolean-true? x) 1)
        ((boolean-false? x) 0)
        (else (throw 'invalid-boolean x))))

(define (boolean-decode w x)
  (cond ((zero? x) 'disabled)
        ((= 1 x) 'enabled)
        (else (throw 'invalid-boolean x))))

(define-semantics boolean
  (range boolean-range)
  (default (static #f))
  (encode boolean-encode)
  (decode boolean-decode))

(define invert-bit (lambda (w x) (logxor x 1)))

(define-semantics boolean/active-low
  (inherit boolean)
  (default (static #t))
  (encode (semantics-compose invert-bit boolean-encode))
  (decode (semantics-compose boolean-decode invert-bit)))

;; Integer codecs. For a nice summary of common signed integer encodings (un-
;; signed ones are easy, because that's just the identity function) take a look
;; at this application note from Intersil (now Renesas):
;;
;; https://www.renesas.com/us/en/document/apn/an9657-data-conversion-binary-code-formats
;;
;; All encodings need to know the width of an item to work.
;;
;; With some of these integer encodings you get the notion of two encodings for
;; zero (namely with one's complement and signed magnitude), often referred to
;; positive and negative zero. When encoding, these semantics opt for the posi-
;; tive variant of zero. The decoders map both variants to an unsigned zero.
;;
;; Another possible integer encoding is the so-called zig-zag encoding, used by
;; some of Google's protocol-buffers' data-types:
;;
;; https://developers.google.com/protocol-buffers/docs/encoding
;;
;; It has the property of using only few set bits for integers with small abso-
;; lute values, which can be beneficial when combined with variable length en-
;; coding.

(define ~ make-evaluation)

(define uint-codec (~ '(lambda (w x) (bit-mask w x))))

(define (unsigned-integer-max w) (1- (2e w)))
(define (unsigned-integer-min w) 0)

(define-semantics unsigned-integer
  (range (lambda (s w) `(range ,(unsigned-integer-min w)
                               ,(unsigned-integer-max w))))
  (default (static 0))
  (encode uint-codec)
  (decode uint-codec))

(define (twos-complement-min w) (- (2e (1- w))))
(define (twos-complement-max w) (1- (2e (1- w))))

(define-semantics twos-complement
  (range (lambda (s w) `(range ,(twos-complement-min w)
                               ,(twos-complement-max w))))
  (default (static 0))
  (encode (~ '(lambda (w x)
                (if (x >= 0)
                    (bit-mask (decrement w) x)
                    (bit-mask w (increment (complement (multiply -1 x))))))))
  (decode (~ '(lambda (w x)
                (let (t (left-shift 1 (decrement w)))
                  (let (r (decrement t))
                    (if (x < t)
                        (bit-mask (decrement w) x)
                        (multiply -1 (increment (bit-xor r (bit-and r x)))))))))))

(define (ones-complement-limit w) (- (1- (2e (1- w)))))

(define-semantics ones-complement
  (range (lambda (s w) `(range ,(ones-complement-limit w)
                               ,(ones-complement-limit w))))
  (default (static 0))
  (encode (~ '(lambda (w x)
                (if (x >= 0)
                    (bit-mask (decrement w) x)
                    (bit-mask w (complement (multiply -1 x)))))))
  (decode (~ '(lambda (w x)
                (let (t (left-shift 1 (decrement w)))
                  (let (r (decrement t))
                    (if (x < t)
                        (bit-mask (decrement w) x)
                        (multiply -1 (bit-xor r (bit-and r x))))))))))

(define signed-magnitude-limit ones-complement-limit)

(define-semantics signed-magnitude
  (range (lambda (s w) `(range ,(signed-magnitude-limit w)
                               ,(signed-magnitude-limit w))))
  (default (static 0))
  (encode (~ '(lambda (w x)
                (let (n (decrement w))
                  (if (x < 0)
                      (bit-mask n (multiply -1 x))
                      (bit-ior (left-shift 1 n) (bit-mask n x)))))))
  (decode (~ '(lambda (w x)
                (let (n (decrement w))
                  (let (v (bit-mask n x))
                    (let (s (bit-extract n 1 x))
                      (if (s = 0)
                          (multiply -1 v)
                          v))))))))

(define offset-binary-min twos-complement-min)
(define offset-binary-max twos-complement-max)

(define-semantics offset-binary
  (range (lambda (s w) `(range ,(offset-binary-min w)
                               ,(offset-binary-max w))))
  (default (static 0))
  (encode (~ '(lambda (w x)
                (let (half (left-shift 1 (decrement w)))
                  (bit-mask w (increment x half))))))
  (decode (~ '(lambda (w x)
                (let (half (left-shift 1 (decrement w)))
                  (decrement (bit-mask w x) half))))))

(define zig-zag-min twos-complement-min)
(define zig-zag-max twos-complement-max)

(define-semantics zig-zag
  (range (lambda (s w) `(range ,(offset-binary-min w)
                               ,(offset-binary-max w))))
  (default (static 0))
  (encode (~ '(lambda (w x)
                (bit-mask w (bit-xor (right-shift x (decrement w))
                                     (left-shift x 1))))))
  (decode (~ '(lambda (w x)
                (bit-xor (right-shift x 1)
                         (multiply -1 (bit-and x 1)))))))

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

(define ieee-754-single-max 3.402823466e38)
(define ieee-754-single-min (* -1 ieee-754-single-max))

(define-semantics ieee-754-single
  (range (lambda (s w) `(range ,ieee-754-single-min
                               ,ieee-754-single-max)))
  (default (static 0))
  (encode encode-ieee-754-single)
  (decode decode-ieee-754-single))

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

(define ieee-754-double-max 1.7976931348623158e308)
(define ieee-754-double-min (* -1 ieee-754-double-max))

(define-semantics ieee-754-double
  (range (lambda (s w) `(range ,ieee-754-double-min
                               ,ieee-754-double-max)))
  (default (static 0))
  (encode encode-ieee-754-double)
  (decode decode-ieee-754-double))
