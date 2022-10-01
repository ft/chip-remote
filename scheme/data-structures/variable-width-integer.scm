;; Copyright (c) 2022 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (data-structures variable-width-integer)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:export (varint-decode
            varint-encode
            varint-length
            varint-min-chunks
            varint-bits->chunks
            varint-last-octet?))

(define *value-bits-per-octet* 7)
(define *continuation-mask* #x80)
(define *value-mask* #x7f)

(define (varint-last-octet? o)
  (zero? (logand o *continuation-mask*)))

(define (varint-length bv offset)
  (let loop ((n 0))
    (if (varint-last-octet? (bytevector-u8-ref bv (+ offset n)))
        (1+ n)
        (loop (1+ n)))))

(define* (varint-min-chunks n s #:key width)
  (if width
      (if (zero? n)
          1
          (let* ((encoded (s:encode s width n))
                 (idx (inexact->exact (floor (1+ (log2 encoded))))))
            (varint-bits->chunks idx)))
      (let ((select (if (negative? n) car cdr))
            (compare (if (negative? n) >= <=))
            (lower1 (car (s:range s 1))))
        (when (and (negative? n) (zero? lower1))
          (throw 'invalid-integer-type n 'unsigned-semantics))
        (let loop ((width *value-bits-per-octet*)
                   (chunks 1))
          (let ((limit (select (s:range s width))))
            (if (compare n limit)
                chunks
                (loop (+ *value-bits-per-octet* width) (1+ chunks))))))))

(define (varint-bits->chunks bits)
  (inexact->exact (ceiling (/ bits (exact->inexact *value-bits-per-octet*)))))

(define (varint-extract bv offset octets)
  (let loop ((n (1- octets)) (acc 0))
    (if (negative? n)
        acc
        (let ((v (logand *value-mask* (u8vector-ref bv (+ offset n))))
              (pos (* *value-bits-per-octet* n)))
          (loop (1- n) (logior acc (ash v pos)))))))

(define* (varint-encode value #:key
                        return-used?
                        buffer width
                        (offset 0)
                        (semantics unsigned-integer))
  (when (and width (not (s:in-range? semantics width value)))
    (throw 'value-out-of-range value))
  (let* ((min-octets (varint-min-chunks value semantics #:width width))
         (octets (if width
                     (min min-octets (varint-bits->chunks width))
                     min-octets))
         (bits (or width (* *value-bits-per-octet* octets)))
         (bv (or buffer (make-u8vector octets 0)))
         (last (1- octets))
         (encoded (s:encode semantics bits value)))
    (let loop ((i 0))
      (if (= i octets)
          (if return-used?
              (cons octets bv)
              bv)
          (let* ((chunk (bit-extract-width encoded
                                           (* i *value-bits-per-octet*)
                                           *value-bits-per-octet*))
                 (octet (if (= i last)
                            chunk
                            (logior *continuation-mask* chunk))))
            (u8vector-set! bv (+ i offset) octet)
            (loop (1+ i)))))))

(define* (varint-decode bv #:key
                        return-consumed?
                        width
                        (offset 0)
                        (semantics unsigned-integer))
  (let* ((octets (varint-length bv offset))
         (bits (or width (* *value-bits-per-octet* octets)))
         (value (varint-extract bv offset octets))
         (return-value (s:decode semantics bits value)))
    (if return-consumed?
        (cons octets return-value)
        return-value)))

(define-syntax generate-shorthands
  (lambda (x)
    (syntax-case x ()
      ((_ (sems ...) (widths ...))
       #'(begin (generate-shorthands sems (widths ...)) ...))
      ((_ sem (widths ...))
       (identifier? #'sem)
       #'(begin (generate-shorthands sem widths) ...))
      ((_ s w)
       (and (identifier? #'s)
            (integer? (syntax->datum #'w)))
       (let ((base (symbol-append
                       'varint:
                       (match (syntax->datum #'s)
                         ('unsigned-integer 'uint)
                         ('twos-complement  'int)
                         ('zig-zag          'sint))
                       (number->symbol (syntax->datum #'w)))))
         (with-syntax ((enc (datum->syntax x (symbol-append base '-encode)))
                       (dec (datum->syntax x (symbol-append base '-decode))))
           #'(begin (define*-public (dec bv #:key return-consumed? (offset 0))
                      (varint-decode bv
                                     #:width w #:semantics s
                                     #:return-consumed? return-consumed?
                                     #:offset offset))
                    (define*-public (enc n #:key return-used? buffer (offset 0))
                      (varint-encode n
                                     #:width w #:semantics s
                                     #:return-used? return-used?
                                     #:buffer buffer
                                     #:offset offset)))))))))

(generate-shorthands (unsigned-integer twos-complement zig-zag)
                     (32 64 128 256 512))
