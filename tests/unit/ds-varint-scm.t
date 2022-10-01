;; -*- scheme -*-

;; Copyright (c) 2022 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 match)
             (srfi srfi-1)
             (test tap)
             (test setup)
             (chip-remote codecs)
             (chip-remote semantics)
             (data-structures variable-width-integer))

(init-test-tap!)

(define-syntax-rule (~ exp) (cons exp 'exp))

(define* (test-codec #:key
                     decoded encoded width
                     (semantics (~ unsigned-integer)))
  (define-test (format #f "~a,~a,~a → ~a"
                       encoded width (cdr semantics) decoded)
    (pass-if-= (varint-decode encoded
                              #:width width
                              #:semantics (car semantics))
               decoded))
  (define-test (format #f "~a,~a,~a → ~a"
                       decoded width (cdr semantics) encoded)
    (pass-if-equal? (varint-encode decoded
                                   #:width width
                                   #:semantics (car semantics))
                    encoded)))

(define chunk-size-checks
  `((    0      1   ,(~ unsigned-integer))
    (  127      1   ,(~ unsigned-integer))
    (  128      2   ,(~ unsigned-integer))
    ;; The #f denotes, that this will throw, because negative values cannot be
    ;; encoded using unsigned integer semantics, obviously.
    (  -12     #f   ,(~ unsigned-integer))
    ( 1024      2   ,(~ unsigned-integer))
    (16383      2   ,(~ unsigned-integer))
    (16384      3   ,(~ unsigned-integer))
    (    0      1   ,(~ twos-complement))
    (   -1      1   ,(~ twos-complement))
    (    1      1   ,(~ twos-complement))
    (   63      1   ,(~ twos-complement))
    (  -64      1   ,(~ twos-complement))
    (   64      2   ,(~ twos-complement))
    (  -65      2   ,(~ twos-complement))
    (    0      1   ,(~ zig-zag))
    (   -1      1   ,(~ zig-zag))
    (    1      1   ,(~ zig-zag))
    (   63      1   ,(~ zig-zag))
    (  -64      1   ,(~ zig-zag))
    (   64      2   ,(~ zig-zag))
    (  -65      2   ,(~ zig-zag))))

(define (test-chunk-size-calculation spec)
  (match spec
    ((value expected (semantics . name))
     (if expected
         (define-test (format #f "~a with ~a semantics needs ~a 7-bit chunks"
                              value name expected)
           (pass-if-= expected (varint-min-chunks value semantics)))
         (define-test (format #f "~a with ~a semantics cannot be encoded"
                              value name)
           (pass-if-exception 'invalid-integer-type
                              (varint-min-chunks value semantics)))))))

(define tc32-range (s:range twos-complement 32))
(define tc32-min (car tc32-range))
(define tc32-max (cdr tc32-range))

(define zz32-range (s:range zig-zag 32))
(define zz32-min (car zz32-range))
(define zz32-max (cdr zz32-range))

(define max-width-tests
  ;; width     value   byte-vector                       semantics
  `((   32         0   #vu8(#x00)                       ,(~ unsigned-integer))
    (   32         1   #vu8(#x01)                       ,(~ twos-complement))
    (   32        -1   #vu8(#xff #xff #xff #xff #x0f)   ,(~ twos-complement))
    (   32        -1   #vu8(#x01)                       ,(~ zig-zag))
    (   32      1234   #vu8(#xd2 #x09)                  ,(~ twos-complement))
    (   32     -1234   #vu8(#xae #xf6 #xff #xff #x0f)   ,(~ twos-complement))
    (   32      1234   #vu8(#xa4 #x13)                  ,(~ zig-zag))
    (   32     -1234   #vu8(#xa3 #x13)                  ,(~ zig-zag))
    (   32 ,tc32-min   #vu8(#x80 #x80 #x80 #x80 #x08)   ,(~ twos-complement))
    (   32 ,tc32-max   #vu8(#xff #xff #xff #xff #x07)   ,(~ twos-complement))
    (   32 ,zz32-min   #vu8(#xff #xff #xff #xff #x0f)   ,(~ zig-zag))
    (   32 ,zz32-max   #vu8(#xfe #xff #xff #xff #x0f)   ,(~ zig-zag))))

(define (test-max-width-checks spec)
  (match spec
    ((width value expected semantics)
     (test-codec #:decoded value
                 #:encoded expected
                 #:width width
                 #:semantics semantics))))

(with-fs-test-bundle
  (plan 70)

  (define-test "last-octet? 0x80 → #f"
    (pass-if-false (varint-last-octet? #x80)))
  (define-test "last-octet? 0x7f → #t"
    (pass-if-true (varint-last-octet? #x7f)))

  (let* ((bv #vu8(#xff #xff #xff #xff #xff #xff #xff #x20))
         (n (u8vector-length bv)))
    (let loop ((k (1- n)))
      (unless (negative? k)
        (define-test
            (format #f "len ~a → ~a" (drop (u8vector->list bv) k) (- n k))
          (pass-if-= (varint-length bv k)
                     (- n k))
          (loop (- k 1))))))

  (define-test "32bit varints need at most 5 octets to encode"
    (pass-if-= 5 (varint-bits->chunks 32)))

  (define-test "64bit varints need at most 10 octets to encode"
    (pass-if-= 5 (varint-bits->chunks 32)))

  (for-each test-chunk-size-calculation chunk-size-checks)

  ;; This is very simple because it fits within 7 bits of data, so it's just
  ;; one octet encoded.
  (test-codec #:decoded 32
              #:encoded #vu8(#x20))

  ;; 1234dec   -binary→   0001001   1010010   (7-bit chunks)
  ;; Little endian:       1010010 ↔ 0001001
  ;; Continuation bits:  11010010  00001001
  (test-codec #:decoded 1234
              #:encoded #vu8(#b11010010 #b00001001))

  ;; With given width, this should still be the same result.
  (test-codec #:decoded 1234
              #:encoded #vu8(#b11010010 #b00001001)
              #:width 32)

  (test-codec #:decoded 16383
              #:encoded #vu8(#b11111111 #b01111111)
              #:width 14)

  (define-test "16384 cannot be encoded in a 14 bit integer"
    (pass-if-exception 'value-out-of-range
                       (varint-encode 16384 #:width 14)))
  (let ((buffer (make-u8vector 8 0)))
    (define-test "Encoding into existing buffer works"
      (pass-if-equal? (varint-encode 1234
                                     #:buffer buffer
                                     #:offset 2)
                      #vu8(0 0 #b11010010 #b00001001 0 0 0 0)))
    (define-test "Provided buffer was modified as expected"
      (pass-if-equal? buffer #vu8(0 0 #b11010010 #b00001001 0 0 0 0))))

  (define-test "Encoder returns the correct number of octets used"
    (pass-if-= (car (varint-encode 1234 #:return-used? #t))
               2))
  (define-test "Decoder returns the correct number of octets consumed"
    (pass-if-= (car (varint-decode #vu8(#b11010010 #b00001001 0 0 0 0 0 0)
                                   #:return-consumed? #t))
               2))

  (for-each test-max-width-checks max-width-tests))
