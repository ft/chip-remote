;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (protocol net-unicode)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:export (string->net-unicode
            net-unicode->string))

;; RFC5198 specifies a way to encode unicode text for use in text-based
;; protocols. This specification is used by CoAP (RFC7252).
;;
;; Yeah, so… …this is not really an implementation of said rfc. I just need
;; something that works for short strings for now, so I can keep working on
;; CoAP.

(define (string->net-unicode s)
  (string->bytevector s "utf-8"))

(define* (net-unicode->string bv #:key (offset 0) length)
  (let ((bv* (if (zero? offset)
                 bv
                 (let* ((n (or length (- (bytevector-length bv) offset)))
                        (new (make-bytevector n)))
                   (bytevector-copy! bv offset new 0 n)
                   new))))
    (bytevector->string bv* "utf-8")))
