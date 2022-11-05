;; Copyright (c) 2022 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; This module implements framing for data stream channels (like TCP) by way of
;; prefixing a frame with its length in octets, encoded by a variable-length
;; integer value.
;;
;; Leveraging Scheme's arbitrary sized integers, this is allows for frames of
;; any size.

(define-module (protocol length-prefix)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (data-structures variable-width-integer)
  #:export (send-bytevector
            recv-bytevector))

(define (io-exactly! fnc port bv offset)
  (let ((bvl (bytevector-length bv)))
    (if (>= offset bvl)
        bv
        (let* ((n (- bvl offset))
               (m (fnc port bv offset n)))
          (if (eof-object? m)
              eof-object
              (io-exactly! fnc port bv (+ offset m)))))))

(define (get-exactly! port bv offset)
  (io-exactly! get-bytevector-n! port bv offset))

(define (send-bytevector port bv)
  (let ((pfx (varint-encode (bytevector-length bv))))
    (put-bytevector port pfx)
    (put-bytevector port bv)
    (+ (bytevector-length pfx)
       (bytevector-length bv))))

(define (fetch-prefix port lst)
  (cond ((null? lst) (fetch-prefix port (cons (get-u8 port) lst)))
        ((varint-last-octet? (car lst)) (list->u8vector (reverse lst)))
        (else (fetch-prefix port (cons (get-u8 port) lst)))))

(define* (recv-bytevector port #:key (limit #f))
  (let ((n (varint-decode (fetch-prefix port '()))))
    (cond ((eof-object? n) n)
          ((and limit (> n limit)) #f)
          (else (let ((bv (make-bytevector n)))
                  (get-exactly! port bv 0))))))
