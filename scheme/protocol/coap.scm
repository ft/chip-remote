;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; The  Constrained Application  Protocol  (CoAP) is  a  transfer protocol  for
;; systems that use networks  and nodes that are constrained in  one way or the
;; other. It is specified in RFC7252. The transport used in that system is UDP.
;; Extensions for transport over TCP and  TLS exist (RFC8323), and it should be
;; straight forward to use serial links as  a transport medium as well, given a
;; suitable framing  algorithm like  SLIP (RFC1055)  or Constant  Overhead Byte
;; Stuffing (COBS).
;;
;; This is not a  complete implementation of CoAP. It is  intended to be enough
;; of a subset to investigate the possibility  of using the protocol as the new
;; transport in chip-remote.
;;
;; Not all details of  CoAP seem ideal, but it does solve  the issue of service
;; discovery and establishing information context.  Plus, being an RFCed, means
;; there are numerous implementations of it — in Zephyr, for example — but, ob-
;; viously, not for GNU Guile…

(define-module (protocol coap)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (protocol coap message)
  #:use-module (protocol coap option)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote utilities)
  #:export (decode encode encode*))

;; A CoAP message is comprised of at least four octet, followed by optional
;; token, option list and payload (which if it exists is prefixed by a 0xff
;; octet:
;;
;;  0                   1                   2                   3   Deci. Index
;;  0               1               2               3               Octet Index
;;  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1   Bit Index
;; +---+---+-------+---------------+-------------------------------+
;; |Ver| T |  TKL  |      Code     |          Message ID           |
;; +---+---+-------+---------------+-------------------------------+
;; |   Token (if any, TKL bytes) ...                               |
;; +---------------------------------------------------------------+
;; |   Options (if any) ...                                        |
;; +---------------+-----------------------------------------------+
;; |1 1 1 1 1 1 1 1|    Payload (if any) ...                       |
;; +---------------+-----------------------------------------------+
;;
;; The octet-order for multi-octet data is network order (big endian). Most
;; encodings are straight forward. Only the option encoding is a little more
;; involved. See the (protocol coap option) module for details.

(define (coap-encode version type code id toklen token options paylen payload)
  (let* ((min-len 4)
         (tok-offset min-len)
         (opt-offset (+ tok-offset toklen))
         (hdr-len (+ min-len toklen
                     (options-size options)
                     (if (positive? paylen) 1 0)))
         (msg (make-bytevector (+ hdr-len paylen))))
    (bytevector-u8-set! msg 0
                        (chain (put 4 version)
                               (put 6 type)
                               (put 0 toklen)))
    (bytevector-u8-set! msg 1
                        (chain (put 5 (car code))
                               (put 0 (cdr code))))
    (bytevector-u16-set! msg 2 id 'big)
    (when (positive? toklen)
      (bytevector-uint-set! msg tok-offset token 'big toklen))
    (options-put msg (+ tok-offset toklen) options)
    (when (positive? paylen)
      (bytevector-u8-set! msg (1- hdr-len) #xff)
      (bytevector-copy! payload 0 msg hdr-len paylen))
    msg))

;; This this the user-facing CoAP encoder: It provides a common interface for
;; generating any and all possible CoAP messages. Beyond that it assembles all
;; required data in a convenient format for the low-level coap-encode function
;; to consume.
(define* (encode #:key payload token message-id
                 (options '())
                 (version 1)
                 (type 'non-confirmable)
                 (code 'empty))

  (unless (message-type? type)
    (throw 'coap:encode:invalid-message-type type))

  (when (and token (not (non-negative-integer? token)))
    (throw 'coap:encode:invalid-token token))

  (when (and options (not (list? options)))
    (throw 'coap:encode:invalid-options options))

  ;; TODO: Are there more possible payload types? Strings maybe, that
  ;; coap-encode could generate?
  (when (and payload (not (bytevector? payload)))
    (throw 'coap:encode:invalid-payload payload))

  (let ((code* (if (symbol? code) `(method ,code) code))
        (tl (cond (token (min-octets-for-uint token))
                  (else 0))))

    (unless (code? code*)
      (throw 'coap:encode:invalid-message-code code*))

    (unless (token-length? tl)
      (throw 'coap:encode:token-out-of-range token tl))

    (coap-encode version
                 (assq-ref coap-message-types type)
                 (apply resolve-code code*)
                 (or message-id 0)
                 tl
                 token
                 (encodable-options options)
                 (cond (payload (bytevector-length payload))
                       (else 0))
                 payload)))

(define (encode* spec)
  (define (use key default)
    (let ((result (assq key spec)))
      (if (not result)
          default
          (let ((value (cdr result)))
            (if (null? value)
                default
                value)))))
  (encode #:version    (use 'version    1)
          #:message-id (use 'message-id #f)
          #:token      (use 'token      #f)
          #:options    (use 'options    '())
          #:type       (use 'type       'non-confirmable)
          #:code       (use 'code       'empty)
          #:payload    (use 'payload    #f)))

(define (decode bv)
  (let ((len (bytevector-length bv)))
    (when (< len 4)
      (throw 'coap:decode:invalid-length len))
    (let-values (((toklen type version class detail message-id)
                  (apply values
                         (fetch bv 0
                                '(4 2 2)
                                '(3 5)
                                `(2 → ,(lambda (bv offset)
                                         (bytevector-u16-ref bv offset
                                                             'big)))))))
      (unless (= version 1)
        (throw 'coap:decode:invalid-version version))
      (unless (value-from? class coap-message-classes)
        (throw 'coap:decode:invalid-class class))
      (let* ((class-sym (reverse-lookup coap-message-classes class))
             (detail-table (class->detail-table class-sym))
             (token (and (positive? toklen)
                         (bytevector-uint-ref bv 4 'big toklen)))
             (offset+options (parse-options bv (+ 4 toklen)))
             (options (cdr offset+options))
             (payload (parse-payload bv (car offset+options))))
        (unless (value-from? detail detail-table)
          (throw 'coap:decode:invalid-class-details class-sym detail))
        `((version    . ,version)
          (type       . ,(reverse-lookup coap-message-types type))
          (code         ,class-sym ,(reverse-lookup detail-table detail))
          (message-id . ,message-id)
          (token      . ,(or token '()))
          (options    . ,options)
          (payload    . ,(or payload '())))))))
