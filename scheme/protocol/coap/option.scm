;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (protocol coap option)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (protocol net-unicode)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote utilities)
  #:export (encodable-option
            encodable-options
            options-put
            option-size
            options-size
            parse-options))

(define* (make-range-check a #:optional b)
  (lambda (n)
    (if b
        (and (>= n a) (<= n b))
        (= a n))))

(define coap-message-options
  ;; This is from §5.10 of RFC7252. The ‘ns’ tag is for non sensical, since the
  ;; no-cache-key options only make sense with safe options.
  ;;
  ;; The opt:no-cache-key? function tests both columns.
  (let ((_ make-range-check)
        (undef (if #f #f)))
    ;; name            #  C  U  N  R fmt    length         default
    `((if-match        1 #t #f #f #t opaque ,(_ 0    8)    ,undef)
      (uri-host        3 #t #t ns #f string ,(_ 1  255)   the-uri)
      (etag            4 #f #f #f #t opaque ,(_ 1    8)    ,undef)
      (if-none-match   5 #t #f #f #f empty  ,(_ 0)         ,undef)
      (uri-port        7 #t #t ns #f uint   ,(_ 0    2)  the-port)
      (location-path   8 #f #f #f #t string ,(_ 0  255)    ,undef)
      (uri-path       11 #t #t ns #t string ,(_ 0  255)    ,undef)
      (content-format 12 #f #f #f #f uint   ,(_ 0    2)    ,undef)
      (max-age        14 #f #t ns #f uint   ,(_ 0    4)        60)
      (uri-query      15 #t #t ns #t string ,(_ 0  255)    ,undef)
      (accept         17 #t #f #f #f uint   ,(_ 0    2)    ,undef)
      (location-query 20 #f #f #f #t string ,(_ 0  255)    ,undef)
      (proxy-uri      35 #t #t ns #f string ,(_ 1 1034)    ,undef)
      (proxy-scheme   39 #t #t ns #f string ,(_ 1  255)    ,undef)
      (size1          60 #f #f #t #f uint   ,(_ 0    4)    ,undef))))

(define opt:name          first)
(define opt:number        second)
(define opt:critical?     third)
(define opt:unsafe?       fourth)
(define (opt:no-cache-key? o)
  (and (not (opt:unsafe? o))
       (fifth o)))
(define opt:repeatable?   sixth)
(define opt:format        seventh)
(define opt:length-ok?    eighth)
(define opt:default       ninth)

(define (opt:has-default? o)
  (not (equal? (if #f #f) (opt:default o))))

(define (opt:by-name name)
  (assq name coap-message-options))

(define (opt:by-number n)
  (let loop ((rest coap-message-options))
    (if (null? rest)
        #f
        (let ((this (car rest)))
          (if (= n (opt:number this))
              this
              (loop (cdr rest)))))))

(define (opt:name< a b)
  (< (opt:number (opt:by-name a))
     (opt:number (opt:by-name b))))

;; Options are encoded in an code-delta scheme, with the initial option code
;; being assumed to be zero. Options are encoded in increasing numeric value of
;; their respective codes, which ensures the delta to be zero or positive. The
;; idea is to get away with very few bits to encode a potentiall large number
;; of option codes. The basic format is this:
;;
;;   0   1   2   3   4   5   6   7
;; +---------------+---------------+
;; |  Option Delta | Option Length |   1 octet
;; +---------------+---------------+
;; |         Option Value          |   0 or more octets
;; +-------------------------------+
;;
;; However, both code-delta and value-length can be encoded in additional
;; octets to produce the full format, including the optional value extension
;; octets:
;;
;;   0   1   2   3   4   5   6   7
;; +---------------+---------------+
;; |  Option Delta | Option Length |   1 octet
;; +---------------+---------------+
;; |     extended Option Delta     |   0 to two octets
;; +-------------------------------+
;; |     extended Option Length    |   0 to two octets
;; +-------------------------------+
;; |         Option Value          |   0 or more octets
;; +-------------------------------+
;;
;; Delta extension happens in two cases: Values 13 and 14. A value of 15 is not
;; allowed and must result in a message format error. Extensional semantics:
;;
;;   13: Extension field is one octet wide and encodes the delta value minus 13.
;;   14: Extension field is two octets wide and encodes a delta value minus 269,
;;       which is 255+14.
;;
;; So to encode deltas that means:
;;
;; +----------+-------------------+----------------------+
;; |      Min |               Max | Format               |
;; |----------+-------------------+----------------------+
;; |        0 |                12 | Basic                |
;; |       13 |    255+13 =   268 | One Octet Extension  |
;; |      269 | 65535+269 = 65804 | Two Octet Extension  |
;; +----------+-------------------+----------------------+
;;
;; The extended Option Length field works exactly the same.

(define (value->extension-length kind n)
  (cond ((in-range n   0    12) 0)
        ((in-range n  13   268) 1)
        ((in-range n 269 65804) 2)
        (else (throw 'coap:option-header-value-out-of-range kind n))))

(define (encoded-value kind n)
  (let ((l (value->extension-length kind n)))
    (cons l (cond ((= l 1) (- n 13))
                  ((= l 2) (- n 269))
                  (else n)))))

(define (extension-length->basic-field l v)
  (cond ((= l 0)  v)
        ((= l 1) 13)
        (else    14)))

;; encodable-option returns a pair: (option-code encoder-list)
;; …where encoder-list is:
;;
;; (delta-ext-len delta-value length-ext-len length-value option-value)
;;
;; This enables the main encoder to pre-calculate the full header size and
;; encode the respective values in the correct positions. ‘option-value’ is #f
;; for options that don't carry a value.

(define (calculate-option-length opt value)
  (define (opt-type? fmt type pred value)
    (and (eq? type fmt)
         (pred value)))
  (let* ((fmt (opt:format opt))
         (empty? (eq? fmt 'empty)))
    (cond ((and empty? value)
           (throw 'coap:useless-option-value (opt:name opt) value))
          ((not (or empty? value))
           ;; TODO: Support default values? How? Some are easy (like max-age),
           ;;       but how to do with with uri and port?
           (throw 'coap:missing-option-value (opt:name opt)))
          ((and empty? (not value)) (cons 0 #f))
          ((opt-type? fmt 'uint non-negative-integer? value)
           (cons (min-octets-for-uint value) value))
          ((opt-type? fmt 'opaque bytevector? value)
           (cons (bytevector-length value) value))
          ((opt-type? fmt 'string string? value)
           (let ((new (string->net-unicode value)))
             (cons (bytevector-length new) new)))
          (else (throw 'coap:invalid-option-value (opt:name opt) value)))))

(define (handle-option prev-opt-id spec)
  (let* ((name (car spec))
         (value* (cdr spec))
         (value (if (null? value*) #f (car value*)))
         (opt (opt:by-name name))
         (num (opt:number opt))
         (delta (encoded-value 'delta (- num prev-opt-id)))
         (len+val (calculate-option-length opt value))
         (len (encoded-value 'length (car len+val))))
    (list num (car delta) (cdr delta) (car len) (cdr len) (cdr len+val))))

(define (encodable-option prev-opt-id opt)
  (match opt
    ((? symbol? name)       (handle-option prev-opt-id (list name)))
    (((? symbol? name) . _) (handle-option prev-opt-id opt))
    (_ (throw 'coap:invalid-option-spec opt))))

(define (opt-spec-symbol s)
  (if (list? s)
      (car s)
      s))

;; This returns a list-of-lists in accordance with encoder-list part of the
;; return values of each encode-option invocation.
(define (encodable-options lst)
  (map/carry encodable-option 0
             (sort lst (lambda (a b)
                         (opt:name< (opt-spec-symbol a)
                                    (opt-spec-symbol b))))))

(define (real-size lx len)
  (cond ((= lx 0) len)
        ((= lx 1) (+ len 13))
        (else (+ len 269))))

(define (option-size o)
  (match o
    ((dx delta lx len value)
     (+ 1 dx lx (real-size lx len)))))

(define (options-size opts)
  (apply + (map option-size opts)))

(define (option-put-value hdr offset len value)
  (cond ((bytevector? value)
         (bytevector-copy! value 0 hdr offset (bytevector-length value)))
        ((non-negative-integer? value)
         (bytevector-uint-set! hdr offset value 'big len))
        (else (throw 'coap:unsupported-option-value-type value))))

(define (option-put hdr offset option)
  (let* ((dx (first option))
         (delta* (second option))
         (delta (extension-length->basic-field dx delta*))
         (lx (third option))
         (length* (fourth option))
         (length (extension-length->basic-field lx length*)))
    (bytevector-u8-set! hdr offset
                        (chain (put 0 length)
                               (put 4 delta)))
    (when (positive? dx)
      (bytevector-uint-set! hdr (1+ offset) delta* 'big dx))
    (when (positive? lx)
      (bytevector-uint-set! hdr (+ 1 offset dx) length* 'big lx))
    (option-put-value hdr (+ 1 offset dx lx)
                      (real-size lx length*)
                      (fifth option)))
  (+ offset (option-size option)))

(define (options-put hdr offset options)
  (fold (lambda (option offset)
          (option-put hdr offset option))
        offset options))

(define-immutable-record-type <parse-state>
  (make-parse-state* bv offset last-option accumulator)
  parse-state?
  (bv          parse:bv)
  (offset      parse:offset      parse:new:offset)
  (last-option parse:last-option parse:new:last-option)
  (accumulator parse:accumulator parse:new:accumulator))

(define (make-parse-state bv offset)
  (make-parse-state* bv offset 0 '()))

(define (parse-next state)
  (bytevector-u8-ref (parse:bv state)
                     (parse:offset state)))

(define (parse-end? state)
  (>= (parse:offset state)
      (bytevector-length (parse:bv state))))

(define (parse-payload? state)
  (= #xff (parse-next state)))

(define (parse-new-offset state mod)
  (parse:new:offset state (mod (parse:offset state))))

(define (parse-last-option state delta)
  (parse:new:last-option state (+ delta (parse:last-option state))))

(define (parse-add-option state opt)
  (parse:new:accumulator state (append (parse:accumulator state) (list opt))))

(define (parse-basic-delta octet)
  (bit-extract-width octet 4 4))

(define (parse-basic-size octet)
  (bit-extract-width octet 0 4))

(define (basic->extension n)
  (cond ((= n 13) 1)
        ((= n 14) 2)
        (else 0)))

(define (decoded-value m n)
  (cond ((= m 1) (+ n 13))
        ((= m 2) (+ n 269))
        (else n)))

(define (parse-size-field state basic-size d s)
  (let ((size (if (zero? s)
                  basic-size
                  (decoded-value
                   s (bytevector-uint-ref (parse:bv state)
                                          (+ 1 d (parse:offset state))
                                          'big s))))
        (n (bytevector-length (parse:bv state)))
        (o (parse:offset state)))
    (when (> (+ 1 d s size)
             (- n o))
      (throw 'coap:decode:option-too-large size o n (parse:bv state)))
    size))

(define (extract-option state d s p)
  (let* ((delta (if (zero? d)
                    (parse-basic-delta (parse-next state))
                    (decoded-value
                     d (bytevector-uint-ref (parse:bv state)
                                            (+ 1 (parse:offset state))
                                            'big d))))
         (id (+ (parse:last-option state) delta))
         (opt-spec (opt:by-number id))
         (p-offset (+ 1 d s (parse:offset state))))
    (when (not ((opt:length-ok? opt-spec) p))
      (throw 'coap:decode:invalid-option-payload-size p opt-spec))
    (values id (list (car opt-spec)
                     (case (opt:format opt-spec)
                       ((string) (net-unicode->string (parse:bv state)
                                                      #:offset p-offset
                                                      #:length p))
                       ((uint) (bytevector-uint-ref (parse:bv state)
                                                    p-offset 'big p))
                       (else (let ((new (make-bytevector p)))
                               (bytevector-copy! (parse:bv state)
                                                 p-offset new 0 p)
                               new)))))))

(define (parse-option-size state)
  (let ((basic-size (parse-basic-size (parse-next state)))
        (basic-delta (parse-basic-delta (parse-next state))))
    (when (= basic-size 15)
      (throw 'coap:decode:invalid-basic-option-size
             basic-size
             (parse-next state)
             (parse:offset state)
             (parse:bv state)))
    (when (= basic-delta 15)
      (throw 'coap:decode:invalid-basic-option-delta
             basic-delta
             (parse-next state)
             (parse:offset state)
             (parse:bv state)))
    (let* ((d (basic->extension basic-delta))
           (s (basic->extension basic-size))
           (p (parse-size-field state basic-size d s)))
      (values d s p (+ 1 d s p)))))

(define (parse-option state)
  (let*-values (((d s p size) (parse-option-size state))
                ((id opt) (extract-option state d s p)))
    (parse-new-offset (parse:new:last-option (parse-add-option state opt) id)
                      (lambda (n) (+ n size)))))

(define (parse state)
  (cond ((parse-end? state)     state)
        ((parse-payload? state) (parse-new-offset state 1+))
        (else                   (parse (parse-option state)))))

(define (parse-options bv offset)
  (let ((result (parse (make-parse-state bv offset))))
    (cons (parse:offset result)
          (parse:accumulator result))))
