;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (protocol coap message)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (chip-remote utilities)
  #:export (class->detail-table
            coap-message-types
            coap-message-classes
            code?
            message-type?
            parse-payload
            resolve-code
            reverse-lookup
            token-length?
            value-from?))

(define (reverse-lookup table value)
  (cond ((null? table) #f)
        ((= value (cdar table)) (caar table))
        (else (reverse-lookup (cdr table) value))))

(define coap-message-types
  '((confirmable     . 0)
    (non-confirmable . 1)
    (acknowledgement . 2)
    (reset           . 3)))

(define coap-message-classes
  '((method       . 0)
    (success      . 2)
    (error:client . 4)
    (error:server . 5)
    (signal       . 7)))

(define coap-method-types
  '((empty            . 0)
    (get              . 1)
    (post             . 2)
    (put              . 3)
    (delete           . 4)
    (fetch            . 5)
    (patch            . 6)
    (patch:idempotent . 7)))

(define coap-success-types
  '((created  . 1)
    (deleted  . 2)
    (valid    . 3)
    (changed  . 4)
    (content  . 5)
    (continue . 31)))

(define coap-client-error-types
  '((bad-request                .  0)
    (unauthorized               .  1)
    (bad-option                 .  2)
    (forbidden                  .  3)
    (not-found                  .  4)
    (method-not-allowed         .  5)
    (not-acceptable             .  6)
    (request-entity-incomplete  .  8)
    (conflict                   .  9)
    (precondition-failed        . 12)
    (request-entity-too-large   . 13)
    (unsupported-content-format . 15)))

(define coap-server-error-types
  '((internal-server-error  . 0)
    (not-implemented        . 1)
    (bad-gateway            . 2)
    (service-unavailable    . 3)
    (gateway-timeout        . 4)
    (proxying-not-supported . 5)))

(define coap-signal-types
  '((unassigned . 0)
    (csm        . 1)
    (ping       . 2)
    (pong       . 3)
    (release    . 4)
    (abort      . 5)))

(define (element-from? f k lst)
  (!! (member k (map f lst))))

(define (keyword-from? k lst)
  (element-from? car k lst))

(define (value-from? k lst)
  (element-from? cdr k lst))

(define (message-type? x)
  (keyword-from? x coap-message-types))

(define (message-class? x)
  (keyword-from? x coap-message-classes))

(define (method? x)
  (keyword-from? x coap-method-types))

(define (success? x)
 (keyword-from? x coap-success-types))

(define (client-error? x)
 (keyword-from? x coap-client-error-types))

(define (server-error? x)
 (keyword-from? x coap-server-error-types))

(define (signal-code? x)
 (keyword-from? x coap-signal-types))

(define class-details
  `((method       ,method?       ,coap-method-types)
    (success      ,success?      ,coap-success-types)
    (error:client ,client-error? ,coap-client-error-types)
    (error:server ,server-error? ,coap-server-error-types)
    (signal       ,signal-code?  ,coap-signal-types)))

(define (token-length? n)
  (and (>= n 0)
       (<= n 8)))

(define (message-id? n)
  (and (>= n 0)
       (<= n (uint-max n))))

(define (code? obj)
  (and (list? obj)
       (= (length obj) 2)))

(define (resolve-code class detail)
  (unless (message-class? class)
    (throw 'coap:encode:invalid-message-class class))
  (let ((dinfo (assq-ref class-details class)))
    (unless ((car dinfo) detail)
      (throw 'coap:encode:invalid-message-class-detail class detail))
    (cons (assq-ref coap-message-classes class)
          (assq-ref (cadr dinfo) detail))))

(define (class->detail-table class)
  (case class
    ((method)       coap-method-types)
    ((success)      coap-success-types)
    ((error:client) coap-client-error-types)
    ((error:server) coap-server-error-types)
    ((signal)       coap-signal-types)))

(define (parse-payload bv offset)
  (if (>= offset (bytevector-length bv))
      #f
      (let ((new (make-bytevector (- (bytevector-length bv) offset))))
        (bytevector-copy! bv offset new 0 (bytevector-length new))
        new)))
