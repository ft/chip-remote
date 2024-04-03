;; Copyright (c) 2017-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote semantics)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote interpreter)
  #:use-module (chip-remote named-value)
  #:use-module (chip-remote utilities)
  #:use-module (data-structures records)
  #:use-module (data-structures records utilities)
  #:export (semantics
            make-semantics
            define-semantics
            semantics?
            semantics-name
            semantics-default
            semantics-default*
            semantics-range
            semantics-range*
            semantics-decoder
            semantics-encoder
            semantics-allow
            semantics-compose
            semantics-decode
            semantics-encode
            semantics-in-range?
            table-lookup
            tbl
            static))

(define-record-type* <semantics>
  semantics make-semantics semantics? this-semantics
  (name   semantics-name (default 'none) (sanitize (need 'name symbol?)))
  ;; This is a function that takes a semantics datum and an item width and
  ;; produces an encoding of the range of values that are valid for the im-
  ;; plemented semantics.
  ;;
  ;; The value returned is one of:
  ;;
  ;;  (none)
  ;;  (range <minimum> <maximum>)
  ;;  (enumeration <list-of-values>)
  ;;  (table <association-list/named-value>)
  ;;
  ;; This is consumed by semantics-in-range?.
  (range   semantics-range* (default (lambda (s w) '(none))))
  (default semantics-default*)
  ;; The default decoder/encoder pair is able to look at range, and if that
  ;; looks like a table lookup, use that table to the their work. That way, you
  ;; don't have to specify these functions for table lookup semantics.
  (decode  semantics-decoder (thunked)
           (default (lambda (w v)
                     (default-decode this-semantics w v))))
  (encode  semantics-encoder (thunked)
           (default (lambda (w v)
                     (default-encode this-semantics w v))))
  ;; Allow users to specify a function that allows values into the encoding
  ;; function. This is used in semantics-encode.
  (allow   semantics-allow (default (lambda (w v) #t))))

(new-record-definer define-semantics semantics)

(define (table-lookup table)
  (lambda (s w) (list 'table table)))

(define* (tbl t #:key default without)
  (semantics (range (table-lookup t))
             (allow (if without
                        (lambda (w v) (not (member v without)))
                        (lambda (w v) #t)))
             (default (if default
                          (static default)
                          (static (caar (if (named-value? t)
                                            (value-data t)
                                            t)))))))

(define (static value)
  (lambda (s w) value))

(define (decode-with-table table value)
  (let loop ((rest (if (named-value? table) (value-data table) table)))
    (if (null? rest)
        'chip-remote:undefined
        (let ((k (caar rest))
              (v (cdar rest)))
          (if (= value v)
              k
              (loop (cdr rest)))))))

(define (encode-with-table table key)
  (let ((value (assoc key (if (named-value? table)
                              (value-data table)
                              table))))
    (if value
        (cdr value)
        (throw 'cr/undefined key table))))

(define (codec a s w v)
  (let ((f (a s)))
    (cond ((procedure? f) (f w v))
         ((evaluation? f) ((evaluation-value f) w v))
          (else (throw 'cr/unknown-semantics-type f)))))

(define (semantics-encode s w v)
  (unless ((semantics-allow s) w v)
    (throw 'invalid-semantics-encoder-input v w s))
  (codec semantics-encoder s w v))

(define (semantics-decode s w v)
  (codec semantics-decoder s w v))

(define (semantics-range s w)
  ((semantics-range* s) s w))

(define (semantics-default s w)
  ((semantics-default* s) s w))

(define (semantics-in-range? s w v)
  (match (semantics-range s w)
    (('none) #t)
    (('range minimum maximum)
     (and (>= v minimum)
          (<= v maximum)))
    (('table table)
     (!! (memq v (map car
                      (if (named-value? table)
                          (value-data table)
                          table)))))
    (('enumeration . lst) (!! (memq v lst)))
    (_ #f)))

(define (semantics-compose a b)
  (lambda (w x)
    (a w (b w x))))

(define (default-codec f s w x)
  (let ((range (semantics-range s w)))
    (match range
      (('table table) (f table x))
      (_ x))))

(define (default-encode s w x)
  (default-codec encode-with-table s w x))

(define (default-decode s w x)
  (default-codec decode-with-table s w x))
