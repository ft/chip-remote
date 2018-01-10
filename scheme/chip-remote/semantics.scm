;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote semantics)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote interpreter)
  #:export (make-semantics
            generate-semantics
            define-semantics
            semantics?
            semantics-type
            semantics-data
            semantics-decode
            semantics-encode
            deduce-semantics))

(define-immutable-record-type <semantics>
  (make-semantics name type data decode encode)
  semantics?
  (name semantics-name identify-semantics)
  (type semantics-type)
  (data semantics-data)
  (decode semantics-decode amend-decoder)
  (encode semantics-encode amend-encoder))

(define (deduce-semantics width meta semantics)
  (match semantics
    (#f (if (= width 1)
            (deduce-semantics width meta *boolean-fallback*)
            (deduce-semantics width meta *unsigned-integer-fallback*)))
    ((? procedure? semantics) (semantics width))
    ((? semantics? semantics) semantics)
    (_ (throw 'cannot-deduce-semantics width meta semantics))))

(define-syntax generate-semantics
  (lambda (x)

    (define (sym->codec kw table accessor sym)
      (datum->syntax
       kw `(@@ (chip-remote codecs)
               ,(accessor (assq-ref table (syntax->datum sym))))))

    (define simple-table
      '((boolean decode-boolean encode-boolean)
        (boolean/active-low decode-boolean/active-low encode-boolean/active-low)))

    (define (simple-type? syn)
      (memq (syntax->datum syn) (map car simple-table)))

    (define (simple-decoder kw type)
      (sym->codec kw simple-table car type))

    (define (simple-encoder kw type)
      (sym->codec kw simple-table cadr type))

    (define width-table
      '((unsigned-integer decode-unsigned-integer encode-unsigned-integer)
        (ones-complement decode-ones-complement encode-ones-complement)
        (twos-complement decode-twos-complement encode-twos-complement)
        (offset-binary decode-offset-binary encode-offset-binary)
        (signed-magnitude decode-signed-magnitude encode-signed-magnitude)))

    (define (width-type? syn)
      (memq (syntax->datum syn) (map car width-table)))

    (define (width-decoder kw type)
      (sym->codec kw width-table car type))

    (define (width-encoder kw type)
      (sym->codec kw width-table cadr type))

    (syntax-case x (lookup interpreter scheme)
      ((kw type)
       (simple-type? #'type)
       #`(make-semantics #f 'type #f
                         #,(simple-decoder #'kw #'type)
                         #,(simple-encoder #'kw #'type)))
      ((kw type)
       (width-type? #'type)
       #`(lambda (w)
           (let* ((sem (make-semantics #f 'type #f
                                       #,(width-decoder #'kw #'type)
                                       #,(width-encoder #'kw #'type)))
                  (dec (semantics-decode sem))
                  (enc (semantics-encode sem)))
             (amend-decoder (amend-encoder sem
                                           (lambda (v) (enc w v)))
                            (lambda (v) (dec w v))))))
      ((kw lookup table)
       #'(make-semantics #f 'table-lookup table
                         (make-table-decoder table)
                         (make-table-encoder table)))
      ((kw interpreter #:encode e #:decode d)
       #'(make-semantics #f 'interpreter #f
                         (make-evaluation d)
                         (make-evaluation e)))
      ((kw interpreter #:decode d #:encode e)
       #'(generate-semantics interpreter #:encode e #:decode d))
      ((kw scheme #:encode e #:decode d)
       #'(make-semantics #f 'scheme #f
                         (make-evaluation d)
                         (make-evaluation e)))
      ((kw scheme #:decode d #:encode e)
       #'(generate-semantics scheme #:encode e #:decode d)))))

(define-syntax-rule (define-semantics binding e* ...)
  (define binding (identify-semantics (generate-semantics e* ...)
                                      'binding)))

(define *boolean-fallback* (generate-semantics boolean))
(define *unsigned-integer-fallback* (generate-semantics unsigned-integer))
