;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote semantics)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote interpreter)
  #:use-module (chip-remote named-value)
  #:export (make-semantics
            generate-semantics
            define-semantics
            semantics?
            semantics-name
            semantics-type
            semantics-data
            semantics-decode
            semantics-encode
            s:encode
            s:decode
            s:range))

(define-immutable-record-type <semantics>
  (make-semantics name type data range decode encode)
  semantics?
  (name semantics-name identify-semantics)
  (type semantics-type)
  (data semantics-data)
  (range semantics-range)
  (decode semantics-decode amend-decoder)
  (encode semantics-encode amend-encoder))

(define (default-range s w)
  ;; Possible return values of range generators:
  ;;   - #f: Don't make any stipulation upon semantics' input range.
  ;;   - (MIN . MAX) a pair of minimum/maximum values
  ;;   - (a b c ...) a set of allowed input values
  #f)

(define (lookup-range s w)
  (let ((tab (semantics-data s)))
    (map car (if (named-value? tab)
                 (value-data tab)
                 tab))))

;; With derived semantics, the new semantics sit between their parent and the
;; encoded value. This allows easier use of the interpreter language.
(define (call x)
  (cond ((evaluation? x) (evaluation-value x))
        (else x)))

(define (derived-encoder parent enc)
  (if parent
      (lambda (w v) ((call enc) w (s:encode parent w v)))
      enc))

(define (derived-decoder parent dec)
  (if parent
      (lambda (w v) (s:decode parent w ((call dec) w v)))
      dec))

(define (derived-range parent r)
  (if parent
      (semantics-range parent)
      r))

(define (eval-processor type proc)
  (case type
    ((interpreter) (make-evaluation proc))
    (else proc)))

(define* (gensem type #:key encode decode derive-from (range default-range))
  (unless encode (throw 'cr/required-keyword-argument #:encode))
  (unless decode (throw 'cr/required-keyword-argument #:decode))
  (make-semantics #f type #f (derived-range derive-from range)
                  (derived-decoder derive-from (eval-processor type decode))
                  (derived-encoder derive-from (eval-processor type encode))))

(define (decode-with-table table value)
  (let loop ((rest (if (named-value? table) (value-data table) table)))
    (if (null? rest)
        'chip-remote:undefined
        (let ((k (caar rest))
              (v (cdar rest)))
          (if (= value v)
              k
              (loop (cdr rest)))))))

(define (make-table-decoder table)
  (lambda (w x) (decode-with-table table x)))

(define (encode-with-table table key)
  (let ((value (assoc key (if (named-value? table)
                              (value-data table)
                              table))))
    (if value
        (cdr value)
        'chip-remote:undefined)))

(define (make-table-encoder table)
  (lambda (w x) (encode-with-table table x)))

(define* (gensem:lookup table #:key derive-from (range lookup-range))
  (make-semantics #f 'table-lookup table (derived-range derive-from range)
                  (derived-decoder derive-from (make-table-decoder table))
                  (derived-encoder derive-from (make-table-encoder table))))

(define-syntax generate-semantics
  (lambda (x)
    (syntax-case x (lookup interpreter scheme)
      ((_ interpreter exp ...) #'(gensem 'interpreter exp ...))
      ((_ scheme exp ...) #'(gensem 'scheme exp ...))
      ((_ lookup exp ...) #'(gensem:lookup exp ...))
      (_ (error "Invalid semantics generation")))))

(define-syntax-rule (define-semantics binding e* ...)
  (define binding (identify-semantics (generate-semantics e* ...)
                                      'binding)))

(define (codec a s w v)
  (let ((f (a s)))
    (cond ((procedure? f) (f w v))
          ((evaluation? f) ((evaluation-value f) w v))
          (else (throw 'cr/unknown-codec-type f)))))

(define (s:encode s w v)
  (codec semantics-encode s w v))

(define (s:decode s w v)
  (codec semantics-decode s w v))

(define (s:range s w)
  ((semantics-range s) s w))
