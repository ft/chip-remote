;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote named-value)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote pretty-print)
  #:export (make-named-value
            define-value
            identify-value
            named-value?
            value-name
            value-data))

(define-immutable-record-type <named-value>
  (make-named-value name value)
  named-value?
  (name value-name identify-value)
  (value value-data))

(define (pp-named-value port indent named-value)
  (let ((pp (make-printer port indent))
        (cplx (make-printer/object port indent)))
    (pp-record port 'named-value
               (lambda ()
                 (pp 'name (value-name named-value))
                 (cplx 'value (value-data named-value))))))

(set-record-type-printer! <named-value>
  (lambda (rec port)
    (pp-named-value port (pp-indent) rec)))

(define-syntax-rule (define-value binding expr)
  (define binding (make-named-value 'binding expr)))
