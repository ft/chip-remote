;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote named-value)
  #:use-module (srfi srfi-9 gnu)
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

(define-syntax-rule (define-value binding expr)
  (define binding (make-named-value 'binding expr)))
