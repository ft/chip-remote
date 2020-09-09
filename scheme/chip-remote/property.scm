;; Copyright (c) 2020 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote property)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote item)
  #:export (make-property
            make-property*
            property?
            property-item
            property-value
            change-property-value))

(define-immutable-record-type <property>
  (make-property* item value)
  property?
  (item property-item)
  (value property-value change-property-value))

(define* (make-property item #:optional (value 0))
  (unless (item? item)
    (throw 'cr/expected-item item))
  (unless (and (integer? value)
               (>= value 0))
    (throw 'cr/expected-unsigned-integer value))
  (make-property* item value))
