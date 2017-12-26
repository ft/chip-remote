(define-module (chip-remote manufacturer)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 optargs)
  #:export (make-manufacturer
            manufacturer?
            manufacturer-name
            manufacturer-homepage
            manufacturer-wikipedia
            define-manufacturer))

(define-record-type <manufacturer>
  (make-manufacturer* name homepage wikipedia)
  manufacturer?
  (name manufacturer-name)
  (homepage manufacturer-homepage)
  (wikipedia manufacturer-wikipedia))

(define* (make-manufacturer #:key
                            (name #f)
                            (homepage #f)
                            (wikipedia #f))
  (make-manufacturer* name homepage wikipedia))

(define-syntax-rule (define-manufacturer binding e0 e* ...)
  (define binding (make-manufacturer e0 e* ...)))
