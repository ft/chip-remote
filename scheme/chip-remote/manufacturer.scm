(define-module (chip-remote manufacturer)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote pretty-print)
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

(define (pp-manufacturer port indent man)
  (let ((pp (make-printer port indent)))
    (pp-record port 'manufacturer
               (lambda ()
                 (pp 'name (manufacturer-name man))
                 (pp 'homepage (manufacturer-homepage man))
                 (pp 'wikipedia (manufacturer-wikipedia man))))))

(set-record-type-printer! <manufacturer>
  (lambda (rec port)
    (pp-manufacturer port (pp-indent) rec)))

(define* (make-manufacturer #:key
                            (name #f)
                            (homepage #f)
                            (wikipedia #f))
  (make-manufacturer* name homepage wikipedia))

(define-syntax-rule (define-manufacturer binding e0 e* ...)
  (define binding (make-manufacturer e0 e* ...)))
