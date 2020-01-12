;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote named-value)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote pretty-print)
  #:export (make-named-value
            define-value
            pp-named-value
            identify-value
            named-value?
            value-name
            value-data))

(define-immutable-record-type <named-value>
  (make-named-value name value)
  named-value?
  (name value-name identify-value)
  (value value-data))

(define (pp-named-value named-value)
  `(wrap "#<" ">"
         (type named-value) (newline)
         (indent complex
                 (key name) (space ,(value-name named-value)) (newline)
                 (key value) (space ,(value-data named-value)))))

(set-record-type-printer! <named-value>
  (lambda (named-value port)
    (pp-eval port (pp-named-value named-value))))

(define-syntax-rule (define-value binding expr)
  (define binding (make-named-value 'binding expr)))
