;; Copyright (c) 2024 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (data-structures records utilities)
  #:export (need new-record-definer))

(define (need exception predicate?)
  (lambda (value)
    (unless (predicate? value)
      (throw 'invalid-field-value exception value))
    value))

(define-syntax new-record-definer
  (lambda (s)
    (with-ellipsis :::
      (syntax-case s ()
        ((_ definer backend)
         #'(define-syntax definer
             ;; (inherit ...) must be the first key value pair passed into the
             ;; record, so we need to reorder, when it's there.
             (lambda (s)
               (syntax-case s (inherit)
                 ((_ binding (inherit orig-record) e* ...)
                  #'(define binding
                      (backend (inherit orig-record) (name 'binding) e* ...)))
                 ((_ binding e* ...)
                  #'(define binding (backend (name 'binding) e* ...)))))))))))
