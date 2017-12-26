;; Copyright (c) 2016 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote compiler transform)
  #:use-module (chip-remote compiler utilities)
  #:export (handle-non-unique-entries))

(define (handle-non-unique-entries kw state)
  (let loop ((rest (foreach-entry kw state
                                  (lambda (kw x)
                                    (list (syntax->datum (assq-ref x 'name)))))))
    (cond ((null? rest) state)
          ((member (car rest) (cdr rest))
           (throw 'non-unique-entry (car rest)))
          (else (loop (cdr rest))))))
