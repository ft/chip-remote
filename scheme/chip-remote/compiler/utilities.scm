;; Copyright (c) 2015 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote compiler utilities)
  #:use-module (srfi srfi-1)
  #:export (foreach-entry))

(define (foreach-entry kw state cb)
  (let loop ((rest (concatenate
                    (map (lambda (x) (assq-ref (cdr x) 'contents))
                         (assq-ref state 'registers))))
             (acc '()))
    (cond ((null? rest) acc)
          (else (loop (cdr rest)
                      (concatenate (list acc (cb kw (car rest)))))))))
