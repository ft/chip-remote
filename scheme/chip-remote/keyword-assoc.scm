;; Copyright (c) 2014-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote keyword-assoc)
  #:export (kwa-ref))

(define (kwa-ref kw-lst kw)
  (let ((m (memq kw kw-lst)))
    (if (not m)
        (throw 'cr-missing-keyword-entry kw-lst kw)
        (cadr m))))
