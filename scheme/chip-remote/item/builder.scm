;; Copyright (c) 2017-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote item builder)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote item)
  #:use-module (chip-remote item access)
  #:export (make-address reserved reserved*))

(define* (make-address #:key (access (ro)) (offset 0) (width #f))
  (unless width (throw 'missing-argument 'width))
  (unless (and (integer? width)
               (>= width 0))
    (throw 'invalid-width width))
  (lambda (addr)
    (item (name 'address)
          (offset offset)
          (width width)
          (access access)
          (default addr))))

(define (res o w d)
  (item (name 'reserved)
        (offset o)
        (width w)
        (access ro)
        (default d)))

(define* (reserved offset width #:key (default 0))
  (res offset width default))

(define* (reserved* #:key (default 0) offset width)
  (res offset width default))
