;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti cdce72010 validate)
  :export (divider?
           mn-divider-value?
           output-index?
           r-divider?
           register-index?))

(define (divider? idx)
  (and (> idx 0)
       (< idx 9)))

(define (mn-divider-value? value)
  (and (> value 0)
       ;; max. value is 14 bits set to `1' plus 1.
       (<= value #b100000000000000)))

(define (output-index? idx)
  (and (>= idx 0)
       (<= idx 9)))

(define (r-divider? type)
  (and (symbol? type)
       (or (equal? type 'primary)
           (equal? type 'secondary))))

(define (register-index? idx)
  (and (>= idx 0)
       (<= idx 12)))
