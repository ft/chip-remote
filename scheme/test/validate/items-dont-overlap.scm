;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test validate items-dont-overlap)
  #:use-module (srfi srfi-1)
  #:use-module (test tap)
  #:use-module (chip-remote device)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote item)
  #:use-module (chip-remote utilities)
  #:export (items-dont-overlap/count
            items-dont-overlap/check))

(define (test-overlap a b)
  (let ((a-name (item-name a))
        (a-offset (item-offset a))
        (a-width (item-width a))
        (b-name (item-name b))
        (b-offset (item-offset b))
        (b-width (item-width b)))
    (define-test (fmt "Items a(~a, o:~a, w:~a) b(~a, o:~a, w:~a) don't overlap"
                      a-name a-offset a-width b-name b-offset b-width)
      (pass-if-<= (+ a-offset a-width)
                  b-offset))))

(define (check-reg reg)
  (let ((items (sorted-items reg)))
    (let loop ((rest items))
      (when (> (length rest) 1)
        (let ((focus (take rest 2)))
          (test-overlap (first focus) (second focus)))
        (loop (cdr rest))))))

(define (items-dont-overlap/check dev cfg)
  (for-each check-reg (device-registers dev)))

(define (items-dont-overlap/count dev cfg)
  (apply + (map 1- (map length (map register-items (device-registers dev))))))
