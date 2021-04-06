;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test validate items-no-holes-and-overlap)
  #:use-module (srfi srfi-1)
  #:use-module (test tap)
  #:use-module (chip-remote device)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote item)
  #:use-module (chip-remote utilities)
  #:export (make-alignment-test
            define-alignment-test
            items-alignment-count))

(define-syntax-rule (define-alignment-test name test str)
  (define (name a b)
    (let ((a-name (item-name a))
          (a-offset (item-offset a))
          (a-width (item-width a))
          (b-name (item-name b))
          (b-offset (item-offset b))
          (b-width (item-width b)))
      (define-test (fmt "Items a(~a, o:~a, w:~a) b(~a, o:~a, w:~a) don't~a"
                        a-name a-offset a-width b-name b-offset b-width str)
        (test (+ a-offset a-width)
                    b-offset)))))

(define (make-alignment-test fnc)
  (lambda (reg)
    (let ((items (sorted-items reg)))
      (let loop ((rest items))
        (when (> (length rest) 1)
          (let ((focus (take rest 2)))
            (fnc (first focus) (second focus)))
          (loop (cdr rest)))))))

(define (items-alignment-count dev cfg)
  (apply + (map 1-
                (filter (lambda (x) (> x 0))
                        (map length (map register-items
                                         (device-registers dev)))))))
