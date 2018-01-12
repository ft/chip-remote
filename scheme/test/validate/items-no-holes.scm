;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test validate items-no-holes)
  #:use-module (srfi srfi-1)
  #:use-module (test tap)
  #:use-module (test validate items-no-holes-and-overlap)
  #:use-module (chip-remote device)
  #:export (items-no-holes/count
            items-no-holes/check))

(define items-no-holes/count items-alignment-count)
(define-alignment-test test-holes pass-if->=)
(define check-reg (make-alignment-test test-holes))
(define (items-no-holes/check dev cfg)
  (for-each check-reg (device-registers dev)))
