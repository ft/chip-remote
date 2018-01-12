;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test validate items-dont-overlap)
  #:use-module (srfi srfi-1)
  #:use-module (test tap)
  #:use-module (test validate items-no-holes-and-overlap)
  #:use-module (chip-remote device)
  #:export (items-dont-overlap/count
            items-dont-overlap/check))

(define items-dont-overlap/count items-alignment-count)
(define-alignment-test test-overlap pass-if-<= " don't overlap")
(define check-reg (make-alignment-test test-overlap))
(define (items-dont-overlap/check dev cfg)
  (for-each check-reg (device-registers dev)))
