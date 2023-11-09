;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test validate items-no-holes)
  #:use-module (srfi srfi-1)
  #:use-module (test tap)
  #:use-module (test validate items-no-holes-and-overlap)
  #:use-module (chip-remote device)
  #:use-module (chip-remote utilities)
  #:export (items-no-holes/count
            items-no-holes/check))

(define (items-no-holes/count dev cfg)
  (let ((skip? (kwa-ref cfg #:allow-holes? #f)))
    (if skip? 0 (items-alignment-count dev cfg))))
(define-alignment-test test-holes pass-if->= " don't form holes")
(define check-reg (make-alignment-test test-holes))
(define (items-no-holes/check dev cfg)
  (let ((skip? (kwa-ref cfg #:allow-holes? #f)))
    (if skip? #t (for-each check-reg (device-registers dev)))))
