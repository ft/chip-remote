;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (bitops)
  #:export (bit-extract-width
            clear-bits
            one-bits
            set-bits))

(define (one-bits width)
  (1- (ash 1 width)))

(define (clear-bits oldval width shifts)
  (logand (lognot (ash (one-bits width) shifts)) oldval))

(define (set-bits value bits width shift)
  (let ((rv (clear-bits value width shift)))
    (logior rv (ash bits shift))))

(define (bit-extract-width value start width)
  (bit-extract value start (+ start width)))
