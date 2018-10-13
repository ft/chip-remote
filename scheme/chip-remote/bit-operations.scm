;; Copyright (c) 2011-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote bit-operations)
  #:export (bit-extract-width
            clear-bits
            one-bits
            set-bits
            logclear))

(define (one-bits width)
  "Returns an integer, that has its lower ‘width’ bits set:

    (one-bits 5) => 31 (binary: #b11111)"
  (1- (ash 1 width)))

(define (clear-bits oldval width shifts)
  "Clears ‘width’ bits in ‘oldval’ with the lowest cleared bit being
‘shifts’ (indexing starts at 0):

    (clear-bits #b11111111 3 0) => #b11111000
    (clear-bits #b11111111 3 3) => #b11000111
    (clear-bits #b11111111 3 6) => #b00111111"
  (logand (lognot (ash (one-bits width) shifts)) oldval))

(define (set-bits value bits width shift)
  "Returns a new value based on ‘value’ with a block of ‘width’ bits set to
‘bits’ with the lowest bit being at index ‘shifts’ (indexing starts at 0):

    (set-bits #b11111111 #b0110 4 0)) => #b11110110
    (set-bits #b11111111 #b0110 4 2)) => #b11011011"
  (let ((rv (clear-bits value width shift)))
    (logior rv (ash bits shift))))

(define (bit-extract-width value start width)
  "Like ‘bit-extract’ but instead of start and end, the block of bits to
extract from ‘value’ is addressed by ‘start’ and ‘width’:

    (bit-extract      #b01101010 3 7) => #b1101
    (bit-extract-with #b01101010 3 4) => #b1101"
  (bit-extract value start (+ start width)))

(define (logclear value bits)
  "Clear ‘bits’ in ‘value’.

    (logclear #xff #b11000011) => #b111100"
  (logand (lognot bits) value))
