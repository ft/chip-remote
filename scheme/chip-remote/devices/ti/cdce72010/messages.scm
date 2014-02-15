;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti cdce72010 messages)
  #:export (error-divider
            error-invalid-r-divider
            error-invalid-reg-index
            error-mn-divider-value
            error-not-boolean
            error-output-index))

(define (error-divider x)
  (display "`divider' needs to be an integer 1..8.\n")
  (display (format #f "Was: ~d\n" x)))

(define (error-mn-divider-value value)
  (display (format #f "The M and N dividers need to be set from 1 to ~d.\n"
                   (#b100000000000000)))
  (display (format #f "You tried to set: ~d\n" value)))

(define (error-output-index)
  (display "Output index out of range (0..9)\n"))

(define (error-invalid-r-divider)
  (display "r-divider type needs to be either 'primary or 'secondary.\n"))

(define (error-invalid-reg-index idx)
  (format #t "Register index (~d) out of range. [0..12]\n" idx))

(define (error-not-boolean what)
  (format #t "~a needs to be a boolean value.\n" what))
