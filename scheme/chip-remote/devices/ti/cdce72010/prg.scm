;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti cdce72010 prg)
  #:use-module (bitops)
  #:use-module (chip-remote devices ti cdce72010 tables)
  #:export (clear-device-power-down-bit
           clear-odiv-enable-bit
           clear-pll-power-down-bit
           set-bits-fbdiv
           set-bits-mdiv
           set-bits-ndiv
           set-bits-odiv
           set-bits-output-mode
           set-bits-rdiv
           set-device-power-down-bit
           set-odiv-enable-bit
           set-pll-power-down-bit))


(define (set-bits-odiv regval divval)
  (set-bits regval (get-bits-for-divider divval) 7 17))

(define (set-bits-fbdiv regval divval)
  (set-bits regval (get-bits-for-divider divval) 7 9))

;; The M and N dividers are way simpler to set than the fb/output ones. You
;; just need to substract 1 off of the `divval' value and put the resulting
;; bits into the right position.
(define (set-bits-mdiv regval divval)
  (set-bits regval (1- divval) 14 4))

(define (set-bits-ndiv regval divval)
  (set-bits regval (1- divval) 14 18))

(define (set-bits-output-mode regval mode)
  (set-bits regval (get-bits-for-output-mode mode) 7 25))

(define (set-odiv-enable-bit regval)
  (logior regval (ash 1 24)))

(define (clear-odiv-enable-bit regval)
  (clear-bits regval 1 24))

(define (set-pll-power-down-bit regval)
  (logior regval (ash 1 23)))

(define (clear-pll-power-down-bit regval)
  (clear-bits regval 1 23))

(define (set-device-power-down-bit regval)
  (logior regval (ash 1 11)))

(define (clear-device-power-down-bit regval)
  (clear-bits regval 1 11))

(define (set-bits-rdiv regval type state)
  (let ((bitnum (cond
                 ((equal? type 'primary) 4)
                 ((equal? type 'secondary) 5))))
    (cond (state
           (logior regval (ash 1 bitnum)))
          (else
           (clear-bits regval 1 bitnum)))))
