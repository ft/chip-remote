;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices analog-devices ad9262 registers)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote item builder)
  #:use-module (chip-remote register)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote devices analog-devices ad9262 tables)
  #:export (reg:spi-port-cfg
            reg:chip-id
            reg:chip-grade
            reg:channel-index
            reg:power-modes
            reg:pll-enable
            reg:pll
            reg:analog-input
            reg:output-modes
            reg:output-adjust
            reg:output-clock
            reg:reference
            reg:output-data
            reg:overrange
            reg:quad-error-correction-1
            reg:quad-error-correction-2))

(define-register reg:spi-port-cfg
  ;; Interesting register, that maintains its semantics, irregardless of
  ;; previously configured spi port mode, due to its symmetry.
  #:contents
  (=> (reserved 0 1))
  (lsb-first-a? 1 1)
  (soft-reset-a! 2 1)
  (=> (reserved 3 2))
  (soft-reset-b! 5 1)
  (lsb-first-b? 6 1)
  (=> (reserved 7 1)))

(define-register reg:chip-id #:contents (chip-id 0 8))

(define-register reg:chip-grade
  #:contents
  (=> (reserved 0 4))
  (chip-grade 4 2 #:semantics lookup chip-grade-map)
  (=> (reserved 6 2)))

(define-register reg:channel-index
  #:contents
  (channel-index 0 2 #:semantics lookup channel-index-map)
  (=> (reserved 2 6)))

(define-register reg:power-modes
  #:contents
  (power-down 0 2 #:semantics lookup power-down-map)
  (=> (reserved 2 6)))

(define-register reg:pll-enable
  #:contents
  (=> (reserved 0 2))
  (pll-enable? 2 1)
  (=> (reserved 3 5)))

(define-register reg:pll
  #:contents
  (pll-multiplier 0 6 #:semantics lookup pll-mult-map)
  (pll-autoband? 6 1)
  (pll-locked? 7 1))

(define-register reg:analog-input
  #:contents
  (=> (reserved 0 5))
  (bandwidth 5 2 #:semantics lookup bw-map)
  (=> (reserved 7 1)))

(define-register reg:output-modes
  #:contents
  (output-format 0 2 #:semantics lookup output-format-map)
  (output-invert? 2 1)
  (=> (reserved 3 1))
  (output-tristate? 4 1)
  (output-interleave? 5 1)
  (=> (reserved 6 1))
  (output-driver 7 1 #:semantics lookup driver-map))

(define-register reg:output-adjust
  #:contents
  (drive-strength-18 0 2 #:semantics lookup driver-18-map)
  (drive-strength-33 2 2 #:semantics lookup driver-33-map)
  (=> (reserved 4 4)))

(define-register reg:output-clock
  #:contents
  (=> (reserved 0 7))
  (invert-dco? 7 1))

(define-register reg:reference
  #:contents
  (=> (reserved 0 6))
  (use-external-reference? 6 1)
  (=> (reserved 7 1)))

(define-register reg:output-data
  #:contents
  (output-data-rate 0 6 #:semantics lookup data-rate-map)
  (enable-quad-error-correction? 6 1)
  (=> (reserved 7 1)))

(define-register reg:overrange
  #:contents
  (overrange-threshold 0 6 #:semantics lookup threshold-map)
  (indicator-or-condition? 6 1)
  (auto-reset? 7 1))

(define-register reg:quad-error-correction-1
  #:contents
  (disable-gain-correction? 0 1)
  (disable-phase-correction? 1 1)
  (disable-dc-correction? 2 1)
  (freeze-gain-correction? 3 1)
  (freeze-phase-correction? 4 1)
  (freeze-dc-correction? 5 1)
  (=> (reserved 6 2)))

(define-register reg:quad-error-correction-2
  #:contents
  (force-gain-correction-initial? 0 1)
  (force-phase-correction-initial? 1 1)
  (force-dc-correction-initial? 2 1)
  (=> (reserved 3 5)))
