;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices analog-devices ad9262 registers)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote item)
  #:use-module (chip-remote item builder)
  #:use-module (chip-remote named-value)
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
  (items
   (list (reserved        0 1)
         (‣ lsb-first-a?  1 1)
         (‣ soft-reset-a! 2 1)
         (reserved        3 2)
         (‣ soft-reset-b! 5 1)
         (‣ lsb-first-b?  6 1)
         (reserved        7 1))))

(define-register reg:chip-id (items (list (‣ chip-id 0 8))))

(define-register reg:chip-grade
  (items
   (list (reserved     0 4)
         (‣ chip-grade 4 2 (semantics (tbl chip-grade-map)))
         (reserved     6 2))))

(define-register reg:channel-index
  (items
   (list (‣ channel-index 0 2 (semantics (tbl channel-index-map)))
         (reserved        2 6))))

(define-register reg:power-modes
  (items
   (list (‣ power-down 0 2 (semantics (tbl power-down-map)))
         (reserved     2 6))))

(define-register reg:pll-enable
  (items
   (list (reserved      0 2)
         (‣ pll-enable? 2 1)
         (reserved      3 5))))

(define-register reg:pll
  (items
   (list (‣ pll-multiplier 0 6 (semantics (tbl pll-mult-map)))
         (‣ pll-autoband?  6 1)
         (‣ pll-locked?    7 1))))

(define-register reg:analog-input
  (items
   (list (reserved    0 5)
         (‣ bandwidth 5 2 (semantics (tbl bw-map)))
         (reserved    7 1))))

(define-register reg:output-modes
  (items
   (list (‣ output-format      0 2 (semantics (tbl output-format-map)))
         (‣ output-invert?     2 1)
         (reserved             3 1)
         (‣ output-tristate?   4 1)
         (‣ output-interleave? 5 1)
         (reserved             6 1)
         (‣ output-driver      7 1 (semantics (tbl driver-map))))))

(define-register reg:output-adjust
  (items
   (list (‣ drive-strength-18 0 2 (semantics (tbl driver-18-map)))
         (‣ drive-strength-33 2 2 (semantics (tbl driver-33-map)))
         (reserved            4 4))))

(define-register reg:output-clock
  (items
   (list (reserved      0 7)
         (‣ invert-dco? 7 1))))

(define-register reg:reference
  (items
   (list (reserved                  0 6)
         (‣ use-external-reference? 6 1)
         (reserved                  7 1))))

(define-register reg:output-data
  (items
   (list (‣ output-data-rate              0 6 (semantics (tbl data-rate-map)))
         (‣ enable-quad-error-correction? 6 1)
         (reserved                        7 1))))

(define-register reg:overrange
  (items
   (list (‣ overrange-threshold     0 6 (semantics (tbl threshold-map)))
         (‣ indicator-or-condition? 6 1)
         (‣ auto-reset?             7 1))))

(define-register reg:quad-error-correction-1
  (items
   (list (‣ disable-gain-correction?  0 1)
         (‣ disable-phase-correction? 1 1)
         (‣ disable-dc-correction?    2 1)
         (‣ freeze-gain-correction?   3 1)
         (‣ freeze-phase-correction?  4 1)
         (‣ freeze-dc-correction?     5 1)
         (reserved                    6 2))))

(define-register reg:quad-error-correction-2
  (items
   (list (‣ force-gain-correction-initial?  0 1)
         (‣ force-phase-correction-initial? 1 1)
         (‣ force-dc-correction-initial?    2 1)
         (reserved                          3 5))))
