;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices analog-devices adf4169 registers)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote interpreter)
  #:use-module (chip-remote item)
  #:use-module (chip-remote item builder)
  #:use-module (chip-remote named-value)
  #:use-module (chip-remote register)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote devices analog-devices adf4169 tables)
  #:export (reg:frac/int
            reg:lsb-frac
            reg:r-divider
            reg:function
            reg:clock
            reg:deviation
            reg:step
            reg:delay))

(define address (make-address #:width 3))

(define-semantics div-integer
  (inherit unsigned-integer)
  (range (lambda (s w) `(range 23 4095)))
  (default (const 1024)))

(define-register reg:frac/int
  (items
   (list (address 0)
         (‣ div-fractional-msb  3 12 (default 0))
         (‣ div-integer        15 12 (semantics div-integer))
         (‣ muxout-ctrl        27  4 (semantics (tbl muxout-cfg
                                                     #:without '(reserved)
                                                     #:default 'three-state)))
         (‣ ramp-enabled?      31 1 (default #f)))))

(define-register reg:lsb-frac
  (items
   (list (address 1)
         (‣ phase               3 12 (semantics twos-complement))
         (‣ div-fractional-lsb 15 13 (default 0))
         (‣ phase-adjust       28  1 (default #f))
         (reserved             29  3))))

(define-semantics unsigned-zero-is-32
  (range (lambda (s w) '(range 1 32)))
  (default (const 4))
  (encode (make-evaluation '(lambda (w x) (if (x = 32) 0 x))))
  (decode (make-evaluation '(lambda (w x) (if (x = 0) 32 x)))))

(define-register reg:r-divider
  (items
   (list (address 2)
         (‣ clk1-divider           3 12 (default 4095))
         (‣ r-divider             15  5 (semantics unsigned-zero-is-32))
         (‣ reference-doubler?    20  1 (default #f))
         (‣ reference-div-by-2?   21  1 (default #f))
         (‣ prescaler             22  1 (semantics (tbl prescaler-cfg
                                                        #:default 4/5)))
         (reserved                23  1)
         (‣ charge-pump-current   24  4 (semantics (tbl charge-pump-cfg
                                                        #:default 5)))
         (‣ cycle-spin-reduction? 28  1 (default #f))
         (reserved                29  3))))

(define-register reg:function
  (items
   (list (address 3)
         (‣ rf-counter-reset?        3 1 (default #f))
         (‣ charge-pump-3state       4 1 (default #f))
         (‣ power-down?              5 1 (default #f))
         (‣ phase-detect-polarity    6 1 (semantics
                                          (tbl phase-detect-polarity-map
                                               #:default 'positive)))
         (‣ lock-detect-precision    7 1 (semantics
                                          (tbl lock-detect-precision-map
                                               #:default 6)))
         (‣ fsk-enable?             8 1 (default #f))
         (‣ psk-enable?             9 1 (default #f))
         (‣ ramp-mode              10 2 (semantics (tbl ramp-mode-cfg
                                                        #:default 'sawtooth)))
         (reserved 12 2)
         (‣ sigma-delta-reset?     14 1
                                   (semantics boolean/active-low)
                                   (default #t))
         (‣ n-divider-load-delay?  15 1 (default #t))
         (‣ loss-of-lock-enable?   16 1
                                   (semantics boolean/active-low)
                                   (default #f))
         (reserved                 17 4 #:default #b0001)
         (‣ negative-bleed-enable? 21 1 (default #f))
         (‣ negative-bleed-current 22 3 (semantics (tbl negative-bleed-map
                                                        #:default 3730/1000)))
         (reserved                 25 7))))

(define-register reg:clock
  (items
   (list (address 4)
         (reserved 3 3)
         (‣ clock-divider-select  6  1 (semantics
                                        (tbl word-select #:default 'word-1)))
         (‣ clock2-divider        7 12 (default 1024))
         (‣ clock-divider-mode   19  2 (semantics
                                        (tbl clk-divider-mode-map
                                             #:without '(reserved)
                                             #:default 'clock-divider-off)))
         (‣ ramp-status          21  5 (semantics
                                        (tbl ramp-status-map
                                             #:default 'normal-operation)))
         (‣ sigma-delta-mode     26  5 (semantics
                                        (tbl sigma-delta-mode-map
                                             #:default 'normal-operation)))
         (‣ latch-enable-select  31  1 (semantics
                                        (tbl latch-enable-map
                                             #:default 'sync-with-refin))))))

(define-semantics deviation-offset
  (inherit unsigned-integer)
  (range (lambda (s w) `(range 0 9))))

(define-register reg:deviation
  (items
   (list (address 5)
         (‣ deviation         3 16 (semantics twos-complement))
         (‣ deviation-offset 19  4 (semantics deviation-offset))
         (‣ deviation-select 23  1 (semantics (tbl word-select
                                                   #:default 'word-1)))
         (‣ dual-ramp-enable? 24 1 (default #f))
         (‣ fsk-ramp-enable?  25 1 (default #f))
         (‣ interrupt-ctrl    26 2 (semantics (tbl interrupt-ctrl-map
                                                   #:default 'interrupt-off)))
         (reserved            28 1)
         (‣ tx-data-ramp-clock-select 29 1 (semantics
                                            (tbl tx-data-ramp-clock-map
                                                 #:default 'clock-divider)))
         (‣ tx-data-invert?   30 1 (default #f))
         (reserved 31 1))))

(define-register reg:step
  (items
   (list (address 6)
         (‣ step         3 20 (default 0))
         (‣ step-select 23  1 (semantics (tbl word-select #:default 'word-1)))
         (reserved      24  8))))

(define-register reg:delay
  (items
   (list (address 7)
         (‣ delay-start              3 12 (default 0))
         (‣ delay-start-enable?     15 1 (default #f))
         (‣ delay-clock-select      16 1 (semantics (tbl delay-clock-map
                                                         #:default 'pfd-clock)))
         (‣ ramp-delay-enable?      17 1 (default #f))
         (reserved                  18 1)
         (‣ fast-ramp-enable?       19 1 (default #f))
         (‣ tx-data-trigger-enable? 20 1 (default #f))
         (‣ single-full-triangle?   21 1 (default #f))
         (‣ triangle-delay-enable?  22 1 (default #f))
         (‣ tx-data-trigger-delay?  23 1 (default #f))
         (reserved                  24 8))))
