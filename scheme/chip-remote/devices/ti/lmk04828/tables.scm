;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti lmk04828 tables)
  #:export (clkin-sel-mode-map
            clkin-sel-polarity-map
            clkin-sel0-mux-map
            clkin-sel1-mux-map
            clkin-type-map
            clkin0-out-mux-map
            clkin1-out-mux-map
            clkout-mux-map
            dac-clk-mult-map
            devclk-sysref-map
            fb-mux-map
            los-timeout-map
            oscin-freq-map
            oscout-format-map
            oscout-mux-map
            output-format-map
            output-polarity-map
            pin-type-map
            pll2-loopfilter-c3-map
            pll2-loopfilter-c4-map
            pll2-loopfilter-resistor-map
            pll2-prescaler-map
            pll2-window-size-map
            pll2-nclk-mux-map
            pll1-nclk-mux-map
            pll1-cp-gain-map
            pll2-cp-gain-map
            pll-cp-polarity-map
            pll-ld-mux-map
            pll-ld-type-map
            pll1-window-size-map
            pll1-delay-map
            reset-mux-map
            sdclkout-ddelay-map
            sdio-readback-type-map
            sync-polarity-map
            sync-mode-map
            sysref-mux-map
            sysref-output-state-map
            vco-mux-map))

(define clkin-sel-mode-map
  '((clkin-0-manual . #b000)
    (clkin-1-manual . #b001)
    (clkin-2-manual . #b010)
    (pin-select     . #b011)
    (auto-mode      . #b100)
    (reserved       . #b101)
    (reserved       . #b110)
    (reserved       . #b111)))

(define clkin-sel0-mux-map
  '((logic-low       . #b000)
    (clkin0-los      . #b001)
    (clkin0-selected . #b010)
    (dac-locked      . #b011)
    (dac-low         . #b100)
    (dac-high        . #b101)
    (spi-readback    . #b110)
    (reserved        . #b111)))

(define clkin-sel1-mux-map
  '((logic-low       . #b000)
    (clkin0-los      . #b001)
    (clkin0-selected . #b010)
    (dac-locked      . #b011)
    (dac-low         . #b100)
    (dac-high        . #b101)
    (spi-readback    . #b110)
    (reserved        . #b111)))

(define clkin-sel-polarity-map
  '((active-high . #b0)
    (active-low . #b1)))

(define clkin-type-map
  '((bipolar . #b0)
    (mos . #b1)))

(define clkin1-out-mux-map
  '((f-in         . #b00)
    (feedback-mux . #b01)
    (pll-1        . #b10)
    (reserved     . #b11)))

(define clkin0-out-mux-map
  '((sysref-mux . #b00)
    (reserved   . #b01)
    (pll-1      . #b10)
    (reserved   . #b11)))

(define clkout-mux-map
  '((divider-only . #b00)
    (div+dcc+hs   . #b01)
    (bypass       . #b10)
    (analog+div   . #b11)))

(define dac-clk-mult-map
  '((    4 . #b00)
    (   64 . #b01)
    ( 1024 . #b10)
    (16384 . #b11)))

(define devclk-sysref-map
  '((device-clock . #b0)
    (sysref       . #b1)))

(define fb-mux-map
  '((dclkout-6 . #b00)
    (dclkout-8 . #b01)
    (sysref    . #b10)
    (external  . #b11)))

(define los-timeout-map ;; in kHz
  '((370   . #b00)
    (2100  . #b01)
    (8800  . #b10)
    (22000 . #b11)))

(define pin-type-map
  '((input                     . #b000)
    (input-with-pull-up        . #b001)
    (input-with-pull-down      . #b010)
    (output-push-pull          . #b011)
    (output-inverted-push-pull . #b100)
    (reserved                  . #b101)
    (output-open-drain         . #b110)
    (reserved                  . #b111)))

(define sdclkout-ddelay-map
  '((reserved . #x0)
    (2  . #x1)
    (3  . #x2)
    (4  . #x3)
    (5  . #x4)
    (6  . #x5)
    (7  . #x6)
    (8  . #x7)
    (9  . #x8)
    (10 . #x9)
    (11 . #xa)
    (reserved . #xb)
    (reserved . #xc)
    (reserved . #xd)
    (reserved . #xe)
    (reserved . #xf)))

(define sysref-output-state-map
  '((normal . #b00)
    (active . #b01)
    (active . #b10)
    (vcm-voltage . #b11)))

(define output-polarity-map
  '((normal . #b0)
    (inverted . #b1)))

(define output-format-map
  '((powerdown     . #b000)
    (lvds          . #b001)
    (hsds-6mA      . #b010)
    (hsds-8mA      . #b011)
    (hsds-10mA     . #b100)
    (lvpecl-1600mV . #b101)
    (lvpecl-2000mV . #b110)
    (lvpecl        . #b111)))

(define pll1-nclk-mux-map
  '((osc-in       . #b0)
    (feedback-mux . #b1)))

(define pll-cp-polarity-map
  '((negative-slope . #b0)
    (positive-slope . #b1)))

(define pll1-cp-gain-map ;; in mico amperes
  '((  50 . #b0000)
    ( 150 . #b0001)
    ( 250 . #b0010)
    ( 350 . #b0011)
    ( 450 . #b0100)
    ( 550 . #b0101)
    ( 650 . #b0110)
    ( 750 . #b0111)
    ( 850 . #b1000)
    ( 950 . #b1001)
    (1050 . #b1010)
    (1150 . #b1011)
    (1250 . #b1100)
    (1350 . #b1101)
    (1450 . #b1110)
    (1550 . #b1111)))

(define pll-ld-mux-map
  '((logic-low         . #b00000)
    (pll1-dld          . #b00001)
    (pll2-dld          . #b00010)
    (pll1-and-pll2-dld . #b00011)
    (holdover-status   . #b00100)
    (dac-locked        . #b00101)
    (reserved          . #b00110)
    (spi-readback      . #b00111)
    (dac-rail          . #b01000)
    (dac-low           . #b01001)
    (dac-high          . #b01010)
    (pll1-n            . #b01011)
    (pll1-n/2          . #b01100)
    (pll2-n            . #b01101)
    (pll2-n/2          . #b01110)
    (pll1-r            . #b01111)
    (pll1-r/2          . #b10000)
    (pll2-r            . #b10001)
    (pll2-r/2          . #b10010)
    (reserved          . #b10011)
    (reserved          . #b10100)
    (reserved          . #b10101)
    (reserved          . #b10110)
    (reserved          . #b10111)
    (reserved          . #b11000)
    (reserved          . #b11001)
    (reserved          . #b11010)
    (reserved          . #b11011)
    (reserved          . #b11100)
    (reserved          . #b11101)
    (reserved          . #b11110)
    (reserved          . #b11111)))

(define pll-ld-type-map
  '((reserved                  . #b000)
    (reserved                  . #b001)
    (reserved                  . #b010)
    (output-push-pull          . #b011)
    (output-inverted-push-pull . #b100)
    (reserved                  . #b101)
    (output-open-drain         . #b110)
    (reserved                  . #b111)))

(define pll1-delay-map ;; in pico seconds
  '((   0 . #b000)
    ( 205 . #b001)
    ( 410 . #b010)
    ( 615 . #b011)
    ( 820 . #b100)
    (1025 . #b101)
    (1230 . #b110)
    (1435 . #b111)))

(define pll1-window-size-map ;; in nano seconds
  '(( 4 . #b00)
    ( 9 . #b01)
    (19 . #b10)
    (43 . #b11)))

(define pll2-window-size-map
  '((reserved . #b00)
    (reserved . #b01)
    (3.7ns    . #b10)
    (reserved . #b11)))

(define pll2-loopfilter-c3-map ;; in pico farad
  '((10       . #b0000)
    (11       . #b0001)
    (15       . #b0001)
    (16       . #b0001)
    (19       . #b0001)
    (20       . #b0001)
    (24       . #b0001)
    (25       . #b0001)
    (29       . #b0001)
    (30       . #b0001)
    (33       . #b0001)
    (34       . #b0001)
    (38       . #b0001)
    (39       . #b0001)
    (reserved . #b0001)
    (reserved . #b0001)))

(define pll2-loopfilter-c4-map ;; in pico farad
  '(( 10      . #b0000)
    ( 15      . #b0001)
    ( 29      . #b0001)
    ( 34      . #b0001)
    ( 47      . #b0001)
    ( 52      . #b0001)
    ( 66      . #b0001)
    ( 71      . #b0001)
    (103      . #b0001)
    (108      . #b0001)
    (122      . #b0001)
    (126      . #b0001)
    (141      . #b0001)
    (146      . #b0001)
    (reserved . #b0001)
    (reserved . #b0001)))

(define pll2-loopfilter-resistor-map ;; in Ohm
  '((  200    . #b000)
    ( 1000    . #b001)
    ( 2000    . #b010)
    ( 4000    . #b011)
    (16000    . #b100)
    (reserved . #b101)
    (reserved . #b110)
    (reserved . #b111)))

(define pll2-cp-gain-map ;; in micro amperes
  '(( 100 . #b00)
    ( 400 . #b01)
    (1600 . #b10)
    (3200 . #b11)))

(define pll2-nclk-mux-map
  '((pll-prescaler . #b0)
    (feedback-mux  . #b1)))

(define pll2-prescaler-map
  '((8 . 0) ;; #b000 → prescaler := 8.
    (2 . 1) ;; no, really: 1 → 2. That's what the datasheet says.
    (2 . 2)
    (3 . 3)
    (4 . 4)
    (5 . 5)
    (6 . 6)
    (7 . 7)))

(define vco-mux-map
  '((VCO-0        . #b00)
    (VCO-1        . #b01)
    (external-VCO . #b10)
    (reserver     . #b11)))

(define oscin-freq-map
  '((=0-63-MHz    . #b000)
    (>64-127-MHz  . #b001)
    (>127-255-MHz . #b010)
    (reserved     . #b011)
    (>255-500-MHz . #b100)
    (reserved     . #b101)
    (reserved     . #b110)
    (reserved     . #b111)))

(define oscout-mux-map
  '((buffered-oscin . 0)
    (feedback-mux   . 1)))

(define oscout-format-map
  '((powerdown        . #x0)
    (lvds             . #x1)
    (reserved         . #x2)
    (reserved         . #x3)
    (lvpecl-1600mV    . #x4)
    (lvpecl-2000mV    . #x5)
    (lvcmos-norm-inv  . #x6)
    (lvcmos-inv-norm  . #x7)
    (lvcmos-norm-norm . #x8)
    (lvcmos-inv-inv   . #x9)
    (lvcmos-off-norm  . #xa)
    (lvcmos-off-inv   . #xb)
    (lvcmos-norm-off  . #xc)
    (lvcmos-inv-off   . #xd)
    (lvcmos-off-off   . #xe)
    (reserved         . #xf)))

(define reset-mux-map
  '((logic-low       . #b000)
    (reserved        . #b001)
    (clkin2-selected . #b010)
    (dac-locked      . #b011)
    (dac-low         . #b100)
    (dac-high        . #b101)
    (spi-readback    . #b110)
    (reserved        . #b111)))

(define sdio-readback-type-map
  '((output-push-pull . #b0)
    (output-open-drain . #b1)))

(define sync-polarity-map
  '((normal . #b0)
    (inverted . #b1)))

(define sync-mode-map
  '((sync-disabled . #b00)
    (sync-from-sync-pin . #b01)
    (sync-from-sync-pin-pulsor . #b10)
    (sync-from-spi-write . #b11)))

(define sysref-mux-map
  '((normal-sync     . #b00)
    (re-clocked      . #b01)
    (sysref-pulser   . #b10)
    (sysref-contious . #b11)))
