;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices texas-instruments ads4149)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote device)
  #:use-module (chip-remote item builder)
  #:use-module (chip-remote manufacturer texas-instruments)
  #:use-module (chip-remote register-map)
  #:export (ads4149))

(define lvds-swing-map ;; mV
  '((350 . #b000000)
    (410 . #b011011)
    (465 . #b110010)
    (570 . #b010100)
    (200 . #b111110)
    (125 . #b001111)))

(define lvds-strength-map
  '((default . #b0)
    (double  . #b1)))

(define data-format-map
  '((dfs-pin         . #b00)
    (twos-complement . #b10)
    (offset-binary   . #b11)))

(define lvds-cmos-select-map
  '((dfs-pin . #b00)
    (dfs-pin . #b10)
    (lvds    . #b01)
    (cmos    . #b11)))

(define cmos-clkout-strength-map
  '((maximum  . #b00)
    (medium   . #b01)
    (low      . #b10)
    (very-low . #b11)))

(define clkout-pos-rise-lvds ;; ps
  '((default . #b00)
    (500     . #b01)
    (aligned . #b10)
    (200     . #b11)))

(define clkout-pos-rise-cmos ;; ps
  '((default . #b00)
    (100     . #b01)
    (200     . #b10)
    (1500    . #b11)))

(define clkout-pos-fall-lvds ;; ps
  '((default . #b00)
    (400     . #b01)
    (aligned . #b10)
    (200     . #b11)))

(define clkout-pos-fall-cmos clkout-pos-rise-cmos)

(define high-performace-mode-map
  '((off . #b00)
    (on  . #b11)))

(define gain-map ;; dB
  '((0    . #b0000)
    (1/2  . #b0001)
    (1    . #b0010)
    (3/2  . #b0011)
    (2    . #b0100)
    (5/2  . #b0101)
    (3    . #b0110)
    (7/2  . #b0111)
    (4    . #b1000)
    (9/2  . #b1001)
    (5    . #b1010)
    (10/2 . #b1011)
    (6    . #b1100)))

(define test-pattern-map
  '((normal  . #b000)
    (zeros   . #b001)
    (ones    . #b010)
    (builtin . #b011)
    (ramp    . #b100)
    (custom  . #b101)))

(define lvds-swing-control-map
  '((disabled . #b00)
    (enabled  . #b11)))

(define correction-time-map ;; M
  '((1    . #b0000)
    (2    . #b0001)
    (4    . #b0010)
    (8    . #b0011)
    (16   . #b0100)
    (32   . #b0101)
    (64   . #b0110)
    (128  . #b0111)
    (256  . #b1000)
    (512  . #b1001)
    (1014 . #b1010)
    (2048 . #b1011)))

(define low-speed-map
  '((disabled . #b00)
    (disabled . #b01)
    (disabled . #b10)
    (enabled  . #b11)))

(define-register-map ads4149-register-map
  #:table
  (#x0 (#:contents (serial-readout? 0 1)
                   (device-reset! 1 1)
                   (=> (reserved 2 6))))
  (#x1 (#:contents (=> (reserved 0 2))
                   (lvds-swing 2 6 #:semantics lookup lvds-swing-map)))
  (#x3 (#:contents (high-performance-mode-1
                    0 2 #:semantics lookup high-performace-mode-map)
                   (=> (reserved 2 6))))
  (#x25 (#:contents (test-pattern 0 3
                                  #:semantics lookup test-pattern-map)
                    (disable-gain? 3 1)
                    (gain 4 4 #:semantics lookup gain-map)))
  (#x26 (#:contents (lvds-data-strength
                     0 1 #:semantics lookup lvds-strength-map)
                    (lvds-clkout-strength
                     1 1 #:semantics lookup lvds-strength-map)
                    (=> (reserved 2 6))))
  (#x3d (#:contents (=> (reserved 0 5))
                    (enable-offset-correction? 5 1)
                    (data-format 6 2
                                 #:semantics lookup data-format-map)))
  (#x3f (#:contents (custom-pattern-high 0 8)))
  (#x40 (#:contents (=> (reserved 0 2))
                    (custom-pattern-low 2 6)))
  (#x41 (#:contents (enable-clkout-fall 0 1)
                    (clkout-rise-posn 1 2)
                    (enable-clkout-rise 3 1)
                    (cmos-clkout-strength
                     4 2 #:semantics lookup lvds-strength-map)
                    (lvds-cmos 6 2
                               #:semantics lookup lvds-cmos-select-map)))
  (#x42 (#:contents (=> (reserved 0 2))
                    (standby? 2 1)
                    (disable-low-latency? 3 1)
                    (=> (reserved 4 2))
                    (clkout-fall-posn 6 2)))
  (#x43 (#:contents (enable-lvds-swing
                     0 2 #:semantics lookup lvds-swing-control-map)
                    (=> (reserved 2 2))
                    (power-down-outputs? 4 1)
                    (=> (reserved 5 1))
                    (power-down-global? 6 1)
                    (=> (reserved 7 1))))
  (#x4a (#:contents (high-performance-mode-2 0 1)
                    (=> (reserved 1 7))))
  (#xbf (#:contents (=> (reserved 0 2))
                    (offset-pedestal 2 6
                                     #:semantics* twos-complement)))
  (#xcf (#:contents (=> (reserved 0 2))
                    (offset-correction-time-constant
                     2 4 #:semantics lookup correction-time-map)
                    (=> (reserved 6 1))
                    (freeze-offset-correction 7 1)))
  (#xdf (#:contents (=> (reserved 0 4))
                    (low-speed 4 2
                               #:semantics lookup low-speed-map)
                    (=> (reserved 6 2)))))

(define-device ads4149
  #:manufacturer texas-instruments
  #:homepage "http://www.ti.com/product/ADS4149"
  #:datasheet "http://www.ti.com/lit/ds/symlink/ads4149.pdf"
  #:keywords '(14bit 250MS/s analog-to-digital converter)
  #:register-width 8
  #:register-map* ads4149-register-map)
