;; Copyright (c) 2014-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti ads4149 tables)
  #:export (clkout-pos-fall-cmos
            clkout-pos-fall-lvds
            clkout-pos-rise-cmos
            clkout-pos-rise-lvds
            cmos-clkout-strength-map
            correction-time-map
            data-format-map
            gain-map
            high-performace-mode-map
            low-speed-map
            lvds-cmos-select-map
            lvds-strength-map
            lvds-swing-control-map
            lvds-swing-map
            test-pattern-map))

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
    (500   . #b01)
    (aligned . #b10)
    (200   . #b11)))

(define clkout-pos-rise-cmos ;; ps
  '((default . #b00)
    (100   . #b01)
    (200   . #b10)
    (1500  . #b11)))

(define clkout-pos-fall-lvds ;; ps
  '((default . #b00)
    (400   . #b01)
    (aligned . #b10)
    (200   . #b11)))

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
