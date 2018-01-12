;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices texas-instruments cdce72010)
  #:use-module (chip-remote device)
  #:use-module (chip-remote manufacturer texas-instruments)
  #:use-module (chip-remote register)
  #:export (cdce72010))

;; The lower four bits of a register represent the register's address.
(define-register reg-x0
  #:default #x002c0040
  #:contents (address #:offset 0 #:width 4 #:force #x0 #:access 'read-only)
             (input-buffer-select #:offset 4 #:width 2)
             (primary-secondary-select #:offset 6 #:width 2)
             (vcxo-select #:offset 8 #:width 1)
             (reference-select-ctrl #:offset 9 #:width 1)
             (delay-pfd #:offset 10 #:width 2)
             (reserved #:offset 12 #:width 1 #:default 0)
             (cp-direction #:offset 13 #:width 1)
             (cp-source #:offset 14 #:width 1)
             (cp-sink #:offset 15 #:width 1)
             (cp-op-amp #:offset 16 #:width 1)
             (cp-preset-output-voltage #:offset 17 #:width 1)
             (cp-current #:offset 18 #:width 4)
             (reserved #:offset 22 #:width 2)
             (i-ref-cp-pull-down-enable #:offset 24 #:width 1)
             (output-mode-0 #:offset 25 #:width 7))

(define-register reg-x1
  #:default #x83840051
  #:contents (address #:offset 0 #:width 4 #:force #x1 #:access 'read-only)
             (ac-dc-select #:offset 4 #:width 1)
             (hysteresis-enable #:offset 5 #:width 1)
             (input-termination #:offset 6 #:width 1)
             (primary-input-bias #:offset 7 #:width 1)
             (secondary-input-bias #:offset 8 #:width 1)
             (fail-safe #:offset 9 #:width 1)
             (coarse-phase-adjust-0/1 #:offset 10 #:width 7)
             (output-divider-0/1 #:offset 17 #:width 7)
             (divider-enable-0/1 #:offset 24 #:width 1)
             (output-mode-1 #:offset 25 #:width 7))

(define-register reg-x2
  #:default #x83840052
  #:contents (address #:offset 0 #:width 4)
             (delay-m #:offset 4 #:width 3)
             (delay-n #:offset 7 #:width 3)
             (coarse-phase-adjust-2 #:offset 10 #:width 7)
             (output-divider-2 #:offset 17 #:width 7)
             (divider-enable-2 #:offset 24 #:width 1)
             (output-mode-2 #:offset 25 #:width 7))

(define-register reg-x3
  #:default #x83400003
  #:contents (address #:offset 0 #:width 4)
             (disable-reference-frequency-detect #:offset 4 #:width 1)
             (disable-fb-frequency-detect #:offset 5 #:width 1)
             (bias-divider-01 #:offset 6 #:width 2)
             (bias-divider-23 #:offset 8 #:width 2)
             (coarse-phase-adjust-3 #:offset 10 #:width 7)
             (output-divider-3 #:offset 17 #:width 7)
             (divider-enable-3 #:offset 24 #:width 1)
             (output-mode-3 #:offset 25 #:width 7))

(define-register reg-x4
  #:default #x81800004
  #:contents (address #:offset 0 #:width 4)
             (reserved #:offset 4 #:width 4)
             (hold-cp-on-loss-of-refclk #:offset 8 #:width 1)
             (reserved #:offset 9 #:width 1)
             (coarse-phase-adjust-4 #:offset 10 #:width 7)
             (output-divider-4 #:offset 17 #:width 7)
             (divider-enable-4 #:offset 24 #:width 1)
             (output-mode-4 #:offset 25 #:width 7))

(define-register reg-x5
  #:default #x81800005
  #:contents (address #:offset 0 #:width 4)
             (bias-divider-45 #:offset 4 #:width 2)
             (bias-divider-67 #:offset 6 #:width 2)
             (reserved #:offset 8 #:width 2)
             (coarse-phase-adjust-5 #:offset 10 #:width 7)
             (output-divider-5 #:offset 17 #:width 7)
             (divider-enable-5 #:offset 24 #:width 1)
             (output-mode-5 #:offset 25 #:width 7))

(define-register reg-x6
  #:default #xeb040006
  #:contents (address #:offset 0 #:width 4)
             (fb-frequency-detect-connected-to-lock-detect #:offset 4 #:width 1)
             (reserved #:offset 5 #:width 1)
             (fb-determ-divider-select #:offset 6 #:width 1)
             (fb-determ-divider-2-disable #:offset 7 #:width 1)
             (fb-start-bypass #:offset 8 #:width 1)
             (det-start-bypass #:offset 9 #:width 1)
             (coarse-phase-adjust-6 #:offset 10 #:width 7)
             (output-divider-6 #:offset 17 #:width 7)
             (divider-enable-6 #:offset 24 #:width 1)
             (output-mode-6 #:offset 25 #:width 7))

(define-register reg-x7
  #:default #xeb040717
  #:contents (address #:offset 0 #:width 4)
             (lock-detect-window-a #:offset 4 #:width 2)
             (reserved #:offset 6 #:width 1)
             (coherent-lock #:offset 7 #:width 2)
             (analog-digital-lock-detect #:offset 9 #:width 1)
             (coarse-phase-adjust-7 #:offset 10 #:width 7)
             (output-divider-7 #:offset 17 #:width 7)
             (divider-enable-7 #:offset 24 #:width 1)
             (output-mode-7 #:offset 25 #:width 7))

(define-register reg-x8
  #:default #x010c0158
  #:contents (address #:offset 0 #:width 4)
             (vcxo-buffer-select #:offset 4 #:width 2)
             (vcxo-ac-dc-select #:offset 6 #:width 1)
             (vcxo-hysteresis-enable #:offset 7 #:width 1)
             (vcxo-input-termination #:offset 8 #:width 1)
             (vcxo-input-bias #:offset 9 #:width 1)
             (coarse-phase-adjust-8/9 #:offset 10 #:width 7)
             (output-divider-8/9 #:offset 17 #:width 7)
             (divider-enable-8/9 #:offset 24 #:width 1)
             (output-mode-8 #:offset 25 #:width 7))

(define-register reg-x9
  #:default #x01000049
  #:contents (address #:offset 0 #:width 4)
             (external-hold-over-function #:offset 4 #:width 1)
             (reserved #:offset 5 #:width 1)
             (hold #:offset 6 #:width 1)
             (lock-triggers-hold #:offset 7 #:width 1)
             (hold-count #:offset 8 #:width 2)
             (lock-detect-window-b #:offset 10 #:width 2)
             (no-invert-reset-hold #:offset 12 #:width 1)
             (divider-sync-disable #:offset 13 #:width 1)
             (start-bypass #:offset 14 #:width 1)
             (indet-bp #:offset 15 #:width 1)
             (pll-lock-bypass #:offset 16 #:width 1)
             (low-fd-fb-en #:offset 17 #:width 1)
             (npreset-m-divider #:offset 18 #:width 1)
             (bias-fb-div #:offset 19 #:width 2)
             (bias-div-8/9 #:offset 21 #:width 2)
             (aux-input-bias #:offset 23 #:width 1)
             (disable-aux-input #:offset 24 #:width 1)
             (output-mode-9 #:offset 25 #:width 7))

(define-register reg-xa
  #:default #x0bfc07ca
  #:contents (address #:offset 0 #:width 4)
             (m-divider #:offset 4 #:width 14)
             (n-divider #:offset 18 #:width 14))

(define-register reg-xb
  #:default #x0000058b
  #:contents (address #:offset 0 #:width 4)
             (primary-reference-divider #:offset 4 #:width 1)
             (secondary-reference-divider #:offset 5 #:width 1)
             (fb-div-disable #:offset 6 #:width 1)
             (fb-logic-mode-sel #:offset 7 #:width 1)
             (fb-input-clk-invert #:offset 8 #:width 1)
             (fb-divider #:offset 9 #:width 7)
             (fb-coarse-phase-adjust #:offset 16 #:width 7)
             (pll-power-down #:offset 23 #:width 1)
             (fb-mux-sel #:offset 24 #:width 1)
             (out-mux-sel #:offset 25 #:width 1)
             (fb-sel #:offset 26 #:width 1)
             (ref-clk-reshape #:offset 27 #:width 1)
             (ref-clk-delay-sel #:offset 28 #:width 1)
             (reset-hold-sel #:offset 29 #:width 1)
             (eeprom-lock-status #:offset 30 #:width 1)
             (eeprom-status #:offset 31 #:width 1))

(define-register reg-xc
  #:default #x61e09c0c
  #:contents (address #:offset 0 #:width 4)
             (reserved #:offset 4 #:width 4)
             (aux-in-present #:offset 8 #:width 1)
             (vcxo-in-present #:offset 9 #:width 1)
             (pll-lock-status #:offset 10 #:width 1)
             (device-sleep #:offset 11 #:width 1)
             (software-hold-reset #:offset 12 #:width 1)
             (general-test-mode-enable #:offset 13 #:width 1)
             (revision-control #:offset 14 #:width 3)
             (power-down-io #:offset 17 #:width 1)
             (sxoiref #:offset 18 #:width 1)
             (route-hold-to-pll-lock #:offset 19 #:width 1)
             (reserved #:offset 20 #:width 1)
             (ti-test-status #:offset 24 #:width 4)
             (ti-test-config #:offset 28 #:width 4)
             (primary-ref-clk-present #:offset 29 #:width 1)
             (secondary-ref-clk-present #:offset 30 #:width 1)
             (reserved #:offset 31 #:width 1))

(define-device cdce72010
  #:manufacturer texas-instruments
  #:homepage "http://www.ti.com/product/cdce72010"
  #:datasheet "http://www.ti.com/lit/ds/symlink/cdce72010.pdf"
  #:keywords '(clock distribution synchonisation pll jitter cleaner)
  #:register-width 32
  #:register-map (#:table* (#x0 reg-x0) (#x1 reg-x1) (#x2 reg-x2) (#x3 reg-x3)
                           (#x4 reg-x4) (#x5 reg-x5) (#x6 reg-x6) (#x7 reg-x7)
                           (#x8 reg-x8) (#x9 reg-x9) (#xa reg-xa) (#xb reg-xb)
                           (#xc reg-xc)))
