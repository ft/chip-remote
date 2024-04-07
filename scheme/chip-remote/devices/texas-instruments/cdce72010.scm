;; Copyright (c) 2017-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices texas-instruments cdce72010)
  #:use-module (srfi srfi-1)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote device)
  #:use-module (chip-remote device access)
  #:use-module (chip-remote interact)
  #:use-module (chip-remote item)
  #:use-module (chip-remote item access)
  #:use-module (chip-remote manufacturer texas-instruments)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote protocol)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote utilities)
  #:export (cdce72010
            cdce72010-write-eeprom
            cdce72010-lock-eeprom))

;; TODO: This has no table based custom semantics. All that stuff is just
;; unsigned integer here. This could be improved upon.

(define (address n)
  (‣ address 0 4 (default n) (access ro)))

;; The lower four bits of a register represent the register's address.
(define-register reg-x0
  ;; TODO: Should we allow default annotations, similar to register-width? That
  ;; could be testable, automatically as well. Hm.
  ;;(default #x002c0040)
  (items
   (list (address 0)
         (‣ input-buffer-select        4 2)
         (‣ primary-secondary-select   6 2)
         (‣ vcxo-select                8 1)
         (‣ reference-select-ctrl      9 1)
         (‣ delay-pfd                 10 2)
         (‣ reserved                  12 1 (default 0))
         (‣ cp-direction              13 1)
         (‣ cp-source                 14 1)
         (‣ cp-sink                   15 1)
         (‣ cp-op-amp                 16 1)
         (‣ cp-preset-output-voltage  17 1)
         (‣ cp-current                18 4)
         (‣ reserved                  22 2)
         (‣ i-ref-cp-pull-down-enable 24 1)
         (‣ output-mode-0             25 7))))

(define-register reg-x1
  ;;(default #x83840051)
  (items
   (list (address 1)
         (‣ ac-dc-select             4 1)
         (‣ hysteresis-enable        5 1)
         (‣ input-termination        6 1)
         (‣ primary-input-bias       7 1)
         (‣ secondary-input-bias     8 1)
         (‣ fail-safe                9 1)
         (‣ coarse-phase-adjust-0/1 10 7)
         (‣ output-divider-0/1      17 7)
         (‣ divider-enable-0/1      24 1)
         (‣ output-mode-1           25 7))))

(define-register reg-x2
  ;;(default #x83840052)
  (items
   (list (address 2)
         (‣ delay-m 4 3)
         (‣ delay-n 7 3)
         (‣ coarse-phase-adjust-2 10 7)
         (‣ output-divider-2 17 7)
         (‣ divider-enable-2 24 1)
         (‣ output-mode-2 25 7))))

(define-register reg-x3
  ;;(default #x83400003)
  (items
   (list (address 3)
         (‣ disable-reference-frequency-detect  4 1)
         (‣ disable-fb-frequency-detect         5 1)
         (‣ bias-divider-01                     6 2)
         (‣ bias-divider-23                     8 2)
         (‣ coarse-phase-adjust-3              10 7)
         (‣ output-divider-3                   17 7)
         (‣ divider-enable-3                   24 1)
         (‣ output-mode-3                      25 7))))

(define-register reg-x4
  ;;(default #x81800004)
  (items
   (list (address 4)
         (‣ reserved 4 4)
         (‣ hold-cp-on-loss-of-refclk  8 1)
         (‣ reserved                   9 1)
         (‣ coarse-phase-adjust-4     10 7)
         (‣ output-divider-4          17 7)
         (‣ divider-enable-4          24 1)
         (‣ output-mode-4             25 7))))

(define-register reg-x5
  ;;(default #x81800005)
  (items
   (list (address 5)
         (‣ bias-divider-45        4 2)
         (‣ bias-divider-67        6 2)
         (‣ reserved               8 2)
         (‣ coarse-phase-adjust-5 10 7)
         (‣ output-divider-5      17 7)
         (‣ divider-enable-5      24 1)
         (‣ output-mode-5         25 7))))

(define-register reg-x6
  ;;(default #xeb040006)
  (items
   (list (address 6)
         (‣ fb-frequency-detect-connected-to-lock-detect 4 1)
         (‣ reserved                     5 1)
         (‣ fb-determ-divider-select     6 1)
         (‣ fb-determ-divider-2-disable  7 1)
         (‣ fb-start-bypass              8 1)
         (‣ det-start-bypass             9 1)
         (‣ coarse-phase-adjust-6       10 7)
         (‣ output-divider-6            17 7)
         (‣ divider-enable-6            24 1)
         (‣ output-mode-6               25 7))))

(define-register reg-x7
  ;;(default #xeb040717)
  (items
   (list (address 7)
         (‣ lock-detect-window-a        4 2)
         (‣ reserved                    6 1)
         (‣ coherent-lock               7 2)
         (‣ analog-digital-lock-detect  9 1)
         (‣ coarse-phase-adjust-7      10 7)
         (‣ output-divider-7           17 7)
         (‣ divider-enable-7           24 1)
         (‣ output-mode-7              25 7))))

(define-register reg-x8
  ;;(default #x010c0158)
  (items
   (list (address 8)
         (‣ vcxo-buffer-select       4 2)
         (‣ vcxo-ac-dc-select        6 1)
         (‣ vcxo-hysteresis-enable   7 1)
         (‣ vcxo-input-termination   8 1)
         (‣ vcxo-input-bias          9 1)
         (‣ coarse-phase-adjust-8/9 10 7)
         (‣ output-divider-8/9      17 7)
         (‣ divider-enable-8/9      24 1)
         (‣ output-mode-8           25 7))))

(define-register reg-x9
  ;;(default #x01000049)
  (items
   (list (address 9)
         (‣ external-hold-over-function 4 1)
         (‣ reserved              5 1)
         (‣ hold 6 1)
         (‣ lock-triggers-hold    7 1)
         (‣ hold-count            8 2)
         (‣ lock-detect-window-b 10 2)
         (‣ no-invert-reset-hold 12 1)
         (‣ divider-sync-disable 13 1)
         (‣ start-bypass         14 1)
         (‣ indet-bp             15 1)
         (‣ pll-lock-bypass      16 1)
         (‣ low-fd-fb-en         17 1)
         (‣ npreset-m-divider    18 1)
         (‣ bias-fb-div          19 2)
         (‣ bias-div-8/9         21 2)
         (‣ aux-input-bias       23 1)
         (‣ disable-aux-input    24 1)
         (‣ output-mode-9        25 7))))

(define-register reg-xa
  ;;(default #x0bfc07ca)
  (items
   (list (address 10)
         (‣ m-divider  4 14)
         (‣ n-divider 18 14))))

(define-register reg-xb
  ;;(default #x0000058b)
  (items
   (list (address 11)
         (‣ primary-reference-divider    4 1)
         (‣ secondary-reference-divider  5 1)
         (‣ fb-div-disable               6 1)
         (‣ fb-logic-mode-sel            7 1)
         (‣ fb-input-clk-invert          8 1)
         (‣ fb-divider                   9 7)
         (‣ fb-coarse-phase-adjust      16 7)
         (‣ pll-power-down              23 1)
         (‣ fb-mux-sel                  24 1)
         (‣ out-mux-sel                 25 1)
         (‣ fb-sel                      26 1)
         (‣ ref-clk-reshape             27 1)
         (‣ ref-clk-delay-sel           28 1)
         (‣ reset-hold-sel              29 1)
         (‣ eeprom-lock-status          30 1)
         (‣ eeprom-status               31 1))))

(define-register reg-xc
  ;;(default #x61e09c0c)
  (items
   (list (address 12)
         (‣ reserved                   4 4)
         (‣ aux-in-present             8 1)
         (‣ vcxo-in-present            9 1)
         (‣ pll-lock-status           10 1)
         (‣ device-sleep              11 1)
         (‣ software-hold-reset       12 1)
         (‣ general-test-mode-enable  13 1)
         (‣ revision-control          14 3)
         (‣ power-down-io             17 1)
         (‣ sxoiref                   18 1)
         (‣ route-hold-to-pll-lock    19 1)
         (‣ reserved                  20 1)
         (‣ ti-test-status            21 4)
         (‣ ti-test-config            25 4)
         (‣ primary-ref-clk-present   29 1)
         (‣ secondary-ref-clk-present 30 1)
         (‣ reserved 31 1))))

(define (word->octets word)
  (list (extract-octet word 3)
        (extract-octet word 2)
        (extract-octet word 1)
        (extract-octet word 0)))

(define (octets->word lst)
  (unless (list? lst)
    (throw 'not-a-list lst))
  (cdr (fold (lambda (x state)
               (let ((idx (car state))
                     (value (cdr state)))
                 (cons (- idx 1) (put-octet value x idx))))
             (cons (length lst) 0)
             lst)))

(define (transceive! c ifc value)
  (octets->word (cr:spi-transceive! c ifc (word->octets value))))

(define (setup-spi c ifc)
  (apply cr:setup-spi!
         (cons* c ifc
                '((bit-order-msb-first? #t)
                  (cs-active-low?       #t)
                  (clock-idle-low?      #t)
                  (clock-phase-delay?   #f)
                  ;; The chip supports 20MHz at most, 10 should be safe.
                  ;; Also when using chip-remote, performance is not the
                  ;; chief concern.
                  (clock-rate       #e10e6)
                  ;; The frames to the chip have width of 32 bits. If we
                  ;; transmit four octets per chip-select cycle, that
                  ;; should work. And eight bit word width is a very
                  ;; commonly supported frame size in SPI controllers.
                  (frame-length          8)))))

(define (write-register c ifc p r v)
  ;; The registers are setup in a way such that transmitting them in a 32-bit
  ;; word will write their value to RAM. So there is nothing fancy to do here.
  (transceive! c ifc v))

(define-register ctrl-frame (items (list (‣ control 0  4)
                                         (‣ data    4 28))))

(define (cdce72010-read-bugfix r v)
  (set-bits v r 4 0))

(define *cdce72010-read*         #b1110)
(define *cdce72010-eeprom*       #b1111)
(define *cdce72010-eeprom-write*   #b01)
(define *cdce72010-eeprom-lock*    #b11)

(define (read-register c ifc p r)
  ;; Reading is done in two transmissions, first you send a control word with
  ;; the data set to the register you want to read, then you transmit whatever
  ;; you like and in return get the register's value. Note that the cdce72010
  ;; has a hardware bug that causes the LSB of the address that the chip re-
  ;; turns to be stuck to zero. We're working around this issue by setting the
  ;; address bits to the register we requested.
  (let ((ctrl (control-data-frame:encode ctrl-frame *cdce72010-read* r)))
    (transceive! c ifc ctrl))
  (cdce72010-read-bugfix r (transceive! c ifc 0)))

(define cdce72010-access
  (make-device-access #:setup setup-spi
                      #:read  read-register
                      #:write write-register))

(define (eeprom-cmd c ifc cmd)
  (let ((data (control-data-frame:encode ctrl-frame *cdce72010-eeprom* cmd)))
    (transceive! c ifc data)))

(define (cdce72010-write-eeprom c ifc)
  (eeprom-cmd c ifc *cdce72010-eeprom-write*))

(define (cdce72010-lock-eeprom c ifc)
  (eeprom-cmd c ifc *cdce72010-eeprom-lock*))

(define-device cdce72010
  (manufacturer texas-instruments)
  (homepage "http://www.ti.com/product/cdce72010")
  (datasheet "http://www.ti.com/lit/ds/symlink/cdce72010.pdf")
  (keywords '(clock distribution synchonisation pll jitter cleaner))
  (access cdce72010-access)
  (register-width 32)
  (page-map
   (pm→
    (table
     (↔ (#f (rm→
             (table
              (↔ (#x0 reg-x0) (#x1 reg-x1) (#x2 reg-x2) (#x3 reg-x3)
                 (#x4 reg-x4) (#x5 reg-x5) (#x6 reg-x6) (#x7 reg-x7)
                 (#x8 reg-x8) (#x9 reg-x9) (#xa reg-xa) (#xb reg-xb)
                 (#xc reg-xc))))))))))
