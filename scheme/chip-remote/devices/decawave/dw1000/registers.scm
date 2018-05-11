;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices decawave dw1000 registers)
  #:use-module (chip-remote item builder)
  #:use-module (chip-remote register)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote devices decawave dw1000 tables)
  #:export (reg:device-id
            reg:ieee-eui
            reg:pan-id/short-address
            reg:system-cfg
            reg:system-time
            reg:tx-frame-ctrl
            reg:tx-buffer
            reg:delayed-tx/rx-time
            reg:rx-frame-wait-timeout
            reg:system-ctrl
            reg:system-event-mask
            reg:system-status
            reg:rx-frame-info
            reg:rx-buffer
            reg:rx-frame-quality-info
            reg:rx-time-track-interval
            reg:rx-time-track-offset
            reg:rx-time-of-arrival
            reg:tx-time-of-sending
            reg:tx-antenna-delay
            reg:system-state
            reg:ack-time/response-time
            reg:rx-sniff-mode-cfg
            reg:tx-power-ctrl
            reg:channel-ctrl
            reg:user-sfd-sequences
            reg:agc-ctrl
            reg:external-sync-ctrl
            reg:accumulator-memory
            reg:gpio-ctrl
            reg:digital-rx-cfg
            reg:analog-rx-cfg
            reg:tx-calibration-cfg
            reg:frequency-synthesizer-ctrl
            reg:always-on-ctrl
            reg:otp-interface
            reg:leading-edge-detect-ctrl
            reg:digital-diagnostics
            reg:power-management-ctrl))

(define (octets n)
  (* n 8))

(define (offset byte bit)
  (+ (* byte 8) bit))

(define-register reg:device-id
  #:address #x00
  #:description "Device ID register, includes revision info (0xDECA0130)"
  #:register-width (octets 4)
  #:contents
  (revision 0 4)
  (version 4 4)
  (model 8 8)
  (register-identification 16 16))

(define-register reg:ieee-eui
  #:address #x01
  #:description "IEEE Extended Unique Identifier (63:0)"
  #:register-width (octets 8)
  #:contents
  (ieee-eui-device 0 40)
  (ieee-eui-manufacturer 40 24))

(define-register reg:pan-id/short-address
  #:address #x03
  #:description "PAN ID (31:16) and Short Address (15:0)"
  #:register-width (octets 4)
  #:contents
  (short-address 0 16)
  (pan-id 16 16))

(define-register reg:system-cfg
  #:address #x04
  #:description "System Configuration (31:0)"
  #:register-width (octets 4)
  #:contents
  (frame-filter-enable 0 1)
  (frame-filter-as-coordinator 1 1)
  (frame-filter-allow-beacons 2 1)
  (frame-filter-allow-data-frames 3 1)
  (frame-filter-allow-acks 4 1)
  (frame-filter-allow-mac-cmds 5 1)
  (frame-filter-allow-reserved 6 1)
  (frame-filter-allow-type-4 7 1)
  (frame-filter-allow-type-5 8 1)
  (host-interrupt-polarity 9 1
                           #:semantics lookup irq-polarity-map
                           #:default 'active-high)
  (spi-data-edge 10 1
                 #:semantics lookup spi-data-edge-map
                 #:default 'miso-at-sampling-edge)
  (frame-error-check 11 1
                     #:semantics boolean/active-low
                     #:default #t)
  (double-buffer-operation 12 1
                           #:semantics boolean/active-low
                           #:default #f)
  (rx-abort-on-phy-header-error 13 1
                                #:semantics boolean/active-low
                                #:default #t)
  (rx-abort-on-reed-solomon-error 14 1
                                  #:semantics boolean/active-low
                                  #:default #t)
  (standard-frame-check-seed 15 1
                             #:semantics boolean/active-low
                             #:default #t)
  (phy-header-mode 16 2
                   #:semantics lookup phy-header-mode-map
                   #:default 'standard-frame)
  (smart-tx-power-operation 18 1
                            #:semantics boolean/active-low
                            #:default #t)
  (reserved 19 3)
  (rx-110kbaud-mode 22 1)
  (reserved 23 5)
  (rx-wait-timeout-enable 28 1)
  (rx-auto-reenable 29 1)
  (auto-ack-enable 30 1)
  (auto-ack-pending-bit-default 31 1
                                #:semantics unsigned-integer
                                #:default 0))

(define-register reg:system-time
  #:address #x06
  #:description "System Time Counter (40-bit)"
  #:register-width (octets 5)
  #:contents (high-speed-time-stamp 0 40))

(define-register reg:tx-frame-ctrl
  #:address #x08
  #:description "Transmit Frame Control"
  #:register-width (octets 5)
  #:contents
  (tx-frame-length 0 10 #:default 10)
  (reserved 10 3)
  (tx-bit-rate 13 2
               #:semantics lookup bit-rate-map
               #:default '6.81MBit/s)
  (tx-ranging-enable-bit 15 1)
  (tx-pulse-repetition-frequency 16 2
                                 #:semantics lookup prf-map
                                 #:default '16MHz)
  (tx-preamble-symbol-repetition 18 4
                                 #:semantics lookup preamble-symbol-rep-map
                                 #:default 64)
  (tx-buffer-offset-index 22 10 #:validate range (>= 0) (<= 1023))
  (inter-frame-spacing 32 8))

(define-register reg:tx-buffer
  #:address #x09
  #:description "Transmit Data Buffer"
  #:register-width (octets 1024)
  #:contents (tx-buffer 0 (octets 1024)))

(define-register reg:delayed-tx/rx-time
  #:address #x0a
  #:description "Delayed Send or Receive Time (40-bit)"
  #:register-width (octets 5)
  #:contents (delay-time 0 (octets 5)))

(define-register reg:rx-frame-wait-timeout
  #:address #x0c
  #:description "Receive Frame Wait Timeout Period"
  #:register-width (octets 4)
  #:contents
  (wait-timeout 0 (octets 2))
  (reserved 16 (octets 2)))

(define-register reg:system-ctrl
  #:address #x0d
  #:description "System Control Register"
  #:register-width (octets 4)
  #:contents
  (tx-auto-frame-check 0 1 #:semantics boolean/active-low #:default #t)
  (tx-start 1 1)
  (tx-delayed-enable 2 1 #:default #f)
  (tx-cancel-auto-fcs-suppression 3 1 #:default #f)
  (reserved 4 2)
  (tx/rx-off-switch 6 1 #:default #f)
  (wait-for-response 7 1 #:default #f)
  (rx-enable 8 1 #:default #f)
  (rx-delayed-enable 9 1 #:default #f)
  (reserved 10 14)
  (host-side-rx-buffer-pointer-toggle 24 1
                                      #:semantics unsigned-integer
                                      #:default 0)
  (reserved 25 7))

(define-register reg:system-event-mask
  #:address #x0e
  #:description "System Event Mask Register"
  #:register-width (octets 4)
  #:contents
  (reserved 0 1)
  (mask-pll-lock-event 1 1)
  (mask-external-sync-clock-reset-event 2 1)
  (mask-automatic-ack-trigger-event 3 1)
  (mask-tx-frame-begin-event 4 1)
  (mask-tx-preamble-sent-event 5 1)
  (mask-tx-phy-header-sent-event 6 1)
  (mask-tx-frame-sent-event 7 1)
  (mask-rx-preamble-detect-event 8 1)
  (mask-rx-sfd-detect-event 9 1)
  (mask-lde-processing-done-event 10 1)
  (mask-rx-phy-header-detect-event 11 1)
  (mask-rx-phy-header-error-event 12 1)
  (mask-rx-data-frame-ready-event 13 1)
  (mask-rx-frame-check-good-event 14 1)
  (mask-rx-frame-check-fail-event 15 1)
  (mask-rx-reed-solomon-frame-sync-loss-event 16 1)
  (mask-rx-frame-wait-timeout-event 17 1)
  (mask-rx-lde-detection-error-event 18 1)
  (reserved 19 1)
  (mask-rx-overrun-event 20 1)
  (mask-rx-preamble-detection-timeout-event 21 1)
  (mask-gpio-event 22 1)
  (mask-sleep-to-init-event 23 1)
  (mask-rf-pll-lost-lock-event 24 1)
  (mask-clock-pll-lost-lock-event 25 1)
  (mask-rx-sfd-timeout-event 26 1)
  (mask-half-period-delay-warning-event 27 1)
  (mask-transmit-buffer-error-event 28 1)
  (mask-automatic-frame-filtering-event 29 1)
  (reserved 30 2))

(define-register reg:system-status
  #:address #x0f
  #:description "System event Status Register"
  #:register-width (octets 4)
  #:contents
  (irq-request! 0 1)
  (pll-lock-event 1 1)
  (external-sync-clock-reset-event 2 1)
  (automatic-ack-trigger-event 3 1)
  (tx-frame-begin-event 4 1)
  (tx-preamble-sent-event 5 1)
  (tx-phy-header-sent-event 6 1)
  (tx-frame-sent-event 7 1)
  (rx-preamble-detect-event 8 1)
  (rx-sfd-detect-event 9 1)
  (lde-processing-done-event 10 1)
  (rx-phy-header-detect-event 11 1)
  (rx-phy-header-error-event 12 1)
  (rx-data-frame-ready-event 13 1)
  (rx-frame-check-good-event 14 1)
  (rx-frame-check-fail-event 15 1)
  (rx-reed-solomon-frame-sync-loss-event 16 1)
  (rx-frame-wait-timeout-event 17 1)
  (rx-lde-detection-error-event 18 1)
  (reserved 19 1)
  (rx-overrun-event 20 1)
  (rx-preamble-detection-timeout-event 21 1)
  (gpio-event 22 1)
  (sleep-to-init-event 23 1)
  (rf-pll-lost-lock-event 24 1)
  (clock-pll-lost-lock-event 25 1)
  (rx-sfd-timeout-event 26 1)
  (half-period-delay-warning-event 27 1)
  (transmit-buffer-error-event 28 1)
  (automatic-frame-filtering-event 29 1)
  (host-side-receive-buffer-pointer 30 1)
  (ic-side-receive-buffer-pointer 31 1)
  (rx-reed-solomon-correction-status 32 1)
  (rx-preamble-rejection 33 1)
  (tx-power-up-time-error 34 1)
  (reserved 35 5))

(define-register reg:rx-frame-info
  #:address #x10
  #:description "RX Frame Information (in double buffer set)"
  #:register-width (octets 4)
  #:contents
  (rx-frame-length 0 10)
  (reserved 10 1)
  (rx-preamble-length-low 11 2)
  (rx-bit-rate 13 2 #:semantics lookup bit-rate-map)
  (rx-ranging-bit? 15 1)
  (rx-prf-report 16 2 #:semantics lookup prf-map)
  (rx-preamble-symbol-repetition 18 2)
  (rx-preamble-accumulation-count 20 12))

(define-register reg:rx-buffer
  #:address #x11
  #:description "Receive Data Buffer (in double buffer set)"
  #:register-width (octets 1024)
  #:contents (rx-buffer 0 (octets 1024)))

(define-register reg:rx-frame-quality-info
  #:address #x12
  #:description "Rx Frame Quality information (in double buffer set)"
  #:register-width (octets 8)
  #:contents
  (noise-standard-deviation 0 (octets 2))
  (first-path-amplitude-point-2 16 (octets 2))
  (first-path-amplitude-point-3 32 (octets 2))
  (channel-impulse-response-power 48 (octets 2)))

(define-register reg:rx-time-track-interval
  #:address #x13
  #:description "Receiver Time Tracking Interval (in double buffer set)"
  #:register-width (octets 4)
  #:contents (rx-time-tracking-interval 0 (octets 4)))

(define-register reg:rx-time-track-offset
  #:address #x14
  #:description "Receiver Time Tracking Offset (in double buffer set)"
  #:register-width (octets 5)
  #:contents
  (rx-time-tracking-offset 0 19 #:semantics twos-complement)
  (reserved 19 5)
  (internal-resampler-delay 24 8)
  (rx-carrier-phase-adjust 32 7)
  (reserved 39 1))

(define-register reg:rx-time-of-arrival
  #:address #x15
  #:description "Receive Message Time of Arrival (in double buffer set)"
  #:register-width (octets 14)
  #:contents
  (rx-time-stamp 0 (octets 5))
  (first-path-index 40 (octets 2))
  (first-path-amplitude-point-1 56 (octets 2))
  (rx-raw-frame-time-stamp 72 (octets 5)))

(define-register reg:tx-time-of-sending
  #:address #x17
  #:description "Transmit Message Time of Sending"
  #:register-width (octets 10)
  #:contents
  (tx-time-stamp 0 (octets 5))
  (tx-raw-frame-time-stamp 40 (octets 5)))

(define-register reg:tx-antenna-delay
  #:address #x18
  #:description "16-bit Delay from Transmit to Antenna"
  #:register-width (octets 2)
  #:contents (tx-antenna-delay 0 (octets 2)))

(define-register reg:system-state
  #:address #x19
  #:description "System State information READ ONLY"
  #:register-width (octets 5)
  #:contents (reserved 0 (octets 5)))

(define-register reg:ack-time/response-time
  #:address #x1a
  #:description "Acknowledgement Time and Response Time"
  #:register-width (octets 4)
  #:contents
  (wait-for-response-time 0 20)
  (reserved 20 4)
  (auto-ack-time 24 8))

(define-register reg:rx-sniff-mode-cfg
  #:address #x1d
  #:description "Sniff Mode Configuration"
  #:register-width (octets 4)
  #:contents
  (sniff-mode-on-time 0 4)
  (reserved 4 4)
  (sniff-mode-off-time 8 8)
  (reserved 16 16))

(define-register reg:tx-power-ctrl
  #:address #x1e
  #:description "TX Power Control"
  ;; TODO: So, this one needs to be dependently decoded. I have a vague idea on
  ;; how to do it, but I need to think about it some more before implementing
  ;; it all. Until then, lets just put the default here. This is the case when
  ;; "smart-power" is active.
  #:register-width (octets 4)
  #:contents
  ;; The semantics of these field are also particular. That's already easy to
  ;; implement in the current framework. I'm just lazy right now.
  (tx-normal-frame-power 0 (octets 1))
  (tx-500mu-frame-power 8 (octets 1))
  (tx-250mu-frame-power 16 (octets 1))
  (tx-125mu-frame-power 24 (octets 1)))

(define-register reg:channel-ctrl
  #:address #x1f
  #:description "Channel Control"
  #:register-width (octets 4)
  #:contents
  (tx-channel 0 4)
  (rx-channel 4 4)
  (reserved 8 9)
  (use-decawave-sfd-sequence 17 1)
  (rx-pulse-repetition-frequency 18 2
                                 #:semantics lookup prf-map
                                 #:default '16MHz)
  (use-user-tx-sfd-sequence 20 1)
  (use-user-rx-sfd-sequence 21 1)
  (tx-preamble-code 22 5)
  (rx-preamble-code 27 5))

(define-register reg:user-sfd-sequences
  #:address #x21
  #:description "User-specified short/long TX/RX SFD sequences"
  #:register-width (octets 41)
  ;; TODO: Not messing with this for now.
  #:contents
  (user-sfd-memory 0 (octets 41)))

(define-register reg:agc-ctrl
  #:address #x23
  #:description "Automatic Gain Control configuration"
  #:register-width (octets 32)
  ;; TODO: Also not touching these...
  #:contents
  (agc-ctrl-memory 0 (octets 32)))

(define-register reg:external-sync-ctrl
  #:address #x24
  #:description "External synchronisation control"
  #:register-width (octets 12)
  #:contents
  ;; Another one, I don't care about for now.
  (external-sync-ctrl-memory 0 (octets 12)))

(define-register reg:accumulator-memory
  #:address #x25
  #:description "Read access to accumulator data"
  ;; This one is also interesting: You need to be particular about how you read
  ;; this part of memory. But other than that it's really just a large ass
  ;; register.
  #:register-width (octets 4064)
  #:contents (accumulator-memory 0 (octets 4064)))

(define-register reg:gpio-ctrl
  #:address #x26
  #:description "Peripheral register bus 1 access - GPIO control"
  #:register-width (octets 44)
  #:contents
  (reserved (offset 0 0) 6)
  (mode-select-gpio-0 (offset 0 6) 2
                      #:semantics lookup gpio-0-modes
                      #:default 'gpio)
  (mode-select-gpio-1 (offset 0 8) 2
                      #:semantics lookup gpio-1-modes
                      #:default 'gpio)
  (mode-select-gpio-2 (offset 0 10) 2
                      #:semantics lookup gpio-2-modes
                      #:default 'gpio)
  (mode-select-gpio-3 (offset 0 12) 2
                      #:semantics lookup gpio-3-modes
                      #:default 'gpio)
  (mode-select-gpio-4 (offset 0 14) 2
                      #:semantics lookup gpio-4-modes
                      #:default 'gpio)
  (mode-select-gpio-5 (offset 0 16) 2
                      #:semantics lookup gpio-5-modes
                      #:default 'gpio)
  (mode-select-gpio-6 (offset 0 18) 2
                      #:semantics lookup gpio-6-modes
                      #:default 'gpio)
  (mode-select-gpio-7 (offset 0 20) 2
                      #:semantics lookup gpio-7-modes
                      #:default 'sync-input)
  (mode-select-gpio-8 (offset 0 22) 2
                      #:semantics lookup gpio-8-modes
                      #:default 'irq-output)
  (reserved (offset 0 24) (octets 1))
  (reserved (offset #x04 0) (octets 4))
  (gpio-0-direction (offset #x08 0) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (gpio-1-direction (offset #x08 1) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (gpio-2-direction (offset #x08 2) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (gpio-3-direction (offset #x08 3) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (gpio-0-direction-mask (offset #x08 4) 1 #:default #f)
  (gpio-1-direction-mask (offset #x08 5) 1 #:default #f)
  (gpio-2-direction-mask (offset #x08 6) 1 #:default #f)
  (gpio-3-direction-mask (offset #x08 7) 1 #:default #f)
  (gpio-4-direction (offset #x08 8) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (gpio-5-direction (offset #x08 9) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (gpio-6-direction (offset #x08 10) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (gpio-7-direction (offset #x08 11) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (gpio-4-direction-mask (offset #x08 12) 1 #:default #f)
  (gpio-5-direction-mask (offset #x08 13) 1 #:default #f)
  (gpio-6-direction-mask (offset #x08 14) 1 #:default #f)
  (gpio-7-direction-mask (offset #x08 15) 1 #:default #f)
  (gpio-8-direction (offset #x08 16) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (reserved (offset #x08 17) 3)
  (gpio-8-direction-mask (offset #x08 20) 1 #:default #f)
  (reserved (offset #x08 21) 11)
  (gpio-0-output-value (offset #x0c 0) 1 #:default #f)
  (gpio-1-output-value (offset #x0c 1) 1 #:default #f)
  (gpio-2-output-value (offset #x0c 2) 1 #:default #f)
  (gpio-3-output-value (offset #x0c 3) 1 #:default #f)
  (gpio-0-output-mask (offset #x0c 4) 1 #:default #f)
  (gpio-1-output-mask (offset #x0c 5) 1 #:default #f)
  (gpio-2-output-mask (offset #x0c 6) 1 #:default #f)
  (gpio-3-output-mask (offset #x0c 7) 1 #:default #f)
  (gpio-4-output-value (offset #x0c 8) 1 #:default #f)
  (gpio-5-output-value (offset #x0c 9) 1 #:default #f)
  (gpio-6-output-value (offset #x0c 10) 1 #:default #f)
  (gpio-7-output-value (offset #x0c 11) 1 #:default #f)
  (gpio-4-output-mask (offset #x0c 12) 1 #:default #f)
  (gpio-5-output-mask (offset #x0c 13) 1 #:default #f)
  (gpio-6-output-mask (offset #x0c 14) 1 #:default #f)
  (gpio-7-output-mask (offset #x0c 15) 1 #:default #f)
  (gpio-8-output-value (offset #x0c 16) 1 #:default #f)
  (reserved (offset #x0c 17) 3)
  (gpio-8-output-mask (offset #x0c 20) 1 #:default #f)
  (reserved (offset #x0c 21) 11)
  (gpio-0-irq-enable (offset #x10 0) 1 #:default #f)
  (gpio-1-irq-enable (offset #x10 1) 1 #:default #f)
  (gpio-2-irq-enable (offset #x10 2) 1 #:default #f)
  (gpio-3-irq-enable (offset #x10 3) 1 #:default #f)
  (gpio-4-irq-enable (offset #x10 4) 1 #:default #f)
  (gpio-5-irq-enable (offset #x10 5) 1 #:default #f)
  (gpio-6-irq-enable (offset #x10 6) 1 #:default #f)
  (gpio-7-irq-enable (offset #x10 7) 1 #:default #f)
  (gpio-8-irq-enable (offset #x10 8) 1 #:default #f)
  (reserved (offset #x10 9) 23)
  (gpio-0-irq-sense-mode (offset #x14 0) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-1-irq-sense-mode (offset #x14 1) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-2-irq-sense-mode (offset #x14 2) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-3-irq-sense-mode (offset #x14 3) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-4-irq-sense-mode (offset #x14 4) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-5-irq-sense-mode (offset #x14 5) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-6-irq-sense-mode (offset #x14 6) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-7-irq-sense-mode (offset #x14 7) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-8-irq-sense-mode (offset #x14 8) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (reserved (offset #x14 9) 23)
  (gpio-0-irq-mode (offset #x18 0) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-1-irq-mode (offset #x18 1) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-2-irq-mode (offset #x18 2) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-3-irq-mode (offset #x18 3) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-4-irq-mode (offset #x18 4) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-5-irq-mode (offset #x18 5) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-6-irq-mode (offset #x18 6) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-7-irq-mode (offset #x18 7) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-8-irq-mode (offset #x18 8) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (reserved (offset #x18 9) 23)
  (gpio-0-irq-edge-mode (offset #x1c 0) 1
                        #:semantics lookup gpio-irq-both-map
                        #:default 'normal)
  (gpio-1-irq-edge-mode (offset #x1c 1) 1
                        #:semantics lookup gpio-irq-both-map
                        #:default 'normal)
  (gpio-2-irq-edge-mode (offset #x1c 2) 1
                        #:semantics lookup gpio-irq-both-map
                        #:default 'normal)
  (gpio-3-irq-edge-mode (offset #x1c 3) 1
                        #:semantics lookup gpio-irq-both-map
                        #:default 'normal)
  (gpio-4-irq-edge-mode (offset #x1c 4) 1
                        #:semantics lookup gpio-irq-both-map
                        #:default 'normal)
  (gpio-5-irq-edge-mode (offset #x1c 5) 1
                        #:semantics lookup gpio-irq-both-map
                        #:default 'normal)
  (gpio-6-irq-edge-mode (offset #x1c 6) 1
                        #:semantics lookup gpio-irq-both-map
                        #:default 'normal)
  (gpio-7-irq-edge-mode (offset #x1c 7) 1
                        #:semantics lookup gpio-irq-both-map
                        #:default 'normal)
  (gpio-8-irq-edge-mode (offset #x1c 8) 1
                        #:semantics lookup gpio-irq-both-map
                        #:default 'normal)
  (reserved (offset #x1c 9) 23)
  (gpio-0-clear-irq-flag (offset #x20 0) 1 #:default #f)
  (gpio-1-clear-irq-flag (offset #x20 1) 1 #:default #f)
  (gpio-2-clear-irq-flag (offset #x20 2) 1 #:default #f)
  (gpio-3-clear-irq-flag (offset #x20 3) 1 #:default #f)
  (gpio-4-clear-irq-flag (offset #x20 4) 1 #:default #f)
  (gpio-5-clear-irq-flag (offset #x20 5) 1 #:default #f)
  (gpio-6-clear-irq-flag (offset #x20 6) 1 #:default #f)
  (gpio-7-clear-irq-flag (offset #x20 7) 1 #:default #f)
  (gpio-8-clear-irq-flag (offset #x20 8) 1 #:default #f)
  (reserved (offset #x20 9) 23)
  (gpio-0-irq-debounce-enable (offset #x24 0) 1
                              #:semantics boolean/active-low
                              #:default #f)
  (gpio-1-irq-debounce-enable (offset #x24 1) 1
                              #:semantics boolean/active-low
                              #:default #f)
  (gpio-2-irq-debounce-enable (offset #x24 2) 1
                              #:semantics boolean/active-low
                              #:default #f)
  (gpio-3-irq-debounce-enable (offset #x24 3) 1
                              #:semantics boolean/active-low
                              #:default #f)
  (gpio-4-irq-debounce-enable (offset #x24 4) 1
                              #:semantics boolean/active-low
                              #:default #f)
  (gpio-5-irq-debounce-enable (offset #x24 5) 1
                              #:semantics boolean/active-low
                              #:default #f)
  (gpio-6-irq-debounce-enable (offset #x24 6) 1
                              #:semantics boolean/active-low
                              #:default #f)
  (gpio-7-irq-debounce-enable (offset #x24 7) 1
                              #:semantics boolean/active-low
                              #:default #f)
  (gpio-8-irq-debounce-enable (offset #x24 8) 1
                              #:semantics boolean/active-low
                              #:default #f)
  (reserved (offset #x24 9) 23)
  (gpio-0-raw-io-state (offset #x28 0) 1 #:default #f)
  (gpio-1-raw-io-state (offset #x28 1) 1 #:default #f)
  (gpio-2-raw-io-state (offset #x28 2) 1 #:default #f)
  (gpio-3-raw-io-state (offset #x28 3) 1 #:default #f)
  (gpio-4-raw-io-state (offset #x28 4) 1 #:default #f)
  (gpio-5-raw-io-state (offset #x28 5) 1 #:default #f)
  (gpio-6-raw-io-state (offset #x28 6) 1 #:default #f)
  (gpio-7-raw-io-state (offset #x28 7) 1 #:default #f)
  (gpio-8-raw-io-state (offset #x28 8) 1 #:default #f)
  (reserved (offset #x28 9) 23))

(define-register reg:digital-rx-cfg
  #:address #x27
  #:description "Digital Receiver configuration"
  #:register-width (octets 46)
  ;; TODO: Again, not concerned with these right now.
  #:contents (digital-rx-cfg 0 (octets 46)))

(define-register reg:analog-rx-cfg
  #:address #x28
  #:description "Analog RF Configuration"
  #:register-width (octets 53)
  ;; TODO: And again...
  #:contents (analog-rx-cfg 0 (octets 53)))

(define-register reg:tx-calibration-cfg
  #:address #x2a
  #:description "Transmitter calibration block"
  #:register-width (octets 13)
  ;; TODO: And again...
  #:contents (tx-calibration-cfg 0 (octets 13)))

(define-register reg:frequency-synthesizer-ctrl
  #:address #x2b
  #:description "Frequency synthesiser control block"
  #:register-width (octets 21)
  ;; TODO: And again...
  #:contents (frequency-synthesizer-ctrl 0 (octets 21)))

(define-register reg:always-on-ctrl
  #:address #x2c
  #:description "Always-On register set"
  #:register-width (octets 12)
  ;; TODO: And again...
  #:contents (always-on-ctrl 0 (octets 12)))

(define-register reg:otp-interface
  #:address #x2d
  #:description "One Time Programmable Memory Interface"
  #:register-width (octets 13)
  ;; TODO: And again...
  #:contents (otp-interface 0 (octets 13)))

(define-register reg:leading-edge-detect-ctrl
  #:address #x2e
  #:description "Leading edge detection control block"
  ;; TODO: And again...
  #:register-width (octets 10246)
  #:contents (leading-edge-detect-ctrl 0 (octets 10246)))

(define-register reg:digital-diagnostics
  #:address #x2f
  #:description "Digital Diagnostics Interface"
  #:register-width (octets 38)
  #:contents
  (event-counter-enable (offset #x00 0) 1)
  (event-counter-clear (offset #x00 1) 1)
  (reserved (offset #x00 2) 30)
  (cnt-physical-header-error (offset #x04 0) 12)
  (reserved (offset #x04 12) 4)
  (cnt-frame-sync-lost (offset #x06 0) 12)
  (reserved (offset #x06 12) 4)
  (cnt-frame-checksum-good (offset #x08 0) 12)
  (reserved (offset #x08 12) 4)
  (cnt-frame-checksum-error (offset #x0a 0) 12)
  (reserved (offset #x0a 12) 4)
  (cnt-frame-filter-rejection (offset #x0c 0) 12)
  (reserved (offset #x0c 12) 4)
  (cnt-rx-overrun (offset #x0e 0) 12)
  (reserved (offset #x0e 12) 4)
  (cnt-sfd-timeout (offset #x10 0) 12)
  (reserved (offset #x10 12) 4)
  (cnt-preamble-detection-timeout (offset #x12 0) 12)
  (reserved (offset #x12 12) 4)
  (cnt-rx-frame-wait-timeout (offset #x14 0) 12)
  (reserved (offset #x14 12) 4)
  (cnt-tx-frame-sent (offset #x16 0) 12)
  (reserved (offset #x16 12) 4)
  (cnt-half-period-warning (offset #x18 0) 12)
  (reserved (offset #x18 12) 4)
  (cnt-tx-power-up-warning (offset #x1a 0) 12)
  (reserved (offset #x1a 12) 4)
  (reserved (offset #x1c 0) (octets 8))
  (reserved (offset #x24 0) 4)
  (tx-power-spectrum-test-mode-enable (offset #x24 4) 1)
  (reserved (offset #x24 5) 11))

(define-register reg:power-management-ctrl
  #:address #x36
  #:description "Power Management System Control Block"
  #:register-width (octets 44)
  #:contents
  (system-clock-select 0 2
                       #:semantics lookup system-clock-map
                       #:default 'auto)
  (rx-clock-select 2 2
                   #:semantics lookup system-clock-map
                   #:default 'auto)
  (tx-clock-select 4 2
                   #:semantics lookup system-clock-map
                   #:default 'auto)
  (force-accumulator-clock-enable 6 1 #:default #f)
  (reserved 7 3 #:default #b100)
  (adc-convert-clock-enable 10 1 #:default #f)
  (reserved 11 4)
  (accumulator-memory-clock-enable 15 1 #:default #f)
  (gpio-clock-enable 16 1 #:default #f)
  (gpio-reset 17 1 #:semantics boolean/active-low #:default #t)
  (gpio-debounce-clock-enable 18 1 #:default #f)
  (gpio-debounce-reset 19 1 #:semantics boolean/active-low #:default #t)
  (reserved 20 3 #:default #b011)
  (kilohertz-clock-enable 23 1 #:default #f)
  (reserved 24 4)
  (soft-reset 28 4 #:default #b1111)
  ;; offset 0x04
  (reserved (offset #x04 0) 1)
  (auto-rx-to-init (offset #x04 1) 1 #:default #f)
  (reserved (offset #x04 2) 1)
  (packet-sequence (offset #x04 3) 8 #:default #xe7)
  (auto-tx-to-sleep (offset #x04 11) 1 #:default #f)
  (auto-rx-to-sleep (offset #x04 12) 1 #:default #f)
  (snooze-enable (offset #x04 13) 1 #:default #f)
  (snooze-repeat-enable (offset #x04 14) 1 #:default #f)
  (pll-sync-clock-enable (offset #x04 15) 1 #:default #f)
  (reserved (offset #x04 16) 1)
  (lde-run-enable (offset #x04 17) 1 #:default #f)
  (reserved (offset #x04 18) 8 #:default #b01000000)
  (kilohertz-clock-divider (offset #x04 26) 6 #:default #x100000)
  ;; offset 0x08
  (reserved (offset #x08 0) (octets 4))
  ;; offset 0x0c
  (snooze-time (offset #x0c 0) (octets 1) #:default #b01000000)
  (reserved (offset #x0c (octets 1)) (octets 3))
  ;; offset 0x10
  (reserved (offset #x10 0) (octets 22))
  ;; offset 0x26
  (tx-fine-grain-power-sequencing (offset #x26 0) (octets 2)
                                  #:default #b0000101100111100)
  ;; offset 0x28
  (led-blink-time (offset #x28 0) (octets 1))
  (led-blink-enable (offset #x28 8) 1)
  (reserved (offset #x28 9) 7)
  (led-blink-now-mask (offset #x28 16) 4)
  (reserved (offset #x28 20) 12))
