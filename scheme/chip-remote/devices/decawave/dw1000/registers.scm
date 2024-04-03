;; Copyright (c) 2018-2024 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices decawave dw1000 registers)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote item)
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

(define double-buffered-registers
  '((reg:rx-frame-info . 16)
    (reg:rx-buffer . 17)
    (reg:rx-frame-quality-info . 18)
    (reg:rx-time-track-interval . 19)
    (reg:rx-time-track-offset . 20)
    (reg:rx-time-of-arrival . 21)))

(define (octets n) (* n 8))

(define (offset byte bit) (+ (* byte 8) bit))

(define reg:device-id
  (register
   (name 'device-id)
   (address 0)
   (description "Device ID register")
   (width (octets 4))
   (items
    (list (‣ revision 0 4)
          (‣ version 4 4)
          (‣ model 8 8)
          (‣ register-identification 16 16)))))

(define reg:ieee-eui
  (register
   (name 'ieee-eui)
   (address 1)
   (description "IEEE Extended Unique Identifier")
   (width (octets 8))
   (items
    (list (‣ ieee-eui-device 0 40)
          (‣ ieee-eui-manufacturer 40 24)))))

(define reg:pan-id/short-address
  (register
   (name 'pan-id/short-address)
   (address 3)
   (description "PAN ID and Short Address")
   (width (octets 4))
   (items
    (list (‣ short-address 0 16)
          (‣ pan-id 16 16)))))

(define reg:system-cfg
  (register
   (name 'system-cfg)
   (address 4)
   (description "System Configuration")
   (width (octets 4))
   (items
    (list (‣ frame-filter-enable 0 1)
          (‣ frame-filter-as-coordinator 1 1)
          (‣ frame-filter-allow-beacons 2 1)
          (‣ frame-filter-allow-data-frames 3 1)
          (‣ frame-filter-allow-acks 4 1)
          (‣ frame-filter-allow-mac-cmds 5 1)
          (‣ frame-filter-allow-reserved 6 1)
          (‣ frame-filter-allow-type-4 7 1)
          (‣ frame-filter-allow-type-5 8 1)
          (‣ host-interrupt-polarity 9 1
                                     (semantics (tbl irq-polarity-map
                                                     #:default 'active-high)))
          (‣ spi-data-edge 10 1 (semantics
                                 (tbl spi-data-edge-map
                                      #:default 'miso-at-sampling-edge)))
          (‣ frame-error-check 11 1
                               (semantics boolean/active-low)
                               (default #t))
          (‣ double-buffer-operation 12 1
                                     (semantics boolean/active-low)
                                     (default #f))
          (‣ rx-abort-on-phy-header-error 13 1
                                          (semantics boolean/active-low)
                                          (default #t))
          (‣ rx-abort-on-reed-solomon-error 14 1
                                            (semantics boolean/active-low)
                                            (default #t))
          (‣ standard-frame-check-seed 15 1
                                       (semantics boolean/active-low)
                                       (default #t))
          (‣ phy-header-mode 16 2
                             (semantics
                              (tbl phy-header-mode-map
                                   #:default 'standard-frame)))
          (‣ smart-tx-power-operation 18 1
                                      (semantics boolean/active-low)
                                      (default #t))
          (‣ reserved 19 3)
          (‣ rx-110kbaud-mode 22 1)
          (‣ reserved 23 5)
          (‣ rx-wait-timeout-enable 28 1)
          (‣ rx-auto-reenable 29 1)
          (‣ auto-ack-enable 30 1)
          (‣ auto-ack-pending-bit-default 31 1
                                          (semantics unsigned-integer)
                                          (default 0))))))

(define reg:system-time
  (register
   (name 'system-time)
   (address 6)
   (description "System Time Counter")
   (width (octets 5))
   (items
    (list (‣ high-speed-time-stamp 0 40)))))

(define reg:tx-frame-ctrl
  (register
   (name 'tx-frame-ctrl)
   (address 8)
   (description "Transmit Frame Control")
   (width (octets 5))
   (items
    (list (‣ tx-frame-length 0 10 (default 10))
          (‣ reserved 10 3)
          (‣ tx-bit-rate 13 2 (semantics (tbl bit-rate-map
                                              #:default '#{6.81MBit/s}#)))
          (‣ tx-ranging-enable-bit 15 1)
          (‣ tx-pulse-repetition-frequency
             16 2 (semantics (tbl prf-map #:default '#{16MHz}#)))
          (‣ tx-preamble-symbol-repetition
             18 4 (semantics (tbl preamble-symbol-rep-map #:default 64)))
          (‣ tx-buffer-offset-index
             22 10 (semantics
                    (semantics
                     (inherit unsigned-integer)
                     (range (lambda (s w) '(range 0 1023))))))
          (‣ inter-frame-spacing 32 8)))))

(define reg:tx-buffer
  (register
   (name 'tx-buffer)
   (address 9)
   (description "Transmit Data Buffer")
   (width (octets 1024))
   (items
    (list (‣ tx-buffer 0 (octets 1024))))))

(define reg:delayed-tx/rx-time
  (register
   (name 'delayed-tx/rx-time)
   (address 10)
   (description "Delayed Send or Receive Time")
   (width (octets 5))
   (items
    (list (‣ delay-time 0 (octets 5))))))

(define reg:rx-frame-wait-timeout
  (register
   (name 'rx-frame-wait-timeout)
   (address 12)
   (description "Receive Frame Wait Timeout Period")
   (width (octets 4))
   (items
    (list (‣ wait-timeout 0 (octets 2))
          (‣ reserved 16 (octets 2))))))

(define reg:system-ctrl
  (register
   (name 'system-ctrl)
   (address 13)
   (description "System Control Register")
   (width (octets 4))
   (items
    (list (‣ tx-auto-frame-check 0 1
                                 (semantics boolean/active-low)
                                 (default #t))
          (‣ tx-start 1 1)
          (‣ tx-delayed-enable 2 1 (default #f))
          (‣ tx-cancel-auto-fcs-suppression 3 1 (default #f))
          (‣ reserved 4 2)
          (‣ tx/rx-off-switch 6 1 (default #f))
          (‣ wait-for-response 7 1 (default #f))
          (‣ rx-enable 8 1 (default #f))
          (‣ rx-delayed-enable 9 1 (default #f))
          (‣ reserved 10 14)
          (‣ host-side-rx-buffer-pointer-toggle 24 1
                                                (semantics unsigned-integer)
                                                (default 0))
          (‣ reserved 25 7)))))

(define reg:system-event-mask
  (register
   (name 'system-event-mask)
   (address 14)
   (description "System Event Mask Register")
   (width (octets 4))
   (items
    (list (‣ reserved 0 1)
          (‣ mask-pll-lock-event 1 1)
          (‣ mask-external-sync-clock-reset-event 2 1)
          (‣ mask-automatic-ack-trigger-event 3 1)
          (‣ mask-tx-frame-begin-event 4 1)
          (‣ mask-tx-preamble-sent-event 5 1)
          (‣ mask-tx-phy-header-sent-event 6 1)
          (‣ mask-tx-frame-sent-event 7 1)
          (‣ mask-rx-preamble-detect-event 8 1)
          (‣ mask-rx-sfd-detect-event 9 1)
          (‣ mask-lde-processing-done-event 10 1)
          (‣ mask-rx-phy-header-detect-event 11 1)
          (‣ mask-rx-phy-header-error-event 12 1)
          (‣ mask-rx-data-frame-ready-event 13 1)
          (‣ mask-rx-frame-check-good-event 14 1)
          (‣ mask-rx-frame-check-fail-event 15 1)
          (‣ mask-rx-reed-solomon-frame-sync-loss-event 16 1)
          (‣ mask-rx-frame-wait-timeout-event 17 1)
          (‣ mask-rx-lde-detection-error-event 18 1)
          (‣ reserved 19 1)
          (‣ mask-rx-overrun-event 20 1)
          (‣ mask-rx-preamble-detection-timeout-event 21 1)
          (‣ mask-gpio-event 22 1)
          (‣ mask-sleep-to-init-event 23 1)
          (‣ mask-rf-pll-lost-lock-event 24 1)
          (‣ mask-clock-pll-lost-lock-event 25 1)
          (‣ mask-rx-sfd-timeout-event 26 1)
          (‣ mask-half-period-delay-warning-event 27 1)
          (‣ mask-transmit-buffer-error-event 28 1)
          (‣ mask-automatic-frame-filtering-event 29 1)
          (‣ reserved 30 2)))))

(define reg:system-status
  (register
   (name 'system-status)
   (address 15)
   (description "System event Status Register")
   (width (octets 5))
   (items
    (list (‣ irq-request! 0 1)
          (‣ pll-lock-event 1 1)
          (‣ external-sync-clock-reset-event 2 1)
          (‣ automatic-ack-trigger-event 3 1)
          (‣ tx-frame-begin-event 4 1)
          (‣ tx-preamble-sent-event 5 1)
          (‣ tx-phy-header-sent-event 6 1)
          (‣ tx-frame-sent-event 7 1)
          (‣ rx-preamble-detect-event 8 1)
          (‣ rx-sfd-detect-event 9 1)
          (‣ lde-processing-done-event 10 1)
          (‣ rx-phy-header-detect-event 11 1)
          (‣ rx-phy-header-error-event 12 1)
          (‣ rx-data-frame-ready-event 13 1)
          (‣ rx-frame-check-good-event 14 1)
          (‣ rx-frame-check-fail-event 15 1)
          (‣ rx-reed-solomon-frame-sync-loss-event 16 1)
          (‣ rx-frame-wait-timeout-event 17 1)
          (‣ rx-lde-detection-error-event 18 1)
          (‣ reserved 19 1)
          (‣ rx-overrun-event 20 1)
          (‣ rx-preamble-detection-timeout-event 21 1)
          (‣ gpio-event 22 1)
          (‣ sleep-to-init-event 23 1)
          (‣ rf-pll-lost-lock-event 24 1)
          (‣ clock-pll-lost-lock-event 25 1)
          (‣ rx-sfd-timeout-event 26 1)
          (‣ half-period-delay-warning-event 27 1)
          (‣ transmit-buffer-error-event 28 1)
          (‣ automatic-frame-filtering-event 29 1)
          (‣ host-side-receive-buffer-pointer 30 1)
          (‣ ic-side-receive-buffer-pointer 31 1)
          (‣ rx-reed-solomon-correction-status 32 1)
          (‣ rx-preamble-rejection 33 1)
          (‣ tx-power-up-time-error 34 1)
          (‣ reserved 35 5)))))

(define reg:rx-frame-info
  (register
   (name 'rx-frame-info)
   (address 16)
   (description "RX Frame Information")
   (width (octets 4))
   (items
    (list (‣ rx-frame-length 0 10)
          (‣ reserved 10 1)
          (‣ rx-preamble-length-low 11 2)
          (‣ rx-bit-rate 13 2 (semantics (tbl bit-rate-map)))
          (‣ rx-ranging-bit? 15 1)
          (‣ rx-prf-report 16 2 (semantics (tbl prf-map)))
          (‣ rx-preamble-length-high 18 2)
          (‣ rx-preamble-accumulation-count 20 12)))))

(define reg:rx-buffer
  (register
   (name 'rx-buffer)
   (address 17)
   (description "Receive Data Buffer")
   (width (octets 1024))
   (items
    (list (‣ rx-buffer 0 (octets 1024))))))

(define reg:rx-frame-quality-info
  (register
   (name 'rx-frame-quality-info)
   (address 18)
   (description "Rx Frame Quality information")
   (width (octets 8))
   (items
    (list (‣ noise-standard-deviation 0 (octets 2))
          (‣ first-path-amplitude-point-2 16 (octets 2))
          (‣ first-path-amplitude-point-3 32 (octets 2))
          (‣ channel-impulse-response-power 48 (octets 2))))))

(define reg:rx-time-track-interval
  (register
   (name 'rx-time-track-interval)
   (address 19)
   (description "Receiver Time Tracking Interval")
   (width (octets 4))
   (items
    (list (‣ rx-time-tracking-interval 0 (octets 4))))))

(define reg:rx-time-track-offset
  (register
   (name 'rx-time-track-offset)
   (address 20)
   (description "Receiver Time Tracking Offset")
   (width (octets 5))
   (items
    (list (‣ rx-time-tracking-offset 0 19 (semantics twos-complement))
          (‣ reserved 19 5)
          (‣ internal-resampler-delay 24 8)
          (‣ rx-carrier-phase-adjust 32 7)
          (‣ reserved 39 1)))))

(define reg:rx-time-of-arrival
  (register
   (name 'rx-time-of-arrival)
   (address 21)
   (description "Receive Message Time of Arrival")
   (width (octets 14))
   (items
    (list (‣ rx-time-stamp 0 (octets 5))
          (‣ first-path-index 40 (octets 2))
          (‣ first-path-amplitude-point-1 56 (octets 2))
          (‣ rx-raw-frame-time-stamp 72 (octets 5))))))

(define reg:tx-time-of-sending
  (register
   (name 'tx-time-of-sending)
   (address 23)
   (description "Transmit Message Time of Sending")
   (width (octets 10))
   (items
    (list (‣ tx-time-stamp 0 (octets 5))
          (‣ tx-raw-frame-time-stamp 40 (octets 5))))))

(define reg:tx-antenna-delay
  (register
   (name 'tx-antenna-delay)
   (address 24)
   (description "16-bit Delay from Transmit to Antenna")
   (width (octets 2))
   (items
    (list (‣ tx-antenna-delay 0 (octets 2))))))

(define reg:system-state
  (register
   (name 'system-state)
   (address 25)
   (description "System State information")
   (width (octets 5))
   (items
    (list (‣ reserved 0 (octets 5))))))

(define reg:ack-time/response-time
  (register
   (name 'ack-time/response-time)
   (address 26)
   (description "Acknowledgement Time and Response Time")
   (width (octets 4))
   (items
    (list (‣ wait-for-response-time 0 20)
          (‣ reserved 20 4)
          (‣ auto-ack-time 24 8)))))

(define reg:rx-sniff-mode-cfg
  (register
   (name 'rx-sniff-mode-cfg)
   (address 29)
   (description "Sniff Mode Configuration")
   (width (octets 4))
   (items
    (list (‣ sniff-mode-on-time 0 4)
          (‣ reserved 4 4)
          (‣ sniff-mode-off-time 8 8)
          (‣ reserved 16 16)))))

(define reg:tx-power-ctrl
  (register
   (name 'tx-power-ctrl)
   (address 30)
   (description "TX Power Control")
   (width (octets 4))
   (items
    (list (‣ tx-normal-frame-power 0 (octets 1))
          (‣ tx-500mu-frame-power 8 (octets 1))
          (‣ tx-250mu-frame-power 16 (octets 1))
          (‣ tx-125mu-frame-power 24 (octets 1))))))

(define reg:channel-ctrl
  (register
   (name 'channel-ctrl)
   (address 31)
   (description "Channel Control")
   (width (octets 4))
   (items
    (list (‣ tx-channel 0 4)
          (‣ rx-channel 4 4)
          (‣ reserved 8 9)
          (‣ use-decawave-sfd-sequence 17 1)
          (‣ rx-pulse-repetition-frequency
             18 2 (semantics (tbl prf-map #:default '#{16MHz}#)))
          (‣ use-user-tx-sfd-sequence 20 1)
          (‣ use-user-rx-sfd-sequence 21 1)
          (‣ tx-preamble-code 22 5)
          (‣ rx-preamble-code 27 5)))))

(define reg:user-sfd-sequences
  (register
   (name 'user-sfd-sequences)
   (address 33)
   (description "User-specified short/long TX/RX SFD sequences")
   (width (octets 41))
   (items
    (list (‣ user-sfd-memory 0 (octets 41))))))

(define reg:agc-ctrl
  (register
   (name 'agc-ctrl)
   (address 35)
   (description "Automatic Gain Control configuration")
   (width (octets 32))
   (items
    (list (‣ agc-ctrl-memory 0 (octets 32))))))

(define reg:external-sync-ctrl
  (register
   (name 'external-sync-ctrl)
   (address 36)
   (description "External synchronisation control")
   (width (octets 12))
   (items
    (list (‣ external-sync-ctrl-memory 0 (octets 12))))))

(define reg:accumulator-memory
  (register
   (name 'accumulator-memory)
   (address 37)
   (description "Read access to accumulator data")
   (width (octets 4064))
   (items
    (list (‣ accumulator-memory 0 (octets 4064))))))

(define reg:gpio-ctrl
  (register
   (name 'gpio-ctrl)
   (address 38)
   (description "GPIO control")
   (width (octets 44))
   (items
    (list (‣ reserved (offset 0 0) 6)
          (‣ mode-select-gpio-0 (offset 0 6) 2
                                (semantics (tbl gpio-0-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-1 (offset 0 8) 2
                                (semantics (tbl gpio-1-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-2 (offset 0 10) 2
                                (semantics (tbl gpio-2-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-3 (offset 0 12) 2
                                (semantics (tbl gpio-3-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-4 (offset 0 14) 2
                                (semantics (tbl gpio-4-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-5 (offset 0 16) 2
                                (semantics (tbl gpio-5-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-6 (offset 0 18) 2
                                (semantics (tbl gpio-6-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-7 (offset 0 20) 2
                                (semantics (tbl gpio-7-modes
                                                #:default 'sync-input)))
          (‣ mode-select-gpio-8 (offset 0 22) 2
                                (semantics (tbl gpio-8-modes
                                                #:default 'irq-output)))
          (‣ reserved (offset 0 24) (octets 1))
          (‣ reserved (offset 4 0) (octets 4))
          (‣ gpio-0-direction (offset 8 0) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ gpio-1-direction (offset 8 1) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ gpio-2-direction (offset 8 2) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ gpio-3-direction (offset 8 3) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ gpio-0-direction-mask (offset 8 4) 1 (default #f))
          (‣ gpio-1-direction-mask (offset 8 5) 1 (default #f))
          (‣ gpio-2-direction-mask (offset 8 6) 1 (default #f))
          (‣ gpio-3-direction-mask (offset 8 7) 1 (default #f))
          (‣ gpio-4-direction (offset 8 8) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ gpio-5-direction (offset 8 9) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ gpio-6-direction (offset 8 10) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ gpio-7-direction (offset 8 11) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ gpio-4-direction-mask (offset 8 12) 1 (default #f))
          (‣ gpio-5-direction-mask (offset 8 13) 1 (default #f))
          (‣ gpio-6-direction-mask (offset 8 14) 1 (default #f))
          (‣ gpio-7-direction-mask (offset 8 15) 1 (default #f))
          (‣ gpio-8-direction (offset 8 16) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ reserved (offset 8 17) 3)
          (‣ gpio-8-direction-mask (offset 8 20) 1 (default #f))
          (‣ reserved (offset 8 21) 11)
          (‣ gpio-0-output-value (offset 12 0) 1 (default #f))
          (‣ gpio-1-output-value (offset 12 1) 1 (default #f))
          (‣ gpio-2-output-value (offset 12 2) 1 (default #f))
          (‣ gpio-3-output-value (offset 12 3) 1 (default #f))
          (‣ gpio-0-output-mask (offset 12 4) 1 (default #f))
          (‣ gpio-1-output-mask (offset 12 5) 1 (default #f))
          (‣ gpio-2-output-mask (offset 12 6) 1 (default #f))
          (‣ gpio-3-output-mask (offset 12 7) 1 (default #f))
          (‣ gpio-4-output-value (offset 12 8) 1 (default #f))
          (‣ gpio-5-output-value (offset 12 9) 1 (default #f))
          (‣ gpio-6-output-value (offset 12 10) 1 (default #f))
          (‣ gpio-7-output-value (offset 12 11) 1 (default #f))
          (‣ gpio-4-output-mask (offset 12 12) 1 (default #f))
          (‣ gpio-5-output-mask (offset 12 13) 1 (default #f))
          (‣ gpio-6-output-mask (offset 12 14) 1 (default #f))
          (‣ gpio-7-output-mask (offset 12 15) 1 (default #f))
          (‣ gpio-8-output-value (offset 12 16) 1 (default #f))
          (‣ reserved (offset 12 17) 3)
          (‣ gpio-8-output-mask (offset 12 20) 1 (default #f))
          (‣ reserved (offset 12 21) 11)
          (‣ gpio-0-irq-enable (offset 16 0) 1 (default #f))
          (‣ gpio-1-irq-enable (offset 16 1) 1 (default #f))
          (‣ gpio-2-irq-enable (offset 16 2) 1 (default #f))
          (‣ gpio-3-irq-enable (offset 16 3) 1 (default #f))
          (‣ gpio-4-irq-enable (offset 16 4) 1 (default #f))
          (‣ gpio-5-irq-enable (offset 16 5) 1 (default #f))
          (‣ gpio-6-irq-enable (offset 16 6) 1 (default #f))
          (‣ gpio-7-irq-enable (offset 16 7) 1 (default #f))
          (‣ gpio-8-irq-enable (offset 16 8) 1 (default #f))
          (‣ reserved (offset 16 9) 23)
          (‣ gpio-0-irq-sense-mode (offset 20 0) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-1-irq-sense-mode (offset 20 1) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-2-irq-sense-mode (offset 20 2) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-3-irq-sense-mode (offset 20 3) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-4-irq-sense-mode (offset 20 4) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-5-irq-sense-mode (offset 20 5) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-6-irq-sense-mode (offset 20 6) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-7-irq-sense-mode (offset 20 7) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-8-irq-sense-mode (offset 20 8) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ reserved (offset 20 9) 23)
          (‣ gpio-0-irq-mode (offset 24 0) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-1-irq-mode (offset 24 1) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-2-irq-mode (offset 24 2) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-3-irq-mode (offset 24 3) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-4-irq-mode (offset 24 4) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-5-irq-mode (offset 24 5) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-6-irq-mode (offset 24 6) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-7-irq-mode (offset 24 7) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-8-irq-mode (offset 24 8) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ reserved (offset 24 9) 23)
          (‣ gpio-0-irq-edge-mode (offset 28 0) 1
                                  (semantics (tbl gpio-irq-both-map
                                                  #:default 'normal)))
          (‣ gpio-1-irq-edge-mode (offset 28 1) 1
                                  (semantics (tbl gpio-irq-both-map
                                                  #:default 'normal)))
          (‣ gpio-2-irq-edge-mode (offset 28 2) 1
                                  (semantics (tbl gpio-irq-both-map
                                                  #:default 'normal)))
          (‣ gpio-3-irq-edge-mode (offset 28 3) 1
                                  (semantics (tbl gpio-irq-both-map
                                                  #:default 'normal)))
          (‣ gpio-4-irq-edge-mode (offset 28 4) 1
                                  (semantics (tbl gpio-irq-both-map
                                                  #:default 'normal)))
          (‣ gpio-5-irq-edge-mode (offset 28 5) 1
                                  (semantics (tbl gpio-irq-both-map
                                                  #:default 'normal)))
          (‣ gpio-6-irq-edge-mode (offset 28 6) 1
                                  (semantics (tbl gpio-irq-both-map
                                                  #:default 'normal)))
          (‣ gpio-7-irq-edge-mode (offset 28 7) 1
                                  (semantics (tbl gpio-irq-both-map
                                                  #:default 'normal)))
          (‣ gpio-8-irq-edge-mode (offset 28 8) 1
                                  (semantics (tbl gpio-irq-both-map
                                                  #:default 'normal)))
          (‣ reserved (offset 28 9) 23)
          (‣ gpio-0-clear-irq-flag (offset 32 0) 1 (default #f))
          (‣ gpio-1-clear-irq-flag (offset 32 1) 1 (default #f))
          (‣ gpio-2-clear-irq-flag (offset 32 2) 1 (default #f))
          (‣ gpio-3-clear-irq-flag (offset 32 3) 1 (default #f))
          (‣ gpio-4-clear-irq-flag (offset 32 4) 1 (default #f))
          (‣ gpio-5-clear-irq-flag (offset 32 5) 1 (default #f))
          (‣ gpio-6-clear-irq-flag (offset 32 6) 1 (default #f))
          (‣ gpio-7-clear-irq-flag (offset 32 7) 1 (default #f))
          (‣ gpio-8-clear-irq-flag (offset 32 8) 1 (default #f))
          (‣ reserved (offset 32 9) 23)
          (‣ gpio-0-irq-debounce-enable (offset 36 0) 1
                                        (semantics boolean/active-low)
                                        (default #f))
          (‣ gpio-1-irq-debounce-enable (offset 36 1) 1
                                        (semantics boolean/active-low)
                                        (default #f))
          (‣ gpio-2-irq-debounce-enable (offset 36 2) 1
                                        (semantics boolean/active-low)
                                        (default #f))
          (‣ gpio-3-irq-debounce-enable (offset 36 3) 1
                                        (semantics boolean/active-low)
                                        (default #f))
          (‣ gpio-4-irq-debounce-enable (offset 36 4) 1
                                        (semantics boolean/active-low)
                                        (default #f))
          (‣ gpio-5-irq-debounce-enable (offset 36 5) 1
                                        (semantics boolean/active-low)
                                        (default #f))
          (‣ gpio-6-irq-debounce-enable (offset 36 6) 1
                                        (semantics boolean/active-low)
                                        (default #f))
          (‣ gpio-7-irq-debounce-enable (offset 36 7) 1
                                        (semantics boolean/active-low)
                                        (default #f))
          (‣ gpio-8-irq-debounce-enable (offset 36 8) 1
                                        (semantics boolean/active-low)
                                        (default #f))
          (‣ reserved (offset 36 9) 23)
          (‣ gpio-0-raw-io-state (offset 40 0) 1 (default #f))
          (‣ gpio-1-raw-io-state (offset 40 1) 1 (default #f))
          (‣ gpio-2-raw-io-state (offset 40 2) 1 (default #f))
          (‣ gpio-3-raw-io-state (offset 40 3) 1 (default #f))
          (‣ gpio-4-raw-io-state (offset 40 4) 1 (default #f))
          (‣ gpio-5-raw-io-state (offset 40 5) 1 (default #f))
          (‣ gpio-6-raw-io-state (offset 40 6) 1 (default #f))
          (‣ gpio-7-raw-io-state (offset 40 7) 1 (default #f))
          (‣ gpio-8-raw-io-state (offset 40 8) 1 (default #f))
          (‣ reserved (offset 40 9) 23)))))

(define reg:digital-rx-cfg
  (register
   (name 'digital-rx-cfg)
   (address 39)
   (description "Digital Receiver configuration")
   (width (octets 46))
   (items
    (list (‣ digital-rx-cfg 0 (octets 46))))))

(define reg:analog-rx-cfg
  (register
   (name 'analog-rx-cfg)
   (address 40)
   (description "Analog RF Configuration")
   (width (octets 53))
   (items
    (list (‣ analog-rx-cfg 0 (octets 53))))))

(define reg:tx-calibration-cfg
  (register
   (name 'tx-calibration-cfg)
   (address 42)
   (description "Transmitter calibration block")
   (width (octets 13))
   (items
    (list (‣ tx-calibration-cfg 0 (octets 13))))))

(define reg:frequency-synthesizer-ctrl
  (register
   (name 'frequency-synthesizer-ctrl)
   (address 43)
   (description "Frequency synthesiser control block")
   (width (octets 21))
   (items
    (list (‣ frequency-synthesizer-ctrl 0 (octets 21))))))

(define reg:always-on-ctrl
  (register
   (name 'always-on-ctrl)
   (address 44)
   (description "Always-On register set")
   (width (octets 12))
   (items
    (list (‣ always-on-ctrl 0 (octets 12))))))

(define reg:otp-interface
  (register
   (name 'otp-interface)
   (address 45)
   (description "One Time Programmable Memory Interface")
   (width (octets 13))
   (items
    (list (‣ otp-interface 0 (octets 13))))))

(define reg:leading-edge-detect-ctrl
  (register
   (name 'leading-edge-detect-ctrl)
   (address 46)
   (description "Leading edge detection control block")
   (width (octets 10246))
   (items
    (list (‣ leading-edge-detect-ctrl 0 (octets 10246))))))

(define reg:digital-diagnostics
  (register
   (name 'digital-diagnostics)
   (address 47)
   (description "Digital Diagnostics Interface")
   (width (octets 38))
   (items
    (list (‣ event-counter-enable (offset 0 0) 1)
          (‣ event-counter-clear (offset 0 1) 1)
          (‣ reserved (offset 0 2) 30)
          (‣ cnt-physical-header-error (offset 4 0) 12)
          (‣ reserved (offset 4 12) 4)
          (‣ cnt-frame-sync-lost (offset 6 0) 12)
          (‣ reserved (offset 6 12) 4)
          (‣ cnt-frame-checksum-good (offset 8 0) 12)
          (‣ reserved (offset 8 12) 4)
          (‣ cnt-frame-checksum-error (offset 10 0) 12)
          (‣ reserved (offset 10 12) 4)
          (‣ cnt-frame-filter-rejection (offset 12 0) 12)
          (‣ reserved (offset 12 12) 4)
          (‣ cnt-rx-overrun (offset 14 0) 12)
          (‣ reserved (offset 14 12) 4)
          (‣ cnt-sfd-timeout (offset 16 0) 12)
          (‣ reserved (offset 16 12) 4)
          (‣ cnt-preamble-detection-timeout (offset 18 0) 12)
          (‣ reserved (offset 18 12) 4)
          (‣ cnt-rx-frame-wait-timeout (offset 20 0) 12)
          (‣ reserved (offset 20 12) 4)
          (‣ cnt-tx-frame-sent (offset 22 0) 12)
          (‣ reserved (offset 22 12) 4)
          (‣ cnt-half-period-warning (offset 24 0) 12)
          (‣ reserved (offset 24 12) 4)
          (‣ cnt-tx-power-up-warning (offset 26 0) 12)
          (‣ reserved (offset 26 12) 4)
          (‣ reserved (offset 28 0) (octets 8))
          (‣ reserved (offset 36 0) 4)
          (‣ tx-power-spectrum-test-mode-enable (offset 36 4) 1)
          (‣ reserved (offset 36 5) 11)))))

(define reg:power-management-ctrl
  (register
   (name 'power-management-ctrl)
   (address 54)
   (description "Power Management System Control Block")
   (width (octets 44))
   (items
    (list (‣ system-clock-select 0 2 (semantics (tbl system-clock-map
                                                     #:default 'auto)))
          (‣ rx-clock-select 2 2 (semantics (tbl system-clock-map
                                                 #:default 'auto)))
          (‣ tx-clock-select 4 2 (semantics (tbl system-clock-map
                                                 #:default 'auto)))
          (‣ force-accumulator-clock-enable 6 1 (default #f))
          (‣ reserved 7 3 (default 4))
          (‣ adc-convert-clock-enable 10 1 (default #f))
          (‣ reserved 11 4)
          (‣ accumulator-memory-clock-enable 15 1 (default #f))
          (‣ gpio-clock-enable 16 1 (default #f))
          (‣ gpio-reset 17 1
                        (semantics boolean/active-low)
                        (default #t))
          (‣ gpio-debounce-clock-enable 18 1 (default #f))
          (‣ gpio-debounce-reset 19 1
                                 (semantics boolean/active-low)
                                 (default #t))
          (‣ reserved 20 3 (default 3))
          (‣ kilohertz-clock-enable 23 1 (default #f))
          (‣ reserved 24 4)
          (‣ soft-reset 28 4 (default 15))
          (‣ reserved (offset 4 0) 1)
          (‣ auto-rx-to-init (offset 4 1) 1 (default #f))
          (‣ reserved (offset 4 2) 1)
          (‣ packet-sequence (offset 4 3) 8 (default 231))
          (‣ auto-tx-to-sleep (offset 4 11) 1 (default #f))
          (‣ auto-rx-to-sleep (offset 4 12) 1 (default #f))
          (‣ snooze-enable (offset 4 13) 1 (default #f))
          (‣ snooze-repeat-enable (offset 4 14) 1 (default #f))
          (‣ pll-sync-clock-enable (offset 4 15) 1 (default #f))
          (‣ reserved (offset 4 16) 1)
          (‣ lde-run-enable (offset 4 17) 1 (default #f))
          (‣ reserved (offset 4 18) 8 (default 64))
          (‣ kilohertz-clock-divider (offset 4 26) 6 (default 1048576))
          (‣ reserved (offset 8 0) (octets 4))
          (‣ snooze-time (offset 12 0) (octets 1) (default 64))
          (‣ reserved (offset 12 (octets 1)) (octets 3))
          (‣ reserved (offset 16 0) (octets 22))
          (‣ tx-fine-grain-power-sequencing (offset 38 0) (octets 2)
                                            (default 2876))
          (‣ led-blink-time (offset 40 0) (octets 1))
          (‣ led-blink-enable (offset 40 8) 1)
          (‣ reserved (offset 40 9) 7)
          (‣ led-blink-now-mask (offset 40 16) 4)
          (‣ reserved (offset 40 20) 12)))))
