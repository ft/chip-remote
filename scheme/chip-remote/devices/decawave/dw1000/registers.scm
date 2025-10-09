;; Copyright (c) 2018-2024 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; The dw1000 has a bunch of large registers, that contain many many items. In
;; order to be able to address smaller chunks of data, the chips allows for sub
;; indexing into the registers.
;;
;; The way this transcription handles this is to encode the top-level registers
;; as pages (a page is a register-map, and the one page-map maps page addresses
;; to register-maps), and then put the chip's sub-registers into chip-remote
;; registers. The device-access will then offer access to these smaller chunks
;; of memory naturally.

(define-module (chip-remote devices decawave dw1000 registers)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote item)
  #:use-module (chip-remote item builder)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:use-module (chip-remote devices decawave dw1000 tables))

(define double-buffered-registers
  '((reg:rx-frame-info . 16)
    (reg:rx-buffer . 17)
    (reg:rx-frame-quality-info . 18)
    (reg:rx-time-track-interval . 19)
    (reg:rx-time-track-offset . 20)
    (reg:rx-time-of-arrival . 21)))

(define (octets n) (* n 8))

(define (offset byte bit) (+ (* byte 8) bit))

;; 0x00 4octets DEV_ID RO
;; Device Identifier; includes Device Type and Revision Info

(define-public reg:device-type
  (register
   (name 'device-type)
   (address 0)
   (width (octets 2))
   (items (list (‣ revision 0 4 (default 0))
                (‣ version  4 4 (default 3))
                (‣ model    8 8 (default 1))))))

(define-public reg:identification
  (register
   (name 'id)
   (address 2)
   (width (octets 2))
   (items (list (‣ register-id 0 (octets 2) (default #xdeca))))))

(define-public page:device-id
  (register-map
   (name 'device-id)
   (address #x00)
   (description "Device ID register")
   (width (octets 4))
   (table (↔ (0 reg:device-type)
             (2 reg:identification)))))

;; 0x01 8octets EUI RW
;; Extended Unique Identifier

(define-public reg:euid-device
  (register
   (name 'euid-device)
   (address 0)
   (width (octets 5))
   (items (list (‣ euid-device 0 (octets 5) (default #xff00000000))))))

(define-public reg:euid-manufacturer
  (register
   (name 'euid-manufacturer)
   (address 0)
   (width (octets 3))
   (items (list (‣ euid-manufacturer 0 (octets 3) (default #xffffff))))))

(define-public page:ieee-euid
  (register-map
   (name 'ieee-euid)
   (address #x01)
   (description "IEEE Extended Unique Identifier")
   (width (octets 8))
   (table (↔ (0 reg:euid-device)
             (5 reg:euid-manufacturer)))))

;; 0x02 RESERVED
;; 0x03 4octets PANADR RW
;; PAN Identifier and Short Address

(define-public reg:short-address
  (register
   (name 'short-address)
   (address 0)
   (width (octets 2))
   (items (list (‣ short-address 0 (octets 2) (default #xffff))))))

(define-public reg:pan-id
  (register
   (name 'pan-id)
   (address 0)
   (width (octets 2))
   (items (list (‣ pan-id 0 (octets 2) (default #xffff))))))

(define-public page:pan-id/short-address
  (register-map
   (name 'pan-id/short-address)
   (address #x03)
   (description "PAN ID and Short Address")
   (width (octets 4))
   (table (↔ (0 reg:short-address)
             (2 reg:pan-id)))))

;; 0x04 4octets SYS_CFG RW
;; System Configuration Bitmap

(define-public reg:system-cfg
  (register
   (name 'system-cfg)
   (address 0)
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

(define-public page:system-cfg
  (register-map
   (name 'system-cfg)
   (address #x04)
   (description "System Configuration")
   (width (octets 4))
   (table (↔ (0 reg:system-cfg)))))

;; 0x05 RESERVED
;; 0x06 5octets SYS_TIME RO
;; System Time Counter (40-bit)

(define-public reg:system-time
  (register
   (name 'system-time)
   (address 6)
   (width (octets 5))
   (items (list (‣ system-time 0 40)))))

(define-public page:system-time
  (register-map
   (name 'system-time)
   (address #x06)
   (description "System Time Counter")
   (width (octets 5))
   (table (↔ (0 reg:system-time)))))

;; 0x07 RESERVED
;; 0x08 4octets TX_FCTRL RW
;; Transmit Frame Control

(define-public reg:tx-frame-ctrl
  (register
   (name 'tx-frame-ctrl)
   (address 8)
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

(define-public page:tx-frame-ctrl
  (register-map
   (name 'tx-frame-ctrl)
   (address #x08)
   (description "Transmit Frame Control")
   (width (octets 5))
   (table (↔ (0 reg:tx-frame-ctrl)))))

;; 0x09 1024octets TX_BUFFER WO
;; Transmit Data Buffer

(define-public reg:tx-buffer
  (register
   (name 'tx-buffer)
   (address 0)
   (items
    (list (‣ tx-buffer 0 (octets 1024))))))

(define-public page:tx-buffer
  (register-map
   (name 'tx-buffer)
   (address #x09)
   (description "Transmit Data Buffer")
   (width (octets 1024))
   (table (↔ (0 reg:tx-buffer)))))

;; 0x0a 5octets DX_TIME RW
;; Delayed Send or Receive Time (40-bit)

(define-public reg:delayed-tx/rx-time
  (register
   (name 'delayed-tx/rx-time)
   (address 0)
   (width (octets 5))
   (items (list (‣ delay-time 0 (octets 5))))))

(define-public page:delayed-tx/rx-time
  (register-map
   (name 'delayed-tx/rx-time)
   (address #x0a)
   (description "Delayed Send or Receive Time")
   (width (octets 5))
   (table (↔ (0 reg:delayed-tx/rx-time)))))

;; 0x0b RESERVED
;; 0x0c 2octets RX_FWTO RW
;; Receive Frame Wait Timeout Period

(define-public reg:rx-frame-wait-timeout
  (register
   (name 'rx-frame-wait-timeout)
   (address 0)
   (width (octets 2))
   (items (list (‣ rx-frame-wait-timeout 0 (octets 2))))))

(define-public page:rx-frame-wait-timeout
  (register-map
   (name 'rx-frame-wait-timeout)
   (address #x0c)
   (description "Receive Frame Wait Timeout Period")
   (width (octets 2))
   (table (↔ (0 reg:rx-frame-wait-timeout)))))

;; 0x0d 4octets SYS_CTRL SRW
;; System Control Register

(define-public reg:system-ctrl
  (register
   (name 'system-ctrl)
   (address 0)
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

(define-public page:system-ctrl
  (register-map
   (name 'system-ctrl)
   (address #x0d)
   (description "System Control Register")
   (width (octets 4))
   (table (↔ (0 reg:system-ctrl)))))

;; 0x0e 4octets SYS_MASK RW
;; System Event Mask Register

(define-public reg:system-event-mask
  (register
   (name 'system-event-mask)
   (address 0)
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

(define-public page:system-event-mask
  (register-map
   (name 'system-event-mask)
   (address #x0e)
   (description "System Event Mask Register")
   (width (octets 4))
   (table (↔ (0 reg:system-event-mask)))))

;; 0x0f 5octets SYS_STATUS SRW
;; System Event Status Register

(define-public reg:system-status
  (register
   (name 'system-status)
   (address 0)
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

(define-public page:system-status
  (register-map
   (name 'system-status)
   (address #x0f)
   (description "System event Status Register")
   (width (octets 5))
   (table (↔ (0 reg:system-status)))))

;; 0x10 4octets RX_FINFO ROD
;; RX Frame Information

(define-public reg:rx-frame-info
  (register
   (name 'rx-frame-info)
   (address 0)
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

(define-public page:rx-frame-info
  (register-map
   (name 'rx-frame-info)
   (address #x10)
   (description "RX Frame Information")
   (width (octets 4))
   (table (↔ (0 reg:rx-frame-info)))))

;; 0x11 1024octets RX_BUFFER ROD
;; Receive Data

(define-public reg:rx-buffer
  (register
   (name 'rx-buffer)
   (address 0)
   (items (list (‣ rx-buffer 0 (octets 1024))))))

(define-public page:rx-buffer
  (register-map
   (name 'rx-buffer)
   (address #x11)
   (description "Receive Data Buffer")
   (width (octets 1024))
   (table (↔ (0 reg:rx-buffer)))))

;; 0x12 8octets RX_FQUAL ROD
;; RX Frame Quality information

(define-public reg:rxfq-noise-stddev
  (register
   (name 'rxfq-noise-stddev)
   (address 0)
   (width (octets 2))
   (items (list (‣ rxfq-noise-stddev 0 (octets 2))))))

(define-public reg:rxfq-first-path-amplitude-point-2
  (register
   (name 'rxfq-first-path-amplitude-point-2)
   (address 2)
   (width (octets 2))
   (items (list (‣ rxfq-first-path-amplitude-point-2 0 (octets 2))))))

(define-public reg:rxfq-first-path-amplitude-point-3
  (register
   (name 'rxfq-first-path-amplitude-point-3)
   (address 4)
   (width (octets 2))
   (items (list (‣ rxfq-first-path-amplitude-point-3 0 (octets 2))))))

(define-public reg:rxfq-channel-impulse-response-power
  (register
   (name 'rxfq-channel-impulse-response-power)
   (address 6)
   (width (octets 2))
   (items (list (‣ rxfq-channel-impulse-response-power 0 (octets 2))))))

(define-public page:rx-frame-quality-info
  (register-map
   (name 'rx-frame-quality-info)
   (address #x12)
   (description "Rx Frame Quality information")
   (width (octets 8))
   (table (↔ (0 reg:rxfq-noise-stddev)
             (2 reg:rxfq-first-path-amplitude-point-2)
             (4 reg:rxfq-first-path-amplitude-point-3)
             (6 reg:rxfq-channel-impulse-response-power)))))

;; 0x13 4octets RX_TTCKI ROD
;; Receiver Time Tracking Interval

(define-public reg:rx-time-track-interval
  (register
   (name 'rx-time-track-interval)
   (address 0)
   (width (octets 4))
   (items
    (list (‣ rx-time-tracking-interval 0 (octets 4))))))

(define-public page:rx-time-track-interval
  (register-map
   (name 'rx-time-track-interval)
   (address #x13)
   (description "Receiver Time Tracking Interval")
   (width (octets 4))
   (table (↔ (0 reg:rx-time-track-interval)))))

;; 0x14 5octets RX_TTCKO ROD
;; Receiver Time Tracking Offset

(define-public reg:rx-time-track-offset
  (register
   (name 'rx-time-track-offset)
   (address 0)
   (description "Receiver Time Tracking Offset")
   (width (octets 5))
   (items
    (list (‣ rx-time-tracking-offset 0 19 (semantics twos-complement))
          (‣ reserved 19 5)
          (‣ internal-resampler-delay 24 8)
          (‣ rx-carrier-phase-adjust 32 7)
          (‣ reserved 39 1)))))

(define-public page:rx-time-track-offset
  (register-map
   (name 'rx-time-track-offset)
   (address #x14)
   (description "Receiver Time Tracking Offset")
   (width (octets 5))
   (table (↔ (0 reg:rx-time-track-offset)))))

;; 0x15 14octets RX_TIME ROD
;; Receive Message Time of Arrival

(define-public reg:rx-time-stamp
  (register
   (name 'rx-time-stamp)
   (address 0)
   (width (octets 5))
   (items (list (‣ rx-time-stamp 0 (octets 5))))))

(define-public reg:first-path-index
  (register
   (name 'first-path-index)
   (address 5)
   (width (octets 2))
   (items (list (‣ first-path-index 0 (octets 2))))))

(define-public reg:first-path-amplitude-point-1
  (register
   (name 'first-path-amplitude-point-1)
   (address 7)
   (width (octets 2))
   (items (list (‣ first-path-amplitude-point-1 0 (octets 2))))))

(define-public reg:rx-raw-frame-time-stamp
  (register
   (name 'rx-raw-frame-time-stamp)
   (address 9)
   (width (octets 5))
   (items (list (‣ rx-raw-frame-time-stamp 0 (octets 5))))))

(define-public page:rx-time-of-arrival
  (register-map
   (name 'rx-time-of-arrival)
   (address #x15)
   (description "Receive Message Time of Arrival")
   (width (octets 14))
   (table (↔ (0 reg:rx-time-stamp)
             (5 reg:first-path-index)
             (7 reg:first-path-amplitude-point-1)
             (9 reg:rx-raw-frame-time-stamp)))))

;; 0x16 RESERVED
;; 0x17 10octets TX_TIME RO
;; Transmit Message Time of Sending

(define-public reg:tx-time-stamp
  (register
   (name 'tx-time-stamp)
   (address 0)
   (width (octets 5))
   (items (list (‣ tx-time-stamp 0 (octets 5))))))

(define-public reg:tx-raw-frame-time-stamp
  (register
   (name 'tx-raw-frame-time-stamp)
   (address 5)
   (width (octets 5))
   (items (list (‣ tx-raw-frame-time-stamp 0 (octets 5))))))

(define-public page:tx-time-of-sending
  (register-map
   (name 'rx-time-of-arrival)
   (address #x17)
   (description "Transmit Message Time of Sending")
   (width (octets 10))
   (table (↔ (0 reg:tx-time-stamp)
             (5 reg:tx-raw-frame-time-stamp)))))

;; 0x18 2octets TX_ANTD RW
;; 16-bit Delay from Transmit to Antenna

(define-public reg:tx-antenna-delay
  (register
   (name 'tx-antenna-delay)
   (address 0)
   (width (octets 2))
   (items (list (‣ tx-antenna-delay 0 (octets 2))))))

(define-public page:tx-antenna-delay
  (register-map
   (name 'tx-antenna-delay)
   (address #x18)
   (description "16-bit Delay from Transmit to Antenna")
   (width (octets 2))
   (table (↔ (0 reg:tx-antenna-delay)))))

;; 0x19 4octets SYS_STATE RO
;; System State Information

(define-public reg:tx-state
  (register
   (name 'tx-state)
   (address 0)
   (width (octets 1))
   (items (list (‣ tx-state 0 (octets 1))))))

(define-public reg:rx-state
  (register
   (name 'rx-state)
   (address 1)
   (width (octets 1))
   (items (list (‣ rx-state 0 (octets 1))))))

(define-public reg:pmsc-state
  (register
   (name 'pmsc-state)
   (address 2)
   (width (octets 2))
   (items (list (‣ pmsc-state 0 (octets 2))))))

(define-public page:system-state
  (register-map
   (name 'system-state)
   (address #x19)
   (description "System State Information")
   (width (octets 4))
   (table (↔ (0 reg:tx-state)
             (1 reg:rx-state)
             (2 reg:pmsc-state)))))

;; 0x1a 4octets ACK_RESP_T RW
;; Acknowledgement Time and Response Time

(define-public reg:wait-for-response-time
  (register
   (name 'wait-for-response-time)
   (address 0)
   (width (octets 3))
   (items (list (‣ wait-for-response-time 0 20)
                (‣ reserved 20 4)))))

(define-public reg:auto-ack-time
  (register
   (name 'auto-ack-time)
   (address 3)
   (width (octets 1))
   (items (list (‣ auto-ack-time 0 (octets 1))))))

(define-public page:ack-time/response-time
  (register-map
   (name 'ack-time/response-time)
   (address #x1a)
   (description "Acknowledgement Time and Response Time")
   (width (octets 4))
   (table (↔ (0 reg:wait-for-response-time)
             (3 reg:auto-ack-time)))))

;; 0x1b RESERVED
;; 0x1c RESERVED
;; 0x1d 4octets RX_SNIFF RW
;; Pulsed Preamble Reception Configuration

(define-public reg:sniff-mode-on-time
  (register
   (name 'sniff-mode-on-time)
   (address 0)
   (width (octets 1))
   (items (list (‣ sniff-mode-on-time 0 4)
                (‣ reserved 4 4)))))

(define-public reg:sniff-mode-off-time
  (register
   (name 'sniff-mode-off-time)
   (address 1)
   (width (octets 3))
   (items (list (‣ sniff-mode-off-time 0 8)
                (‣ reserved 8 16)))))

(define-public page:rx-sniff-mode-cfg
  (register-map
   (name 'rx-sniff-mode-cfg)
   (address #x1d)
   (description "Sniff Mode Configuration")
   (width (octets 4))
   (table (↔ (0 reg:sniff-mode-on-time)
             (1 reg:sniff-mode-off-time)))))

;; 0x1e 4octets DEV_ID RW
;; TX Power Control

(define-public reg:tx-normal-frame-power
  (register
   (name 'tx-normal-frame-power)
   (address 0)
   (width (octets 1))
   (items (list (‣ tx-normal-frame-power 0 (octets 1))))))

(define-public reg:tx-500mu-frame-power
  (register
   (name 'tx-500mu-frame-power)
   (address 1)
   (width (octets 1))
   (items (list (‣ tx-500mu-frame-power 0 (octets 1))))))

(define-public reg:tx-250mu-frame-power
  (register
   (name 'tx-250mu-frame-power)
   (address 2)
   (width (octets 1))
   (items (list (‣ tx-250mu-frame-power 0 (octets 1))))))

(define-public reg:tx-125mu-frame-power
  (register
   (name 'tx-125mu-frame-power)
   (address 3)
   (width (octets 1))
   (items (list (‣ tx-125mu-frame-power 0 (octets 1))))))

(define-public page:tx-power-ctrl
  (register-map
   (name 'tx-power-ctrl)
   (address #x1e)
   (description "TX Power Control")
   (width (octets 4))
   (table (↔ (0 reg:tx-normal-frame-power)
             (1 reg:tx-500mu-frame-power)
             (2 reg:tx-250mu-frame-power)
             (3 reg:tx-125mu-frame-power)))))

;; 0x1f 4octets CHAN_CTRL RW
;; Channel Control

(define-public reg:channel-select
  (register
   (name 'channel-select)
   (address 0)
   (width (octets 2))
   (items (list (‣ tx-channel 0 4)
                (‣ rx-channel 4 4)
                (‣ reserved 8 8)))))

(define-public reg:preamble-select
  (register
   (name 'preamble-select)
   (address 2)
   (width (octets 2))
   (items (list (‣ reserved 0 1)
                (‣ use-decawave-sfd-sequence 1 1)
                (‣ rx-pulse-repetition-frequency
                   2 2 (semantics (tbl prf-map #:default '#{16MHz}#)))
                (‣ use-user-tx-sfd-sequence 4 1)
                (‣ use-user-rx-sfd-sequence 5 1)
                (‣ tx-preamble-code 6 5)
                (‣ rx-preamble-code 11 5)))))

(define-public page:channel-ctrl
  (register-map
   (name 'channel-ctrl)
   (address #x1f)
   (description "Channel Control")
   (width (octets 4))
   (table (↔ (0 reg:channel-select)
             (2 reg:preamble-select)))))

;; 0x20 RESERVED
;; 0x21 41octets USR_SFD RW
;; User-Specified Short/Long TX/RX SFD Sequences

(define-public reg:user-sfd-sequences
  (register
   (name 'user-sfd-sequences)
   (address 0)
   (width (octets 41))
   (items (list (‣ user-sfd-memory 0 (octets 41))))))

(define-public page:user-sfd-sequences
  (register-map
   (name 'user-sfd-sequences)
   (address #x21)
   (description "User-Specified Short/Long TX/RX SFD Sequences")
   (width (octets 41))
   (table (↔ (0 reg:user-sfd-sequences)))))

;; 0x22 RESERVED
;; 0x23 33octets AGC_CTRL RW
;; Automatic Gain Control Configuration

(define-public reg:agc-control
  (register
   (name 'agc-control)
   (address #x02)
   (width (octets 2))
   (items (list (‣ enable-agc-measurement
                   0 1 (semantics boolean/active-low) (default #f))
                (‣ reserved 1 15)))))

(define-public reg:agc-tune-1
  (register
   (name 'agc-tune-1)
   (address #x04)
   (width (octets 2))
   (items (list (‣ adc-tune-1
                   0 (octets 2)
                   (semantics (tbl agc-prf-tune-map #:default 'prf-16mhz)))))))

(define-public reg:agc-tune-2
  (register
   (name 'agc-tune-2)
   (address #x0c)
   (width (octets 4))
   (items (list (‣ agc-tune-2 0 (octets 4) (default #x2502a907))))))

(define-public reg:agc-tune-3
  (register
   (name 'agc-tune-3)
   (address #x12)
   (width (octets 2))
   (items (list (‣ agc-tune-3 0 (octets 2) (default #x0035))))))

(define-public reg:agc-status
  (register
   (name 'agc-status)
   (address #x1e)
   (width (octets 3))
   (items (list (‣ reserved  0 6)
                (‣ agc-edg1  6 5)
                (‣ agc-edv2 11 9)
                (‣ reserved 20 4)))))

(define-public page:agc-ctrl
  (register-map
   (name 'agc-ctrl)
   (address #x23)
   (description "Automatic Gain Control configuration")
   (width (octets 33))
   (table (↔ (#x02 reg:agc-control)
             (#x04 reg:agc-tune-1)
             (#x0c reg:agc-tune-2)
             (#x12 reg:agc-tune-3)
             (#x1e reg:agc-status)))))

;; 0x24 12octets EXT_SYNC RW
;; External Synchronisation Control

(define-public reg:external-sync-ctrl-memory
  (register
   (name 'external-sync-ctrl-memory)
   (address 0)
   (width (octets 12))
   (items (list (‣ external-sync-ctrl-memory 0 (octets 12))))))

(define-public page:external-sync-ctrl
  (register-map
   (name 'external-sync-ctrl)
   (address #x24)
   (description "External Synchronisation Control")
   (width (octets 12))
   (table (↔ (0 reg:external-sync-ctrl-memory)))))


;; 0x25 4064octets ACC_MEM RO
;; Read access to accumulator data

(define-public reg:accumulator-memory
  (register
   (name 'accumulator-memory)
   (address 0)
   (width (octets 4064))
   (items (list (‣ accumulator-memory 0 (octets 4064))))))

(define-public page:accumulator-memory
  (register-map
   (name 'accumulator-memory)
   (address #x25)
   (description "Read access to accumulator data")
   (width (octets 4064))
   (table (↔ (0 reg:accumulator-memory)))))

;; 0x26 44octets GPIO_CTRL RW
;; Peripheral register bus 1 access – GPIO control

(define-public reg:gpio-mode
  (register
   (name 'gpio-mode)
   (address #x00)
   (width (octets 8))
   (items
    (list
     (‣ reserved 0 6)
     (‣ mode-select-gpio-0 6 2 (semantics (tbl gpio-0-modes #:default 'gpio)))
     (‣ mode-select-gpio-1 8 2 (semantics (tbl gpio-1-modes #:default 'gpio)))
     (‣ mode-select-gpio-2 10 2 (semantics (tbl gpio-2-modes #:default 'gpio)))
     (‣ mode-select-gpio-3 12 2 (semantics (tbl gpio-3-modes #:default 'gpio)))
     (‣ mode-select-gpio-4 14 2 (semantics (tbl gpio-4-modes #:default 'gpio)))
     (‣ mode-select-gpio-5 16 2 (semantics (tbl gpio-5-modes #:default 'gpio)))
     (‣ mode-select-gpio-6 18 2 (semantics (tbl gpio-6-modes #:default 'gpio)))
     (‣ mode-select-gpio-7 20 2 (semantics (tbl gpio-7-modes #:default 'sync-input)))
     (‣ mode-select-gpio-8 22 2 (semantics (tbl gpio-8-modes #:default 'irq-output)))
     (‣ reserved 24 (octets 5))))))

(define-public reg:gpio-direction
  (register
   (name 'gpio-direction)
   (address #x08)
   (width (octets 4))
   (items
    (list
     (‣ gpio-0-direction 0 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-1-direction 1 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-2-direction 2 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-3-direction 3 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-0-direction-mask 4 1 (default #f))
     (‣ gpio-1-direction-mask 5 1 (default #f))
     (‣ gpio-2-direction-mask 6 1 (default #f))
     (‣ gpio-3-direction-mask 7 1 (default #f))
     (‣ gpio-4-direction  8 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-5-direction  9 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-6-direction 10 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-7-direction 11 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-4-direction-mask 12 1 (default #f))
     (‣ gpio-5-direction-mask 13 1 (default #f))
     (‣ gpio-6-direction-mask 14 1 (default #f))
     (‣ gpio-7-direction-mask 15 1 (default #f))
     (‣ gpio-8-direction 16 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ reserved 17 3)
     (‣ gpio-8-direction-mask 20 1 (default #f))
     (‣ reserved 21 11)))))

(define-public reg:gpio-data-output
  (register
   (name 'gpio-data-output)
   (address #x0c)
   (width (octets 4))
   (items (list (‣ gpio-0-output-value  0 1 (default #f))
                (‣ gpio-1-output-value  1 1 (default #f))
                (‣ gpio-2-output-value  2 1 (default #f))
                (‣ gpio-3-output-value  3 1 (default #f))
                (‣ gpio-0-output-mask   4 1 (default #f))
                (‣ gpio-1-output-mask   5 1 (default #f))
                (‣ gpio-2-output-mask   6 1 (default #f))
                (‣ gpio-3-output-mask   7 1 (default #f))
                (‣ gpio-4-output-value  8 1 (default #f))
                (‣ gpio-5-output-value  9 1 (default #f))
                (‣ gpio-6-output-value 10 1 (default #f))
                (‣ gpio-7-output-value 11 1 (default #f))
                (‣ gpio-4-output-mask  12 1 (default #f))
                (‣ gpio-5-output-mask  13 1 (default #f))
                (‣ gpio-6-output-mask  14 1 (default #f))
                (‣ gpio-7-output-mask  15 1 (default #f))
                (‣ gpio-8-output-value 16 1 (default #f))
                (‣ reserved            17 3)
                (‣ gpio-8-output-mask  20 1 (default #f))
                (‣ reserved            21 11)))))

(define-public reg:gpio-interrupt-enable
  (register
   (name 'gpio-interrupt-enable)
   (address #x10)
   (width (octets 4))
   (items (list (‣ gpio-0-irq-enable 0 1 (default #f))
                (‣ gpio-1-irq-enable 1 1 (default #f))
                (‣ gpio-2-irq-enable 2 1 (default #f))
                (‣ gpio-3-irq-enable 3 1 (default #f))
                (‣ gpio-4-irq-enable 4 1 (default #f))
                (‣ gpio-5-irq-enable 5 1 (default #f))
                (‣ gpio-6-irq-enable 6 1 (default #f))
                (‣ gpio-7-irq-enable 7 1 (default #f))
                (‣ gpio-8-irq-enable 8 1 (default #f))
                (‣ reserved          9 23)))))

(define-public reg:gpio-interrupt-sense
  (register
   (name 'gpio-interrupt-sense)
   (address #x14)
   (width (octets 4))
   (items
    (list
     ;; TODO: Can this semantics be shorter?
     (‣ gpio-0-irq-sense-mode 0 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-1-irq-sense-mode 1 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-2-irq-sense-mode 2 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-3-irq-sense-mode 3 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-4-irq-sense-mode 4 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-5-irq-sense-mode 5 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-6-irq-sense-mode 6 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-7-irq-sense-mode 7 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-8-irq-sense-mode 8 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ reserved              9 23)))))

(define-public reg:gpio-interrupt-mode
  (register
   (name 'gpio-interrupt-mode)
   (address #x18)
   (width (octets 4))
   (items
    (list
     (‣ gpio-0-irq-mode 0 1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-1-irq-mode 1 1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-2-irq-mode 2 1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-3-irq-mode 3 1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-4-irq-mode 4 1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-5-irq-mode 5 1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-6-irq-mode 6 1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-7-irq-mode 7 1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-8-irq-mode 8 1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ reserved        9 23)))))

(define-public reg:gpio-interrupt-bothedge
  (register
   (name 'reg:gpio-interrupt-bothedge)
   (address #x1c)
   (width (octets 4))
   (items
    (list
     (‣ gpio-0-irq-edge-mode 0 1 (semantics (tbl gpio-irq-both-map #:default 'normal)))
     (‣ gpio-1-irq-edge-mode 1 1 (semantics (tbl gpio-irq-both-map #:default 'normal)))
     (‣ gpio-2-irq-edge-mode 2 1 (semantics (tbl gpio-irq-both-map #:default 'normal)))
     (‣ gpio-3-irq-edge-mode 3 1 (semantics (tbl gpio-irq-both-map #:default 'normal)))
     (‣ gpio-4-irq-edge-mode 4 1 (semantics (tbl gpio-irq-both-map #:default 'normal)))
     (‣ gpio-5-irq-edge-mode 5 1 (semantics (tbl gpio-irq-both-map #:default 'normal)))
     (‣ gpio-6-irq-edge-mode 6 1 (semantics (tbl gpio-irq-both-map #:default 'normal)))
     (‣ gpio-7-irq-edge-mode 7 1 (semantics (tbl gpio-irq-both-map #:default 'normal)))
     (‣ gpio-8-irq-edge-mode 8 1 (semantics (tbl gpio-irq-both-map #:default 'normal)))
     (‣ reserved             9 23)))))

(define-public reg:gpio-interrupt-latch-clear
  (register
   (name 'gpio-interrupt-latch-clear)
   (address #x20)
   (width (octets 4))
   (items
    (list
     (‣ gpio-0-clear-irq-flag 0 1 (default #f))
     (‣ gpio-1-clear-irq-flag 1 1 (default #f))
     (‣ gpio-2-clear-irq-flag 2 1 (default #f))
     (‣ gpio-3-clear-irq-flag 3 1 (default #f))
     (‣ gpio-4-clear-irq-flag 4 1 (default #f))
     (‣ gpio-5-clear-irq-flag 5 1 (default #f))
     (‣ gpio-6-clear-irq-flag 6 1 (default #f))
     (‣ gpio-7-clear-irq-flag 7 1 (default #f))
     (‣ gpio-8-clear-irq-flag 8 1 (default #f))
     (‣ reserved              9 23)))))

(define-public reg:gpio-interrupt-debounce
  (register
   (name 'gpio-interrupt-debounce)
   (address #x24)
   (width (octets 4))
   (items
    (list
     (‣ gpio-0-irq-debounce-enable 0 1 (semantics boolean/active-low) (default #f))
     (‣ gpio-1-irq-debounce-enable 1 1 (semantics boolean/active-low) (default #f))
     (‣ gpio-2-irq-debounce-enable 2 1 (semantics boolean/active-low) (default #f))
     (‣ gpio-3-irq-debounce-enable 3 1 (semantics boolean/active-low) (default #f))
     (‣ gpio-4-irq-debounce-enable 4 1 (semantics boolean/active-low) (default #f))
     (‣ gpio-5-irq-debounce-enable 5 1 (semantics boolean/active-low) (default #f))
     (‣ gpio-6-irq-debounce-enable 6 1 (semantics boolean/active-low) (default #f))
     (‣ gpio-7-irq-debounce-enable 7 1 (semantics boolean/active-low) (default #f))
     (‣ gpio-8-irq-debounce-enable 8 1 (semantics boolean/active-low) (default #f))
     (‣ reserved                   9 23)))))

(define-public reg:gpio-raw-state
  (register
   (name 'gpio-raw-state)
   (address #x28)
   (width (octets 4))
   (items
    (list
     (‣ gpio-0-raw-io-state 0 1 (default #f))
     (‣ gpio-1-raw-io-state 1 1 (default #f))
     (‣ gpio-2-raw-io-state 2 1 (default #f))
     (‣ gpio-3-raw-io-state 3 1 (default #f))
     (‣ gpio-4-raw-io-state 4 1 (default #f))
     (‣ gpio-5-raw-io-state 5 1 (default #f))
     (‣ gpio-6-raw-io-state 6 1 (default #f))
     (‣ gpio-7-raw-io-state 7 1 (default #f))
     (‣ gpio-8-raw-io-state 8 1 (default #f))
     (‣ reserved            9 23)))))

(define-public page:gpio-ctrl
  (register-map
   (name 'gpio-ctrl)
   (address #x26)
   (description "GPIO control")
   (width (octets 44))
   (table (↔ (#x00 reg:gpio-mode)
             (#x08 reg:gpio-direction)
             (#x0c reg:gpio-data-output)
             (#x10 reg:gpio-interrupt-enable)
             (#x14 reg:gpio-interrupt-sense)
             (#x18 reg:gpio-interrupt-mode)
             (#x1c reg:gpio-interrupt-bothedge)
             (#x20 reg:gpio-interrupt-latch-clear)
             (#x24 reg:gpio-interrupt-debounce)
             (#x28 reg:gpio-raw-state)))))

;; 0x27 46octets DRX_CONF RW
;; Digital Receiver configuration

(define-public reg:digital-rx-cfg
  (register
   (name 'digital-rx-cfg)
   (address 0)
   (width (octets 46))
   (items (list (‣ digital-rx-cfg 0 (octets 46))))))

(define-public page:digital-rx-cfg
  (register-map
   (name 'digital-rx-cfg)
   (address #x27)
   (description "Digital Receiver configuration")
   (width (octets 46))
   (table (↔ (0 reg:digital-rx-cfg)))))

;; 0x28 58octets RF_CONF RW
;; Analog RF Configuration

(define-public reg:analog-rx-cfg
  (register
   (name 'analog-rx-cfg)
   (address 0)
   (width (octets 58))
   (items (list (‣ analog-rx-cfg 0 (octets 58))))))

(define-public page:analog-rx-cfg
  (register-map
   (name 'analog-rx-cfg)
   (address #x28)
   (description "Analog RF Configuration")
   (width (octets 58))
   (table (↔ (0 reg:analog-rx-cfg)))))


;; 0x29 RESERVED
;; 0x2a 13octets TX_CAL RW
;; Transmitter Calibration Block

(define-public reg:tx-calibration
  (register
   (name 'tx-calibration)
   (address 0)
   (width (octets 13))
   (items (list (‣ tx-calibration-cfg 0 (octets 13))))))

(define-public page:tx-calibration
  (register-map
   (name 'tx-calibration)
   (address #x2a)
   (description "Transmitter Calibration Block")
   (width (octets 13))
   (table (↔ (0 reg:tx-calibration)))))

;; 0x2b 21octets FS_CTRL RW
;; Frequency Synthesiser Control Block

(define-public reg:frequency-synthesizer-ctrl
  (register
   (name 'frequency-synthesizer-ctrl)
   (address 0)
   (width (octets 21))
   (items (list (‣ frequency-synthesizer-ctrl 0 (octets 21))))))

(define-public page:frequency-synthesizer-ctrl
  (register-map
   (name 'frequency-synthesizer-ctrl)
   (address #x2b)
   (description "Frequency Synthesiser Control Block")
   (width (octets 21))
   (table (↔ (0 reg:frequency-synthesizer-ctrl)))))

;; 0x2c 12octets AON RW
;; Always-On Register Set

(define-public reg:always-on-ctrl
  (register
   (name 'always-on-ctrl)
   (address 0)
   (width (octets 12))
   (items (list (‣ always-on-ctrl 0 (octets 12))))))

(define-public page:always-on-ctrl
  (register-map
   (name 'always-on-ctrl)
   (address #x2c)
   (description "Always-On Register Set")
   (width (octets 12))
   (table (↔ (0 reg:always-on-ctrl)))))

;; 0x2d 13octets OTP_IF RW
;; One Time Programmable Memory Interface

(define-public reg:otp-interface
  (register
   (name 'otp-interface)
   (address 0)
   (width (octets 13))
   (items (list (‣ otp-interface 0 (octets 13))))))

(define-public page:otp-interface
  (register-map
   (name 'otp-interface)
   (address #x2d)
   (description "One Time Programmable Memory Interface")
   (width (octets 13))
   (table (↔ (0 reg:otp-interface)))))

;; 0x2e TODOoctets LDE_CTRL RW
;; Leading Edge Detection Control Block

(define-public reg:leading-edge-detect-ctrl
  (register
   (name 'leading-edge-detect-ctrl)
   (address 0)
   (width (octets 2))
   (items (list (‣ leading-edge-detect-ctrl 0 (octets 2))))))

(define-public page:leading-edge-detect-ctrl
  (register-map
   (name 'leading-edge-detect-ctrl)
   (address #x2e)
   (description "Leading Edge Detection Control Block")
   (width (octets 2))
   ;; TODO: This is incomplete!
   (table (↔ (0 reg:leading-edge-detect-ctrl)))))

;; 0x2f 4octets DEV_ID RO
;; Digital Diagnostics Interface

(define-public reg:event-ctrl
  (register
   (name 'event-ctrl)
   (address #x00)
   (width (octets 4))
   (items (list (‣ event-counter-enable 0  1)
                (‣ event-counter-clear  1  1)
                (‣ reserved             2 30)))))

(define-public reg:phr-error-count
  (register
   (name 'phr-error-count)
   (address #x04)
   (width (octets 2))
   (items (list (‣ cnt-physical-header-error  0 12)
                (‣ reserved                  12  4)))))

(define-public reg:rsd-error-count
  (register
   (name 'rsd-error-count)
   (address #x06)
   (width (octets 2))
   (items (list (‣ cnt-frame-sync-lost  0 12)
                (‣ reserved            12  4)))))

(define-public reg:fcs-good-count
  (register
   (name 'fcs-good-count)
   (address #x08)
   (width (octets 2))
   (items (list (‣ cnt-frame-checksum-good  0 12)
                (‣ reserved                12  4)))))

(define-public reg:fcs-bad-count
  (register
   (name 'fcs-bad-count)
   (address #x0a)
   (width (octets 2))
   (items (list (‣ cnt-frame-checksum-error  0 12)
                (‣ reserved                 12  4)))))

(define-public reg:filter-rejection-count
  (register
   (name 'filter-rejection-count)
   (address #x0c)
   (width (octets 2))
   (items (list (‣ cnt-frame-filter-rejection  0 12)
                (‣ reserved                   12  4)))))

(define-public reg:rx-overrun-count
  (register
   (name 'rx-overrun-count)
   (address #x0e)
   (width (octets 2))
   (items (list (‣ cnt-rx-overrun  0 12)
                (‣ reserved       12  4)))))

(define-public reg:sfd-timeout-count
  (register
   (name 'sfd-timeout-count)
   (address #x10)
   (width (octets 2))
   (items (list (‣ cnt-sfd-timeout  0 12)
                (‣ reserved        12  4)))))

(define-public reg:preamble-timout-count
  (register
   (name 'preamble-timout-count)
   (address #x12)
   (width (octets 2))
   (items (list (‣ cnt-preamble-detection-timeout  0 12)
                (‣ reserved                       12  4)))))

(define-public reg:rx-frame-wait-timeout-count
  (register
   (name 'rx-frame-wait-timeout-count)
   (address #x14)
   (width (octets 2))
   (items (list (‣ cnt-rx-frame-wait-timeout  0 12)
                (‣ reserved                  12  4)))))

(define-public reg:tx-frame-sent-count
  (register
   (name 'tx-frame-sent-count)
   (address #x16)
   (width (octets 2))
   (items (list (‣ cnt-tx-frame-sent  0 12)
                (‣ reserved          12  4)))))

(define-public reg:half-period-warning-count
  (register
   (name 'half-period-warning-count)
   (address #x18)
   (width (octets 2))
   (items (list (‣ cnt-half-period-warning  0 12)
                (‣ reserved                12  4)))))

(define-public reg:tx-powerup-warning-count
  (register
   (name 'tx-powerup-warning-count)
   (address #x1a)
   (width (octets 2))
   (items (list (‣ cnt-tx-power-up-warning  0 12)
                (‣ reserved                12  4)))))

(define-public reg:diagnostic-reserved
  (register
   (name 'diagnostic-reserved)
   (address #x1c)
   (width (octets 8))
   (items (list (‣ reserved 0 (octets 8))))))

(define-public reg:diagnostic-testmode-ctrl
  (register
   (name 'diagnostic-testmode-ctrl)
   (address #x1e)
   (width (octets 2))
   (items (list (‣ reserved                           0 4)
                (‣ tx-power-spectrum-test-mode-enable 4 1)
                (‣ reserved                           5 11)))))

(define-public page:digital-diagnostics
  (register-map
   (name 'digital-diagnostics)
   (address #x2f)
   (description "Digital Diagnostics Interface")
   (width (octets 38))
   (table (↔ (#x00 reg:event-ctrl)
             (#x04 reg:phr-error-count)
             (#x06 reg:rsd-error-count)
             (#x00 reg:fcs-good-count)
             (#x08 reg:fcs-bad-count)
             (#x0a reg:filter-rejection-count)
             (#x0c reg:rx-overrun-count)
             (#x0e reg:sfd-timeout-count)
             (#x10 reg:preamble-timout-count)
             (#x12 reg:rx-frame-wait-timeout-count)
             (#x14 reg:tx-frame-sent-count)
             (#x16 reg:half-period-warning-count)
             (#x18 reg:tx-powerup-warning-count)
             (#x1a reg:diagnostic-reserved)
             (#x24 reg:diagnostic-testmode-ctrl)))))

;; 0x30 RESERVED
;; 0x31 RESERVED
;; 0x32 RESERVED
;; 0x33 RESERVED
;; 0x34 RESERVED
;; 0x35 RESERVED
;; 0x36 48octets PMSC RW
;; Power Management System Control Block

(define-public reg:pmsc-ctrl-0
  (register
   (name 'pmsc-ctrl-0)
   (address #x00)
   (width (octets 4))
   (items
    (list
     (‣ system-clock-select              0 2 (semantics (tbl system-clock-map #:default 'auto)))
     (‣ rx-clock-select                  2 2 (semantics (tbl system-clock-map #:default 'auto)))
     (‣ tx-clock-select                  4 2 (semantics (tbl system-clock-map #:default 'auto)))
     (‣ force-accumulator-clock-enable   6 1 (default #f))
     (‣ reserved                         7 3 (default 4))
     (‣ adc-convert-clock-enable        10 1 (default #f))
     (‣ reserved                        11 4)
     (‣ accumulator-memory-clock-enable 15 1 (default #f))
     (‣ gpio-clock-enable               16 1 (default #f))
     (‣ gpio-reset                      17 1 (semantics boolean/active-low) (default #t))
     (‣ gpio-debounce-clock-enable      18 1 (default #f))
     (‣ gpio-debounce-reset             19 1 (semantics boolean/active-low) (default #t))
     (‣ reserved                        20 3 (default 3))
     (‣ kilohertz-clock-enable          23 1 (default #f))
     (‣ reserved                        24 4)
     (‣ soft-reset                      28 4 (default 15))))))

(define-public reg:pmsc-ctrl-1
  (register
   (name 'pmsc-ctrl-1)
   (address #x04)
   (width (octets 4))
   (items (list (‣ reserved                 0 1)
                (‣ auto-rx-to-init          1 1 (default #f))
                (‣ reserved                 2 1)
                (‣ packet-sequence          3 8 (default 231))
                (‣ auto-tx-to-sleep        11 1 (default #f))
                (‣ auto-rx-to-sleep        12 1 (default #f))
                (‣ snooze-enable           13 1 (default #f))
                (‣ snooze-repeat-enable    14 1 (default #f))
                (‣ pll-sync-clock-enable   15 1 (default #f))
                (‣ reserved                16 1)
                (‣ lde-run-enable          17 1 (default #f))
                (‣ reserved                18 8 (default 64))
                (‣ kilohertz-clock-divider 26 6 (default 32))))))

(define-public reg:pmsc-reserved-0
  (register
   (name 'pmsc-reserved-0)
   (address #x08)
   (width (octets 4))
   (items (list (‣ reserved 0 (octets 4))))))

(define-public reg:pmsc-snooze-ctrl
  (register
   (name 'pmsc-snooze-ctrl)
   (address #x0c)
   (width (octets 4))
   (items (list (‣ snooze-time 0 (octets 1) (default 64))
                (‣ reserved    8 (octets 3))))))

(define-public reg:pmsc-reserved-1
  (register
   (name 'pmsc-reserved-1)
   (address #x10)
   (width (octets 22))
   (items (list (‣ reserved 0 (octets 22))))))

(define-public reg:pmsc-fine-tx-ctrl
  (register
   (name 'pmsc-fine-tx-ctrl)
   (address #x26)
   (width (octets 2))
   (items
    (list
     (‣ tx-fine-grain-power-sequencing 0 (octets 2) (default 2876))))))

(define-public reg:pmsc-led-ctrl
  (register
   (name 'pmsc-led-ctrl)
   (address #x28)
   (width (octets 4))
   (items (list (‣ led-blink-time      0  8)
                (‣ led-blink-enable    8  1)
                (‣ reserved            9  7)
                (‣ led-blink-now-mask 16  4)
                (‣ reserved           20 12)))))

(define-public page:power-management-ctrl
  (register-map
   (name 'power-management-ctrl)
   (address #x36)
   (description "Power Management System Control Block")
   (width (octets 44))
   (table (↔ (#x00 reg:pmsc-ctrl-0)
             (#x04 reg:pmsc-ctrl-1)
             (#x08 reg:pmsc-reserved-0)
             (#x0c reg:pmsc-snooze-ctrl)
             (#x10 reg:pmsc-reserved-1)
             (#x26 reg:pmsc-fine-tx-ctrl)
             (#x28 reg:pmsc-led-ctrl)))))
