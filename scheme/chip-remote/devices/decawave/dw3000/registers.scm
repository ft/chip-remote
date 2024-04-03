;; Copyright (c) 2023-2024 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices decawave dw3000 registers)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote item)
  #:use-module (chip-remote item builder)
  #:use-module (chip-remote register)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote devices decawave dw3000 tables)
  #:export (reg:general-cfg
            reg:general-cfg-and-aes
            reg:sts-cfg
            reg:rx-tune
            reg:ext-sync
            reg:gpio-ctrl
            reg:digital-rx-cfg
            reg:analog-rf-cfg
            reg:tx-calibration
            reg:freq-synth-ctrl
            reg:always-on-system-control
            reg:otp-interface
            reg:cia-0
            reg:cia-1
            reg:cia-2-and-rx-antenna-delay
            reg:digital-diag
            reg:pmsc
            reg:rx-buffer-0
            reg:rx-buffer-1
            reg:tx-buffer
            reg:acc-mem
            reg:scratch-ram
            reg:aes-ram
            reg:set-1/set-2
            reg:indirect-ptr-a
            reg:indirect-ptr-b
            reg:in-ptr-cfg))

(define double-buffered-registers '((reg:set-1/set-2 . 24)))

(define (octets n) (* n 8))

(define (offset byte bit) (+ (* byte 8) bit))

(define reg:general-cfg
  (register
   (name 'general-cfg)
   (address 0)
   (description "General Configuration Registers")
   (width (octets 121))
   (items
    (list (‣ device-revision 0 4)
          (‣ device-version 4 4)
          (‣ device-model 8 8)
          (‣ device-tag 16 16)
          (‣ ieee-eui-device (offset 4 0) (octets 5))
          (‣ ieee-eui-manufacturer (offset 4 (octets 5)) (octets 3))
          (‣ short-address (offset 12 0) (octets 2) (default 65535))
          (‣ pan-id (offset 12 (octets 2)) (octets 2) (default 65535))
          (‣ frame-filtering-enable (offset 16 0) 1)
          (‣ disable-fcs-tx (offset 16 1) 1)
          (‣ disable-frame-check-error-handling (offset 16 2) 1)
          (‣ disable-double-rx-buf (offset 16 3) 1 (default #t))
          (‣ phr-mode (offset 16 4) 1)
          (‣ phr-6m8 (offset 16 5) 1)
          (‣ spi-crc-enable (offset 16 6) 1)
          (‣ cia-ipatov-processing (offset 16 7) 1 (default #t))
          (‣ cia-sts-processing (offset 16 8) 1 (default #t))
          (‣ rx-wait-timeout-enable (offset 16 9) 1)
          (‣ rx-auto-reenable (offset 16 10) 1)
          (‣ auto-ack (offset 16 11) 1)
          (‣ sts-packet-configuration (offset 16 12) 2 (default 1))
          (‣ reserved (offset 16 14) 1)
          (‣ configure-sdc-mode (offset 16 15) 1)
          (‣ pdoa-mode (offset 16 16) 2)
          (‣ fast-aat (offset 16 18) 1 (default #t))
          (‣ reserved (offset 16 19) 13)
          (‣ ff-allow-beacon (offset 20 0) 1)
          (‣ ff-allow-data (offset 20 1) 1)
          (‣ ff-allow-ack (offset 20 2) 1)
          (‣ ff-allow-mac (offset 20 3) 1)
          (‣ ff-allow-reserved (offset 20 4) 1)
          (‣ ff-allow-multipurpose (offset 20 5) 1)
          (‣ ff-allow-frack (offset 20 6) 1)
          (‣ ff-allow-extended (offset 20 7) 1)
          (‣ ff-behave-as-coordinator (offset 20 8) 1)
          (‣ ff-allow-mac-implicit-broadcast (offset 20 9) 1)
          (‣ le0-pending (offset 20 10) 1)
          (‣ le1-pending (offset 20 11) 1)
          (‣ le2-pending (offset 20 12) 1)
          (‣ le3-pending (offset 20 13) 1)
          (‣ short-source-adr-enable (offset 20 14) 1)
          (‣ long-source-adr-enable (offset 20 15) 1)
          (‣ spi-crc-read-status (offset 24 0) 8)
          (‣ system-time (offset 28 0) (octets 4))
          (‣ tx-frame-length (offset 36 0) 10 (default 12))
          (‣ tx-bit-rate (offset 36 10) 1 (default 1))
          (‣ tx-phr-ranging-enable (offset 36 11) 1 (default 1))
          (‣ tx-preamble-symbol-repetitions
             (offset 36 12) 4
             (semantics (tbl preamble-symbol-rep-map
                             #:default 64)))
          (‣ tx-buf-index-offset (offset 36 16) 10)
          (‣ reserved (offset 36 26) 14)
          (‣ fine-psr-ctrl (offset 36 40) 8)
          (‣ delayed-trx-time (offset 44 0) (octets 4))
          (‣ delayed-trx-reference-time (offset 48 0) (octets 4))
          (‣ rx-frame-wait-timeout-period (offset 52 0) (octets 3))
          (‣ continuous-frame-test-start (offset 56 0) 1)
          (‣ reserved (offset 56 1) 7)
          (‣ reserved (offset 60 0) 1)
          (‣ cpll-lock-irq-enable (offset 60 1) 1)
          (‣ spi-crc-error-irq-enable (offset 60 2) 1)
          (‣ auto-ack-irq-enable (offset 60 3) 1)
          (‣ tx-frame-begins-irq-enable (offset 60 4) 1)
          (‣ tx-preamble-sent-irq-enable (offset 60 5) 1)
          (‣ tx-phy-header-sent-irq-enable (offset 60 6) 1)
          (‣ tx-frame-sent-irq-enable (offset 60 7) 1)
          (‣ rx-preamble-detected-irq-enable (offset 60 8) 1)
          (‣ rx-sfd-deteced-irq-enable (offset 60 9) 1)
          (‣ rx-cia-done-irq-enable (offset 60 10) 1)
          (‣ rx-phy-header-detected-irq-enable (offset 60 11) 1)
          (‣ rx-phy-header-error-irq-enable (offset 60 12) 1)
          (‣ rx-data-ready-irq-enable (offset 60 13) 1)
          (‣ rx-fcs-good-irq-enable (offset 60 14) 1)
          (‣ rx-fcs-error-irq-enable (offset 60 15) 1)
          (‣ rx-rs-frame-sync-loss-irq-enable (offset 60 16) 1)
          (‣ rx-wait-timeout-irq-enable (offset 60 17) 1)
          (‣ cia-error-irq-enable (offset 60 18) 1)
          (‣ voltage-warning-irq-enable (offset 60 19) 1)
          (‣ rx-overrun-irq-enable (offset 60 20) 1)
          (‣ rx-preamble-detection-timeout-irq-enable (offset 60 21) 1)
          (‣ reserved (offset 60 22) 1)
          (‣ spi-ready-irq-enable (offset 60 23) 1 (default #t))
          (‣ idle-rc-irq-enable (offset 60 24) 1)
          (‣ pll-losing-lock-irq-enable (offset 60 25) 1)
          (‣ rx-sfd-timeout-irq-enable (offset 60 26) 1)
          (‣ hpd-warn-irq-enable (offset 60 27) 1)
          (‣ sts-error-irq-enable (offset 60 28) 1)
          (‣ aff-reject-irq-enable (offset 60 29) 1)
          (‣ reserved (offset 60 30) 3)
          (‣ rx-preamble-reject-irq-enable (offset 60 33) 1)
          (‣ reserved (offset 60 34) 2)
          (‣ vt-detection-irq-enable (offset 60 36) 1)
          (‣ gpio-irq-enable (offset 60 37) 1)
          (‣ aes-done-irq-enable (offset 60 38) 1)
          (‣ aes-error-irq-enable (offset 60 39) 1)
          (‣ cmd-error-irq-enable (offset 60 40) 1 (default #t))
          (‣ spi-overflow-irq-enable (offset 60 41) 1 (default #t))
          (‣ spi-underflow-irq-enable (offset 60 42) 1 (default #t))
          (‣ spi-error-irq-enable (offset 60 43) 1 (default #t))
          (‣ cca-fail-irq-enable (offset 60 44) 1)
          (‣ interrupt-request-status (offset 68 0) 1)
          (‣ cpll-lock-irq (offset 68 1) 1)
          (‣ spi-crc-error-irq (offset 68 2) 1)
          (‣ auto-ack-irq (offset 68 3) 1)
          (‣ tx-frame-begins-irq (offset 68 4) 1)
          (‣ tx-preamble-sent-irq (offset 68 5) 1)
          (‣ tx-phy-header-sent-irq (offset 68 6) 1)
          (‣ tx-frame-sent-irq (offset 68 7) 1)
          (‣ rx-preamble-detected-irq (offset 68 8) 1)
          (‣ rx-sfd-deteced-irq (offset 68 9) 1)
          (‣ rx-cia-done-irq (offset 68 10) 1)
          (‣ rx-phy-header-detected-irq (offset 68 11) 1)
          (‣ rx-phy-header-error-irq (offset 68 12) 1)
          (‣ rx-data-ready-irq (offset 68 13) 1)
          (‣ rx-fcs-good-irq (offset 68 14) 1)
          (‣ rx-fcs-error-irq (offset 68 15) 1)
          (‣ rx-rs-frame-sync-loss-irq (offset 68 16) 1)
          (‣ rx-wait-timeout-irq (offset 68 17) 1)
          (‣ cia-error-irq (offset 68 18) 1)
          (‣ voltage-warning-irq (offset 68 19) 1)
          (‣ rx-overrun-irq (offset 68 20) 1)
          (‣ rx-preamble-detection-timeout-irq (offset 68 21) 1)
          (‣ reserved (offset 68 22) 1)
          (‣ spi-ready-irq (offset 68 23) 1)
          (‣ idle-rc-irq (offset 68 24) 1)
          (‣ pll-losing-lock-irq (offset 68 25) 1)
          (‣ rx-sfd-timeout-irq (offset 68 26) 1)
          (‣ hpd-warn-irq (offset 68 27) 1)
          (‣ sts-error-irq (offset 68 28) 1)
          (‣ aff-reject-irq (offset 68 29) 1)
          (‣ reserved (offset 68 30) 3)
          (‣ rx-preamble-reject-irq (offset 68 33) 1)
          (‣ reserved (offset 68 34) 2)
          (‣ vt-detection-irq (offset 68 36) 1)
          (‣ gpio-irq (offset 68 37) 1)
          (‣ aes-done-irq (offset 68 38) 1)
          (‣ aes-error-irq (offset 68 39) 1)
          (‣ cmd-error-irq (offset 68 40) 1)
          (‣ spi-overflow-irq (offset 68 41) 1)
          (‣ spi-underflow-irq (offset 68 42) 1)
          (‣ spi-error-irq (offset 68 43) 1)
          (‣ cca-fail-irq (offset 68 44) 1)
          (‣ reserved (offset 68 45) 3)
          (‣ rx-frame-length (offset 76 0) 10)
          (‣ reserved (offset 76 10) 1)
          (‣ rx-preamble-length-low (offset 76 11) 2)
          (‣ rx-bit-rate (offset 76 13) 1 (semantics (tbl bit-rate-map)))
          (‣ reserved (offset 76 14) 1)
          (‣ rx-ranging-bit (offset 76 15) 1)
          (‣ rx-prf-report (offset 76 16) 2 (semantics (tbl prf-map)))
          (‣ rx-preamble-length-high (offset 76 18) 2)
          (‣ rx-accu-count (offset 76 20) 12)
          (‣ rx-timestamp (offset 100 0) (octets 5))
          (‣ reserved (offset 100 40) 24)
          (‣ rx-raw-timestamp (offset 112 0) (octets 4))
          (‣ tx-timestamp (offset 116 0) (octets 5))))))

(define reg:general-cfg-and-aes
  (register
   (name 'general-cfg-and-aes)
   (address 1)
   (description "General Configuration (continued) and AES Registers")
   (items
    (list (‣ tx-raw-timestamp (offset 0 0) (octets 4))
          (‣ tx-antenna-delay (offset 4 0) (octets 2))
          (‣ wait-for-resp-time (offset 8 0) 20)
          (‣ ack-time (offset 8 24) 8)
          (‣ data-tx-power-coarse (offset 12 0) 2)
          (‣ data-tx-power-fine (offset 12 2) 6)
          (‣ phr-tx-power-coarse (offset 12 8) 2)
          (‣ phr-tx-power-fine (offset 12 10) 6)
          (‣ shr-tx-power-coarse (offset 12 16) 2)
          (‣ shr-tx-power-fine (offset 12 18) 6)
          (‣ sts-tx-power-coarse (offset 12 24) 2)
          (‣ sts-tx-power-fine (offset 12 26) 6)
          (‣ rf-channel (offset 20 0) 1)
          (‣ sfd-type (offset 20 1) 2 (semantics (tbl sfd-type-map)))
          (‣ tx-preamble-code (offset 20 3) 5)
          (‣ rx-preamble-code (offset 20 8) 5)
          (‣ le-address-0 (offset 24 0) 16)
          (‣ le-address-1 (offset 24 16) 16)
          (‣ le-address-2 (offset 28 0) 16)
          (‣ le-address-3 (offset 28 16) 16)
          (‣ spi-collision-caused-by (offset 32 0) 5
                                     (semantics (tbl spi-collision-map)))
          (‣ db0-rx-fcs-good (offset 36 0) 1)
          (‣ db0-rx-data-ready (offset 36 1) 1)
          (‣ db0-cia-done (offset 36 2) 1)
          (‣ db0-sts-error (offset 36 3) 1)
          (‣ db1-rx-fcs-good (offset 36 4) 1)
          (‣ db1-rx-data-ready (offset 36 5) 1)
          (‣ db1-cia-done (offset 36 6) 1)
          (‣ db1-sts-error (offset 36 7) 1)
          (‣ db-rx-diag-mode (offset 40 0) 3
                             (semantics (tbl db-diagnostics-map)))
          (‣ aes-mode (offset 48 0) 1 (semantics (tbl aes-mode-map)))
          (‣ aes-key-size (offset 48 1) 2 (semantics (tbl aes-key-size-map)))
          (‣ aes-key-addr-offset (offset 48 3) 3)
          (‣ load-aes-key (offset 48 6) 1)
          (‣ aes-key-source (offset 48 7) 1
                            (semantics (tbl aes-key-source-map)))
          (‣ aes-tag-size (offset 48 8) 3
                          (semantics (tbl aes-tag-size-map)))
          (‣ aes-core-select (offset 48 11) 1)
          (‣ aes-otp-key-source
             (offset 48 12)
             1
             (semantics (tbl aes-otp-key-source-map)))))))

(define reg:sts-cfg
  (register
   (name 'sts-cfg)
   (address 2)
   (description
    "Scrambled Timestamp Sequqnce Configuration and Status Registers")
   (items
    (list (‣ sts-length (offset 0 0) 8)
          (‣ reserved (offset 0 8) 8 (default 16))
          (‣ load-sts-aes-init-vector (offset 4 0) 1)
          (‣ restart-sts-from-last-count (offset 4 1) 1)
          (‣ reserved (offset 4 2) 6)
          (‣ sts-acc-qual (offset 8 0) 12)
          (‣ reserved (offset 8 12) 4)
          (‣ sts-key (offset 12 0) (octets 16))
          (‣ sts-init-vector (offset 28 0) (octets 16))))))

(define reg:rx-tune
  (register
   (name 'rx-tune)
   (address 3)
   (description "Receiver Tuning Parameters")
   (items
    (list (‣ rx-tuning-enable (offset 24 0) 1 (default #t))
          (‣ reserved (offset 24 1) 8)
          (‣ rx-tuning-threshold (offset 24 9) 6 (default 56))
          (‣ reserved (offset 24 15) 1 (default #t))
          (‣ dgc-cfg-0 (offset 28 0) (octets 4) (default 268436032))
          (‣ dgc-cfg-1 (offset 32 0) (octets 4) (default 460170377))
          (‣ dgc-lut-0 (offset 56 0) (octets 4) (default 114941))
          (‣ dgc-lut-1 (offset 60 0) (octets 4) (default 115774))
          (‣ dgc-lut-2 (offset 64 0) (octets 4) (default 116414))
          (‣ dgc-lut-3 (offset 68 0) (octets 4) (default 116606))
          (‣ dgc-lut-4 (offset 72 0) (octets 4) (default 118582))
          (‣ dgc-lut-5 (offset 76 0) (octets 4) (default 118709))
          (‣ dgc-lut-6 (offset 80 0) (octets 4) (default 118773))
          (‣ reserved (offset 96 0) 28)
          (‣ dgc-decision-index (offset 96 28) 3)
          (‣ reserved (offset 96 31) 1)))))

(define reg:ext-sync
  (register
   (name 'ext-sync)
   (address 4)
   (description "External sync control and RX calibration")
   (items
    (list (‣ reserved (offset 0 0) 3 (default 4))
          (‣ osts-wait-counter (offset 0 3) 8)
          (‣ ext-timebase-rst-mode (offset 0 11) 1)
          (‣ reserved (offset 0 12) 20)
          (‣ rx-calibration-mode (offset 12 0) 2)
          (‣ reserved (offset 12 2) 2)
          (‣ rx-calibration-enable (offset 12 4) 1)
          (‣ reserved (offset 12 5) 11)
          (‣ rx-calibration-tuning-val (offset 12 16) 4)
          (‣ reserved (offset 12 20) 12)
          (‣ rx-cal-block-result-i (offset 20 0) 29)
          (‣ reserved (offset 20 29) 3)
          (‣ rx-cal-block-result-q (offset 28 0) 29)
          (‣ reserved (offset 28 29) 3)
          (‣ rx-cal-block-status (offset 32 0) 1)
          (‣ reserved (offset 32 1) 7)))))

(define reg:gpio-ctrl
  (register
   (name 'gpio-ctrl)
   (address 5)
   (description "General Purpose Input-Output control registers")
   (items
    (list (‣ mode-select-gpio-0 (offset 0 0) 3
                                (semantics (tbl gpio-0-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-1 (offset 0 3) 3
                                (semantics (tbl gpio-1-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-2 (offset 0 6) 3
                                (semantics (tbl gpio-2-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-3 (offset 0 9) 3
                                (semantics (tbl gpio-3-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-4 (offset 0 12) 3
                                (semantics (tbl gpio-4-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-5 (offset 0 15) 3
                                (semantics (tbl gpio-5-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-6 (offset 0 18) 3
                                (semantics (tbl gpio-6-modes
                                                #:default 'gpio)))
          (‣ mode-select-gpio-7 (offset 0 21) 3
                                (semantics (tbl gpio-7-modes
                                                #:default 'sync-input)))
          (‣ mode-select-gpio-8 (offset 0 24) 3
                                (semantics (tbl gpio-8-modes
                                                #:default 'irq-output)))
          (‣ reserved (offset 0 27) 5)
          (‣ gpio-0-pull-enable (offset 4 0) 1)
          (‣ gpio-1-pull-enable (offset 4 1) 1)
          (‣ gpio-2-pull-enable (offset 4 2) 1)
          (‣ gpio-3-pull-enable (offset 4 3) 1)
          (‣ gpio-4-pull-enable (offset 4 4) 1)
          (‣ gpio-5-pull-enable (offset 4 5) 1)
          (‣ gpio-6-pull-enable (offset 4 6) 1)
          (‣ gpio-7-pull-enable (offset 4 7) 1)
          (‣ gpio-8-pull-enable (offset 4 8) 1)
          (‣ reserved (offset 4 9) 7)
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
          (‣ gpio-4-direction (offset 8 4) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ gpio-5-direction (offset 8 5) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ gpio-6-direction (offset 8 6) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ gpio-7-direction (offset 8 7) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ gpio-8-direction (offset 8 8) 1
                              (semantics (tbl gpio-direction-map
                                              #:default 'input)))
          (‣ reserved (offset 8 9) 7)
          (‣ gpio-0-output-value (offset 12 0) 1)
          (‣ gpio-1-output-value (offset 12 1) 1)
          (‣ gpio-2-output-value (offset 12 2) 1)
          (‣ gpio-3-output-value (offset 12 3) 1)
          (‣ gpio-4-output-value (offset 12 4) 1)
          (‣ gpio-5-output-value (offset 12 5) 1)
          (‣ gpio-6-output-value (offset 12 6) 1)
          (‣ gpio-7-output-value (offset 12 7) 1)
          (‣ gpio-8-output-value (offset 12 8) 1)
          (‣ reserved (offset 12 9) 7)
          (‣ gpio-0-irq-enable (offset 16 0) 1)
          (‣ gpio-1-irq-enable (offset 16 1) 1)
          (‣ gpio-2-irq-enable (offset 16 2) 1)
          (‣ gpio-3-irq-enable (offset 16 3) 1)
          (‣ gpio-4-irq-enable (offset 16 4) 1)
          (‣ gpio-5-irq-enable (offset 16 5) 1)
          (‣ gpio-6-irq-enable (offset 16 6) 1)
          (‣ gpio-7-irq-enable (offset 16 7) 1)
          (‣ gpio-8-irq-enable (offset 16 8) 1)
          (‣ reserved (offset 16 9) 7)
          (‣ gpio-0-irq-status (offset 20 0) 1)
          (‣ gpio-1-irq-status (offset 20 1) 1)
          (‣ gpio-2-irq-status (offset 20 2) 1)
          (‣ gpio-3-irq-status (offset 20 3) 1)
          (‣ gpio-4-irq-status (offset 20 4) 1)
          (‣ gpio-5-irq-status (offset 20 5) 1)
          (‣ gpio-6-irq-status (offset 20 6) 1)
          (‣ gpio-7-irq-status (offset 20 7) 1)
          (‣ gpio-8-irq-status (offset 20 8) 1)
          (‣ reserved (offset 20 9) 7)
          (‣ gpio-0-irq-sense-mode (offset 24 0) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-1-irq-sense-mode (offset 24 1) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-2-irq-sense-mode (offset 24 2) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-3-irq-sense-mode (offset 24 3) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-4-irq-sense-mode (offset 24 4) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-5-irq-sense-mode (offset 24 5) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-6-irq-sense-mode (offset 24 6) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-7-irq-sense-mode (offset 24 7) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ gpio-8-irq-sense-mode (offset 24 8) 1
                                   (semantics
                                    (tbl gpio-irq-mode-map
                                         #:default
                                         'active-high/rising-edge)))
          (‣ reserved (offset 24 9) 7)
          (‣ gpio-0-irq-mode (offset 28 0) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-1-irq-mode (offset 28 1) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-2-irq-mode (offset 28 2) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-3-irq-mode (offset 28 3) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-4-irq-mode (offset 28 4) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-5-irq-mode (offset 28 5) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-6-irq-mode (offset 28 6) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-7-irq-mode (offset 28 7) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ gpio-8-irq-mode (offset 28 8) 1
                             (semantics (tbl gpio-irq-level/edge-map
                                             #:default 'level)))
          (‣ reserved (offset 28 9) 23)
          (‣ gpio-0-bothedge-enable (offset 32 0) 1)
          (‣ gpio-1-bothedge-enable (offset 32 1) 1)
          (‣ gpio-2-bothedge-enable (offset 32 2) 1)
          (‣ gpio-3-bothedge-enable (offset 32 3) 1)
          (‣ gpio-4-bothedge-enable (offset 32 4) 1)
          (‣ gpio-5-bothedge-enable (offset 32 5) 1)
          (‣ gpio-6-bothedge-enable (offset 32 6) 1)
          (‣ gpio-7-bothedge-enable (offset 32 7) 1)
          (‣ gpio-8-bothedge-enable (offset 32 8) 1)
          (‣ reserved (offset 32 9) 7)
          (‣ gpio-0-irq-latch-clear (offset 36 0) 1)
          (‣ gpio-1-irq-latch-clear (offset 36 1) 1)
          (‣ gpio-2-irq-latch-clear (offset 36 2) 1)
          (‣ gpio-3-irq-latch-clear (offset 36 3) 1)
          (‣ gpio-4-irq-latch-clear (offset 36 4) 1)
          (‣ gpio-5-irq-latch-clear (offset 36 5) 1)
          (‣ gpio-6-irq-latch-clear (offset 36 6) 1)
          (‣ gpio-7-irq-latch-clear (offset 36 7) 1)
          (‣ gpio-8-irq-latch-clear (offset 36 8) 1)
          (‣ reserved (offset 36 9) 7)
          (‣ gpio-0-debounce-enable (offset 40 0) 1)
          (‣ gpio-1-debounce-enable (offset 40 1) 1)
          (‣ gpio-2-debounce-enable (offset 40 2) 1)
          (‣ gpio-3-debounce-enable (offset 40 3) 1)
          (‣ gpio-4-debounce-enable (offset 40 4) 1)
          (‣ gpio-5-debounce-enable (offset 40 5) 1)
          (‣ gpio-6-debounce-enable (offset 40 6) 1)
          (‣ gpio-7-debounce-enable (offset 40 7) 1)
          (‣ gpio-8-debounce-enable (offset 40 8) 1)
          (‣ reserved (offset 40 9) 7)
          (‣ gpio-0-io-raw-state (offset 44 0) 1)
          (‣ gpio-1-io-raw-state (offset 44 1) 1)
          (‣ gpio-2-io-raw-state (offset 44 2) 1)
          (‣ gpio-3-io-raw-state (offset 44 3) 1)
          (‣ gpio-4-io-raw-state (offset 44 4) 1)
          (‣ gpio-5-io-raw-state (offset 44 5) 1)
          (‣ gpio-6-io-raw-state (offset 44 6) 1)
          (‣ gpio-7-io-raw-state (offset 44 7) 1)
          (‣ gpio-8-io-raw-state (offset 44 8) 1)
          (‣ reserved (offset 44 9) 7)))))

(define reg:digital-rx-cfg
  (register
   (name 'digital-rx-cfg)
   (address 6)
   (description "Digital receiver configuration")
   (items
    (list (‣ preamble-aquisition-chunk-size (offset 0 0) 2)
          (‣ reserved (offset 0 2) 2)
          (‣ digital-tuning-bit-4 (offset 0 4) 1)
          (‣ reserved (offset 0 5) 11)
          (‣ sfd-detection-timeout (offset 2 0) 16 (default 65))
          (‣ preamble-detection-timeout (offset 4 0) 16)
          (‣ digital-receiver-tuning-word-3 (offset 12 0) 32
                                            (default 2942261324))
          (‣ reserved (offset 20 0) 32)
          (‣ remote-tx-freq-offset-estimate (offset 41 0) 24)))))

(define reg:analog-rf-cfg
  (register
   (name 'analog-rf-cfg)
   (address 7)
   (description "Analog RF configuration block")
   (items
    (list (‣ rf-enable (offset 0 0) 32 (default 33569792))
          (‣ rf-ctrl-mask (offset 4 0) 32 (default 33569792))
          (‣ antenna-auto-toggle-enable (offset 20 0) 1
                                        (semantics boolean/active-low)
                                        (default #t))
          (‣ pdoa-starting-port-select (offset 20 1) 1)
          (‣ reserved (offset 20 2) 6)
          (‣ manual-antenna-switch-ctrl (offset 20 8) 1)
          (‣ reserved (offset 20 9) 3)
          (‣ antenna-switch-ctrl (offset 20 12) 3)
          (‣ reserved (offset 20 15) 1)
          (‣ manual-txrx-switch (offset 20 16) 1)
          (‣ reserved (offset 20 17) 7)
          (‣ txrx-switch-ctrl (offset 20 24) 6)
          (‣ reserved (offset 20 30) 2)
          (‣ analog-tx-ctrl-1 (offset 26 0) 8 (default 14))
          (‣ pulse-generator-delay (offset 28 0) 6 (default 52))
          (‣ reserved (offset 28 6) 26 (default 7347268))
          (‣ tx-test-select (offset 40 0) 4)
          (‣ reserved (offset 40 4) 4)
          (‣ reserved (offset 52 0) 2)
          (‣ sar-temperature-read-enable (offset 52 2) 1)
          (‣ reserved (offset 52 3) 5)
          (‣ internal-ldo-tuning-word (offset 64 0) 60)
          (‣ reserved (offset 64 60) 4)
          (‣ ldo-control (offset 72 0) 32)
          (‣ ldo-tuning-word (offset 81 0) 8 (default 20))))))

(define reg:tx-calibration
  (register
   (name 'tx-calibration)
   (address 8)
   (description "Transmitter calibration block")
   (items
    (list (‣ sar-start (offset 0 0) 1)
          (‣ reserved (offset 0 1) 7)
          (‣ sar-done (offset 4 0) 1)
          (‣ reserved (offset 4 1) 15)
          (‣ sar-voltage (offset 8 0) 8)
          (‣ sar-temperature (offset 8 8) 8)
          (‣ reserved (offset 8 16) 8)
          (‣ sar-wake-up-voltage (offset 12 0) 8)
          (‣ sar-wake-up-temperature (offset 12 8) 8)
          (‣ pulsegen-calibration-start (offset 16 0) 1)
          (‣ pulsegen-auto-calibration-start (offset 16 1) 1)
          (‣ pulsegen-calibration-time (offset 16 2) 4)
          (‣ reserved (offset 16 6) 10)
          (‣ pulsegen-delay (offset 20 0) 12)
          (‣ pulsegen-delay-autocal-done (offset 20 12) 1)
          (‣ reserved (offset 20 13) 3)
          (‣ pulsegen-mode (offset 24 0) 16)
          (‣ pulsegen-target-value (offset 28 0) 12 (default 166))
          (‣ reserved (offset 28 12) 4)))))

(define reg:freq-synth-ctrl
  (register
   (name 'freq-synth-ctrl)
   (address 9)
   (description "Frequency synthesizer control")
   (items
    (list (‣ pll-config (offset 0 0) 16 (default 7996))
          (‣ pll-coarse-code-ch9 (offset 4 0) 8 (default 11))
          (‣ pll-coarse-code-ch5 (offset 4 14) 8 (default 15))
          (‣ reserved (offset 4 22) 10)
          (‣ reserved (offset 8 0) 1)
          (‣ pll-cal-use-old (offset 8 1) 1)
          (‣ reserved (offset 8 2) 2)
          (‣ pll-cal-config (offset 8 4) 4 (default 49))
          (‣ pll-cal-enable (offset 8 8) 1)
          (‣ reserved (offset 8 9) 7)
          (‣ xtal-trim-value (offset 20 0) 6)
          (‣ reserved (offset 20 6) 2)))))

(define reg:always-on-system-control
  (register
   (name 'always-on-system-control)
   (address 10)
   (description "Always-on system control interface")
   (items
    (list (‣ wake-up-always-on-data-download (offset 0 0) 1)
          (‣ wake-up-run-sar (offset 0 1) 1)
          (‣ reserved (offset 0 2) 6)
          (‣ wake-up-goto-pll-from-idle (offset 0 8) 1)
          (‣ wake-up-goto-rx-from-pll (offset 0 9) 1)
          (‣ reserved (offset 0 10) 1)
          (‣ wake-up-run-rx-calibration (offset 0 11) 1)
          (‣ reserved (offset 0 12) 12)
          (‣ always-on-restore (offset 4 0) 1)
          (‣ always-on-save (offset 4 1) 1)
          (‣ always-on-cfg-upload (offset 4 2) 1)
          (‣ always-on-direct-access-read (offset 4 3) 1)
          (‣ always-on-direct-access-write (offset 4 4) 1)
          (‣ always-on-direct-access-write-high (offset 4 5) 1)
          (‣ reserved (offset 4 6) 1)
          (‣ always-on-direct-access-enable (offset 4 7) 1)
          (‣ always-on-access-read-data (offset 8 0) 8)
          (‣ always-on-access-address (offset 12 0) 16)
          (‣ always-on-access-write-data (offset 16 0) 8)
          (‣ always-on-sleep-enable (offset 20 0) 1)
          (‣ always-on-wake-count (offset 20 1) 1)
          (‣ always-on-brownout-detect-enable (offset 20 2) 1
                                              (default #t))
          (‣ always-on-wake-on-spi (offset 20 3) 1 (default #t))
          (‣ always-on-wake-on-wakeup-pin (offset 20 4) 1)
          (‣ always-on-preserve-sleep (offset 20 5) 1)
          (‣ reserved (offset 20 6) 2)))))

(define reg:otp-interface
  (register
   (name 'otp-interface)
   (address 11)
   (description "OTP memory interface")
   (items
    (list (‣ otp-write-data-word (offset 0 0) 32)
          (‣ otp-access-address (offset 4 0) 11)
          (‣ reserved (offset 4 11) 5)
          (‣ otp-manual-control (offset 8 0) 1)
          (‣ otp-read-enable (offset 8 1) 1)
          (‣ otp-write-enable (offset 8 2) 1)
          (‣ otp-write-mode (offset 8 3) 1)
          (‣ reserved (offset 8 4) 2)
          (‣ otp-kick-dgc (offset 8 6) 1)
          (‣ otp-kick-ldo (offset 8 7) 1)
          (‣ reserved (offset 8 8) 2)
          (‣ otp-kick-bias (offset 8 10) 1)
          (‣ otp-kick-ops (offset 8 11) 2)
          (‣ otp-kick-ops-params (offset 8 13) 1)
          (‣ reserved (offset 8 14) 2)
          (‣ otp-programming-done (offset 12 0) 1)
          (‣ otp-vpp-ok (offset 12 1) 1)
          (‣ reserved (offset 12 2) 6)
          (‣ otp-read-data-word (offset 16 0) 32)
          (‣ otp-special-read-data-word (offset 20 0) 32)))))

(define reg:cia-0
  (register
   (name 'cia-0)
   (address 12)
   (description "Channel Impulse response Analyser (CIA) Interface Part 0")
   (items
    (list (‣ ipatov-toa (offset 0 0) 40)
          (‣ ipatov-poa (offset 0 40) 14)
          (‣ reserved (offset 0 54) 2)
          (‣ ipatov-toa-status (offset 0 56) 8)
          (‣ sts0-toa (offset 8 0) 40)
          (‣ sts0-poa (offset 8 40) 14)
          (‣ reserved (offset 8 54) 2)
          (‣ sts0-toast-fp-no-strong-edge (offset 8 56) 1)
          (‣ sts0-toast-noise-thresh-lowered (offset 8 57) 1)
          (‣ sts0-toast-cir-too-weak (offset 8 58) 1)
          (‣ sts0-toast-cfp-too-close (offset 8 59) 1)
          (‣ sts0-toast-fp-too-close (offset 8 60) 1)
          (‣ sts0-toast-cq-enable (offset 8 61) 1)
          (‣ sts0-toast-ss-enable (offset 8 62) 1)
          (‣ sts0-toast-pgr-enable (offset 8 63) 1)
          (‣ sts1-toa (offset 16 0) 40)
          (‣ sts1-poa (offset 16 40) 14)
          (‣ reserved (offset 16 54) 2)
          (‣ sts1-toast-fp-no-strong-edge (offset 16 56) 1)
          (‣ sts1-toast-noise-thresh-lowered (offset 16 57) 1)
          (‣ sts1-toast-cir-too-weak (offset 16 58) 1)
          (‣ sts1-toast-cfp-too-close (offset 16 59) 1)
          (‣ sts1-toast-fp-too-close (offset 16 60) 1)
          (‣ sts1-toast-cq-enable (offset 16 61) 1)
          (‣ sts1-toast-ss-enable (offset 16 62) 1)
          (‣ sts1-toast-pgr-enable (offset 16 63) 1)
          (‣ tdoa (offset 24 0) (octets 6))
          (‣ pdoa (offset 30 0) 14)
          (‣ fp-thresh-test (offset 30 14) 1)
          (‣ reserved (offset 30 15) 1)
          (‣ clock-offs-est (offset 32 0) 13)
          (‣ reserved (offset 32 13) 19)
          (‣ reserved (offset 36 0) 32)
          (‣ preamble-peak-amp (offset 40 0) 21)
          (‣ preamble-peak-index (offset 40 21) 10)
          (‣ reserved (offset 40 31) 1)
          (‣ preamble-channel-area (offset 44 0) 17)
          (‣ reserved (offset 44 17) 15)
          (‣ preamble-fp1-magnitude (offset 48 0) 22)
          (‣ reserved (offset 48 22) 10)
          (‣ preamble-fp2-magnitude (offset 52 0) 22)
          (‣ reserved (offset 52 22) 10)
          (‣ preamble-fp3-magnitude (offset 56 0) 22)
          (‣ reserved (offset 56 22) 10)
          (‣ reserved (offset 60 0) (octets 12))
          (‣ preamble-fp-index (offset 72 0) 16)
          (‣ reserved (offset 72 16) 16)
          (‣ reserved (offset 76 0) (octets 12))
          (‣ preamble-accumulated-symbols (offset 88 0) 12)
          (‣ reserved (offset 88 12) 20)
          (‣ sts0-peak-amplitude (offset 92 0) 21)
          (‣ sts0-peak-index (offset 92 21) 9)
          (‣ reserved (offset 92 30) 2)
          (‣ sts0-channel-area (offset 96 0) 16)
          (‣ reserved (offset 96 16) 16)
          (‣ sts0-fp1-magnitude (offset 100 0) 22)
          (‣ reserved (offset 100 22) 10)
          (‣ sts0-fp2-magnitude (offset 104 0) 22)
          (‣ reserved (offset 104 22) 10)))))

(define reg:cia-1
  (register
   (name 'cia-1)
   (address 13)
   (description "CIA Interface Part 1")
   (items
    (list (‣ sts0-fp3-magnitude (offset 0 0) 22)
          (‣ reserved (offset 0 22) 10)
          (‣ reserved (offset 4 0) (octets 12))
          (‣ sts0-fp-index (offset 16 0) 15)
          (‣ reserved (offset 16 15) 17)
          (‣ reserved (offset 20 0) (octets 12))
          (‣ sts0-accumulated-symbols (offset 32 0) 11)
          (‣ reserved (offset 32 11) 21)
          (‣ reserved (offset 36 0) (octets 20))
          (‣ sts1-peak-amplitude (offset 56 0) 21)
          (‣ sts1-peak-index (offset 56 21) 9)
          (‣ reserved (offset 56 30) 2)
          (‣ sts1-channel-area (offset 60 0) 16)
          (‣ reserved (offset 60 16) 16)
          (‣ sts1-fp1-magnitude (offset 64 0) 22)
          (‣ reserved (offset 64 22) 10)
          (‣ sts1-fp2-magnitude (offset 68 0) 22)
          (‣ reserved (offset 68 22) 10)
          (‣ sts1-fp3-magnitude (offset 72 0) 22)
          (‣ reserved (offset 72 22) 10)
          (‣ reserved (offset 76 0) (octets 12))
          (‣ sts1-fp-index (offset 88 0) 15)
          (‣ reserved (offset 88 15) 17)
          (‣ reserved (offset 92 0) (octets 12))
          (‣ sts1-accumulated-symbols (offset 104 0) 11)
          (‣ reserved (offset 104 11) 21)))))

(define reg:cia-2-and-rx-antenna-delay
  (register
   (name 'cia-2-and-rx-antenna-delay)
   (address 14)
   (description "CIA Interface Part 2 and RX Antenna delay")
   (items
    (list (‣ rx-antenna-delay (offset 0 0) 16 (default 16405))
          (‣ reserved (offset 0 16) 4 (default 1))
          (‣ minimal-diag-enable (offset 0 20) 1 (default #t))
          (‣ reserved (offset 0 21) 11)
          (‣ reserved (offset 4 0) 8 (default 16))
          (‣ fp-agreed-threshold (offset 4 8) 3 (default 3))
          (‣ calibration-temp (offset 4 11) 8)
          (‣ reserved (offset 4 19) 1 (default #t))
          (‣ ant-delay-temp-comp (offset 4 20) 1 (default #t))
          (‣ reserved (offset 4 21) 11 (default 3))))))

(define reg:digital-diag
  (register
   (name 'digital-diag)
   (address 15)
   (description "Digital diagnostics interface")
   (width (octets 77))
   (items
    (list (‣ event-counters-enable (offset 0 0) 1)
          (‣ event-counters-clear (offset 0 1) 1)
          (‣ reserved (offset 0 2) 6)
          (‣ phr-error-counter (offset 4 0) 12)
          (‣ reserved (offset 4 12) 4)
          (‣ rsd-error-counter (offset 6 0) 12)
          (‣ reserved (offset 6 12) 4)
          (‣ fcs-good-counter (offset 8 0) 12)
          (‣ reserved (offset 8 12) 4)
          (‣ fcs-error-counter (offset 10 0) 12)
          (‣ reserved (offset 10 12) 4)
          (‣ frame-filter-rejection (offset 12 0) 8)
          (‣ rx-overrun-error-counter (offset 14 0) 8)
          (‣ sfd-timeout-counter (offset 16 0) 12)
          (‣ reserved (offset 16 12) 4)
          (‣ preamble-detect-timeout-counter (offset 18 0) 12)
          (‣ reserved (offset 18 12) 4)
          (‣ rx-frame-timeout-counter (offset 20 0) 8)
          (‣ tx-frame-counter (offset 22 0) 12)
          (‣ reserved (offset 22 12) 4)
          (‣ half-period-warning-counter (offset 24 0) 8)
          (‣ spi-write-crc-error-counter (offset 26 0) 8)
          (‣ reserved (offset 28 0) (octets 8))
          (‣ reserved (offset 36 0) 4)
          (‣ tx-power-spectrum-test-mode-enable (offset 36 4) 1)
          (‣ reserved (offset 36 5) 16)
          (‣ host-irq-polarity (offset 36 21) 1)
          (‣ reserved (offset 36 22) 2)
          (‣ cia-watchdog-enable (offset 36 24) 1)
          (‣ reserved (offset 36 25) 1)
          (‣ cia-manual-run-enable (offset 36 26) 1)
          (‣ reserved (offset 36 27) 5)
          (‣ sta-quality-error-counter (offset 40 0) 8)
          (‣ low-voltage-warning-counter (offset 42 0) 8)
          (‣ spi-mode-clk-polarity (offset 44 0) 1)
          (‣ spi-mode-clk-phase (offset 44 1) 1)
          (‣ reserved (offset 44 2) 6)
          (‣ tx-state (offset 48 0) 4)
          (‣ reserved (offset 48 4) 4)
          (‣ rx-state (offset 48 8) 6)
          (‣ reserved (offset 48 14) 2)
          (‣ psmc-state (offset 48 16) 5)
          (‣ reserved (offset 48 21) 11)
          (‣ fast-command-status (offset 60 0) 5)
          (‣ reserved (offset 60 5) 3)
          (‣ counter-debug-sts-iv (offset 72 0) (octets 4))
          (‣ spi-crc-lfsr-init-code (offset 76 0) 8)))))

(define reg:pmsc
  (register
   (name 'pmsc)
   (address 17)
   (description "PMSC control and status")
   (items
    (list (‣ soft-arm-reset (offset 0 0) 1)
          (‣ soft-prgn-reset (offset 0 1) 1)
          (‣ soft-cia-reset (offset 0 2) 1)
          (‣ soft-bist-reset (offset 0 3) 1)
          (‣ soft-rx-reset (offset 0 4) 1)
          (‣ soft-tx-reset (offset 0 5) 1)
          (‣ soft-hif-reset (offset 0 6) 1)
          (‣ soft-pmsc-reset (offset 0 7) 1)
          (‣ soft-gpio-reset (offset 0 8) 1)
          (‣ reserved (offset 0 9) 7)
          (‣ sys-clock-ctrl (offset 4 0) 2)
          (‣ rx-clock-ctrl (offset 4 2) 2)
          (‣ tx-clock-ctrl (offset 4 4) 2)
          (‣ acc-clock-enable (offset 4 6) 1)
          (‣ reserved (offset 4 7) 1)
          (‣ cia-clock-enable (offset 4 8) 1)
          (‣ reserved (offset 4 9) 1 (default #t))
          (‣ sar-clock-enable (offset 4 10) 1)
          (‣ reserved (offset 4 11) 4)
          (‣ acc-memory-clock-enable (offset 4 15) 1)
          (‣ gpio-clock-enable (offset 4 16) 1)
          (‣ reserved (offset 4 17) 1)
          (‣ gpio-debounce-clock-enable (offset 4 18) 1)
          (‣ gpio-debounce-reset (offset 4 19) 1
                                 (semantics boolean/active-low)
                                 (default #f))
          (‣ reserved (offset 4 20) 3 (default 3))
          (‣ kilohertz-clock-enable (offset 4 23) 1)
          (‣ reserved (offset 4 24) 8 (default 240))
          (‣ reserved (offset 8 0) 8 (default 56))
          (‣ auto-idle-rc-to-pll (offset 8 8) 1)
          (‣ reserved (offset 8 9) 2 (default 3))
          (‣ auto-sleep-after-tx (offset 8 11) 1)
          (‣ auto-sleep-after-rx (offset 8 12) 1)
          (‣ reserved (offset 8 13) 2)
          (‣ external-sync-pll-enable (offset 8 15) 1)
          (‣ reserved (offset 8 16) 1)
          (‣ cia-run-enable (offset 8 17) 1)
          (‣ reserved (offset 8 18) 5)
          (‣ force-idle-rc-state (offset 8 23) 1)
          (‣ reserved (offset 8 24) 2)
          (‣ kilohertz-clock-divider (offset 8 26) 6)
          (‣ tx-fine-pwr-sequencing (offset 18 0) 32 (default 80906356))
          (‣ led-blink-time (offset 22 0) 8 (default 32))
          (‣ led-blink-enable (offset 22 8) 1)
          (‣ reserved (offset 22 9) 7)
          (‣ led-0-force-blink (offset 22 16) 1)
          (‣ led-1-force-blink (offset 22 17) 1)
          (‣ led-2-force-blink (offset 22 18) 1)
          (‣ led-3-force-blink (offset 22 19) 1)
          (‣ reserved (offset 22 20) 12)
          (‣ sniff-mode-on-time (offset 26 0) 4)
          (‣ reserved (offset 26 4) 4)
          (‣ sniff-mode-off-time (offset 26 8) 8)
          (‣ reserved (offset 26 16) 16)
          (‣ bias-control (offset 31 0) 16)))))

(define reg:rx-buffer-0
  (register
   (name 'rx-buffer-0)
   (address 18)
   (description "RX frame data buffer 0")
   (width (octets 1024))
   (items
    (list (‣ rx-frame-buffer-0 (offset 0 0) (octets 1024))))))

(define reg:rx-buffer-1
  (register
   (name 'rx-buffer-1)
   (address 19)
   (description "RX frame data buffer 1")
   (width (octets 1024))
   (items
    (list (‣ rx-frame-buffer-1 (offset 0 0) (octets 1024))))))

(define reg:tx-buffer
  (register
   (name 'tx-buffer)
   (address 20)
   (description "Transmit data buffer")
   (width (octets 1024))
   (items
    (list (‣ tx-data-buffer (offset 0 0) (octets 1024))))))

(define reg:acc-mem
  (register
   (name 'acc-mem)
   (address 21)
   (description "Accumulator CIR memory")
   (width (octets 12288))
   (items
    (list (‣ cir-accumulator (offset 0 0) (octets 12288))))))

(define reg:scratch-ram
  (register
   (name 'scratch-ram)
   (address 22)
   (description "Scratch RAM memory buffer")
   (width (octets 127))
   (items
    (list (‣ scratch-ram (offset 0 0) (octets 127))))))

(define reg:aes-ram
  (register
   (name 'aes-ram)
   (address 23)
   (description "AES key RAM")
   (width (octets 75))
   (items
    (list (‣ aes-key-ram (offset 0 0) (octets 75))))))

(define reg:set-1/set-2
  (register
   (name 'set-1/set-2)
   (address 24)
   (description "Double Buffer diagnostic register set")
   (width (octets 460))
   (items
    (list (‣ db-diag-reg (offset 0 0) (octets 460))))))

(define reg:indirect-ptr-a
  (register
   (name 'indirect-ptr-a)
   (address 29)
   (description "Indirect pointer A")
   (width (octets 4))
   (items
    (list (‣ indirect-ptr-a (offset 0 0) (octets 4))))))

(define reg:indirect-ptr-b
  (register
   (name 'indirect-ptr-b)
   (address 30)
   (description "Indirect pointer B")
   (width (octets 4))
   (items
    (list (‣ indirect-ptr-b (offset 0 0) (octets 4))))))

(define reg:in-ptr-cfg
  (register
   (name 'in-ptr-cfg)
   (address 31)
   (description "FINT status and indirect pointer interface")
   (items
    (list (‣ tx-ok-status (offset 0 0) 1)
          (‣ cca-fail-status (offset 0 1) 1)
          (‣ rxter-error-status (offset 0 2) 1)
          (‣ rx-ok-status (offset 0 3) 1)
          (‣ rx-error-status (offset 0 4) 1)
          (‣ rx-timeout-status (offset 0 5) 1)
          (‣ sys-event-status (offset 0 6) 1)
          (‣ sys-panic-status (offset 0 7) 1)
          (‣ ptr-a-base-addr (offset 4 0) 5)
          (‣ reserved (offset 4 5) 3)
          (‣ ptr-a-offset (offset 8 0) 15)
          (‣ reserved (offset 8 15) 1)
          (‣ ptr-b-base-addr (offset 12 0) 5)
          (‣ reserved (offset 12 5) 3)
          (‣ ptr-b-offset (offset 16 0) 15)
          (‣ reserved (offset 16 15) 1)))))
