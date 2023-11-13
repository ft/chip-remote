;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices decawave dw3000 registers)
  #:use-module (chip-remote codecs)
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

(define (octets n)
  (* n 8))

(define (offset byte bit)
  (+ (* byte 8) bit))

(define-register reg:general-cfg
  #:address #x00
  #:description "General Configuration Registers"
  #:double-buffer? #f
  #:register-width (octets 121)
  #:contents
  ;; 0x00: DEV_ID
  (device-revision 0  4)
  (device-version  4  4)
  (device-model    8  8)
  (device-tag     16 16)
  ;; 0x04: EUID
  (ieee-eui-device       (offset #x04          0) (octets 5))
  (ieee-eui-manufacturer (offset #x04 (octets 5)) (octets 3))
  ;; 0x0C: PANADR
  (short-address (offset #x0c          0) (octets 2) #:default #xffff)
  (pan-id        (offset #x0c (octets 2)) (octets 2) #:default #xffff)
  ;; 0x10: SYS_CFG
  (frame-filtering-enable             (offset #x10  0)  1)
  (disable-fcs-tx                     (offset #x10  1)  1)
  (disable-frame-check-error-handling (offset #x10  2)  1)
  (disable-double-rx-buf              (offset #x10  3)  1 #:default #t)
  (phr-mode                           (offset #x10  4)  1)
  (phr-6m8                            (offset #x10  5)  1)
  (spi-crc-enable                     (offset #x10  6)  1)
  (cia-ipatov-processing              (offset #x10  7)  1 #:default #t)
  (cia-sts-processing                 (offset #x10  8)  1 #:default #t)
  (rx-wait-timeout-enable             (offset #x10  9)  1)
  (rx-auto-reenable                   (offset #x10 10)  1)
  (auto-ack                           (offset #x10 11)  1)
  (sts-packet-configuration           (offset #x10 12)  2 #:default #b01)
  (reserved                           (offset #x10 14)  1)
  (configure-sdc-mode                 (offset #x10 15)  1)
  (pdoa-mode                          (offset #x10 16)  2)
  (fast-aat                           (offset #x10 18)  1 #:default #t)
  (reserved                           (offset #x10 19) 13)
  ;; 0x14: FF_CFG
  (ff-allow-beacon                 (offset #x14  0) 1)
  (ff-allow-data                   (offset #x14  1) 1)
  (ff-allow-ack                    (offset #x14  2) 1)
  (ff-allow-mac                    (offset #x14  3) 1)
  (ff-allow-reserved               (offset #x14  4) 1)
  (ff-allow-multipurpose           (offset #x14  5) 1)
  (ff-allow-frack                  (offset #x14  6) 1)
  (ff-allow-extended               (offset #x14  7) 1)
  (ff-behave-as-coordinator        (offset #x14  8) 1)
  (ff-allow-mac-implicit-broadcast (offset #x14  9) 1)
  (le0-pending                     (offset #x14 10) 1)
  (le1-pending                     (offset #x14 11) 1)
  (le2-pending                     (offset #x14 12) 1)
  (le3-pending                     (offset #x14 13) 1)
  (short-source-adr-enable         (offset #x14 14) 1)
  (long-source-adr-enable          (offset #x14 15) 1)
  ;; 0x18: SPI_RD_CRC
  (spi-crc-read-status (offset #x18 0) 8)
  ;; 0x1C: SYS_TIME
  (system-time (offset #x1c 0) (octets 4))
  ;; 0x24: TX_FCTRL
  (tx-frame-length                (offset #x24  0) 10 #:default 12)
  (tx-bit-rate                    (offset #x24 10)  1 #:default 1)
  (tx-phr-ranging-enable          (offset #x24 11)  1 #:default 1)
  (tx-preamble-symbol-repetitions (offset #x24 12)  4
                                  #:semantics lookup preamble-symbol-rep-map
                                  #:default 64)
  (tx-buf-index-offset            (offset #x24 16) 10)
  (reserved                       (offset #x24 26) 14)
  (fine-psr-ctrl                  (offset #x24 40)  8)
  ;; 0x2C DX_TIME
  (delayed-trx-time (offset #x2c 0) (octets 4))
  ;; 0x30: DREF_TIME
  (delayed-trx-reference-time (offset #x30 0) (octets 4))
  ;; 0x34: RX_FWTO
  (rx-frame-wait-timeout-period (offset #x34 0) (octets 3))
  ;; 0x38: SYS_CTRL
  (continuous-frame-test-start (offset #x38 0) 1)
  (reserved                    (offset #x38 1) 7)
  ;; 0x3C: SYS_ENABLE
  (reserved                                 (offset #x3c  0) 1)
  (cpll-lock-irq-enable                     (offset #x3c  1) 1)
  (spi-crc-error-irq-enable                 (offset #x3c  2) 1)
  (auto-ack-irq-enable                      (offset #x3c  3) 1)
  (tx-frame-begins-irq-enable               (offset #x3c  4) 1)
  (tx-preamble-sent-irq-enable              (offset #x3c  5) 1)
  (tx-phy-header-sent-irq-enable            (offset #x3c  6) 1)
  (tx-frame-sent-irq-enable                 (offset #x3c  7) 1)
  (rx-preamble-detected-irq-enable          (offset #x3c  8) 1)
  (rx-sfd-deteced-irq-enable                (offset #x3c  9) 1)
  (rx-cia-done-irq-enable                   (offset #x3c 10) 1)
  (rx-phy-header-detected-irq-enable        (offset #x3c 11) 1)
  (rx-phy-header-error-irq-enable           (offset #x3c 12) 1)
  (rx-data-ready-irq-enable                 (offset #x3c 13) 1)
  (rx-fcs-good-irq-enable                   (offset #x3c 14) 1)
  (rx-fcs-error-irq-enable                  (offset #x3c 15) 1)
  (rx-rs-frame-sync-loss-irq-enable         (offset #x3c 16) 1)
  (rx-wait-timeout-irq-enable               (offset #x3c 17) 1)
  (cia-error-irq-enable                     (offset #x3c 18) 1)
  (voltage-warning-irq-enable               (offset #x3c 19) 1)
  (rx-overrun-irq-enable                    (offset #x3c 20) 1) 
  (rx-preamble-detection-timeout-irq-enable (offset #x3c 21) 1)
  (reserved                                 (offset #x3c 22) 1)
  (spi-ready-irq-enable                     (offset #x3c 23) 1 #:default #t)
  (idle-rc-irq-enable                       (offset #x3c 24) 1)
  (pll-losing-lock-irq-enable               (offset #x3c 25) 1)
  (rx-sfd-timeout-irq-enable                (offset #x3c 26) 1)
  (hpd-warn-irq-enable                      (offset #x3c 27) 1)
  (sts-error-irq-enable                     (offset #x3c 28) 1)
  (aff-reject-irq-enable                    (offset #x3c 29) 1)
  (reserved                                 (offset #x3c 30) 3)
  (rx-preamble-reject-irq-enable            (offset #x3c 33) 1)
  (reserved                                 (offset #x3c 34) 2)
  (vt-detection-irq-enable                  (offset #x3c 36) 1)
  (gpio-irq-enable                          (offset #x3c 37) 1)
  (aes-done-irq-enable                      (offset #x3c 38) 1)
  (aes-error-irq-enable                     (offset #x3c 39) 1)
  (cmd-error-irq-enable                     (offset #x3c 40) 1 #:default #t)
  (spi-overflow-irq-enable                  (offset #x3c 41) 1 #:default #t)
  (spi-underflow-irq-enable                 (offset #x3c 42) 1 #:default #t)
  (spi-error-irq-enable                     (offset #x3c 43) 1 #:default #t)
  (cca-fail-irq-enable                      (offset #x3c 44) 1)
  ;; 0x44: SYS_STATUS
  (interrupt-request-status          (offset #x44  0) 1)
  (cpll-lock-irq                     (offset #x44  1) 1)
  (spi-crc-error-irq                 (offset #x44  2) 1)
  (auto-ack-irq                      (offset #x44  3) 1)
  (tx-frame-begins-irq               (offset #x44  4) 1)
  (tx-preamble-sent-irq              (offset #x44  5) 1)
  (tx-phy-header-sent-irq            (offset #x44  6) 1)
  (tx-frame-sent-irq                 (offset #x44  7) 1)
  (rx-preamble-detected-irq          (offset #x44  8) 1)
  (rx-sfd-deteced-irq                (offset #x44  9) 1)
  (rx-cia-done-irq                   (offset #x44 10) 1)
  (rx-phy-header-detected-irq        (offset #x44 11) 1)
  (rx-phy-header-error-irq           (offset #x44 12) 1)
  (rx-data-ready-irq                 (offset #x44 13) 1)
  (rx-fcs-good-irq                   (offset #x44 14) 1)
  (rx-fcs-error-irq                  (offset #x44 15) 1)
  (rx-rs-frame-sync-loss-irq         (offset #x44 16) 1)
  (rx-wait-timeout-irq               (offset #x44 17) 1)
  (cia-error-irq                     (offset #x44 18) 1)
  (voltage-warning-irq               (offset #x44 19) 1)
  (rx-overrun-irq                    (offset #x44 20) 1) 
  (rx-preamble-detection-timeout-irq (offset #x44 21) 1)
  (reserved                          (offset #x44 22) 1)
  (spi-ready-irq                     (offset #x44 23) 1)
  (idle-rc-irq                       (offset #x44 24) 1)
  (pll-losing-lock-irq               (offset #x44 25) 1)
  (rx-sfd-timeout-irq                (offset #x44 26) 1)
  (hpd-warn-irq                      (offset #x44 27) 1)
  (sts-error-irq                     (offset #x44 28) 1)
  (aff-reject-irq                    (offset #x44 29) 1)
  (reserved                          (offset #x44 30) 3)
  (rx-preamble-reject-irq            (offset #x44 33) 1)
  (reserved                          (offset #x44 34) 2)
  (vt-detection-irq                  (offset #x44 36) 1)
  (gpio-irq                          (offset #x44 37) 1)
  (aes-done-irq                      (offset #x44 38) 1)
  (aes-error-irq                     (offset #x44 39) 1)
  (cmd-error-irq                     (offset #x44 40) 1)
  (spi-overflow-irq                  (offset #x44 41) 1)
  (spi-underflow-irq                 (offset #x44 42) 1)
  (spi-error-irq                     (offset #x44 43) 1)
  (cca-fail-irq                      (offset #x44 44) 1)
  (reserved                          (offset #x44 45) 3)
  ;; 0x4C: RX_FINFO
  (rx-frame-length         (offset #x4c  0) 10)
  (reserved                (offset #x4c 10)  1)
  (rx-preamble-length-low  (offset #x4c 11)  2)
  (rx-bit-rate             (offset #x4c 13)  1 #:semantics lookup bit-rate-map)
  (reserved                (offset #x4c 14)  1)
  (rx-ranging-bit          (offset #x4c 15)  1)
  (rx-prf-report           (offset #x4c 16)  2 #:semantics lookup prf-map)
  (rx-preamble-length-high (offset #x4c 18)  2)
  (rx-accu-count           (offset #x4c 20) 12)
  ;; 0x64: RX_TIME
  (rx-timestamp     (offset #x64  0) (octets 5))
  (reserved         (offset #x64 40)         24)
  (rx-raw-timestamp (offset #x70  0) (octets 4))
  ;; 0x74: TX_TIME
  (tx-timestamp (offset #x74 0) (octets 5)))

(define-register reg:general-cfg-and-aes
  #:address #x01
  #:description "General Configuration (continued) and AES Registers"
  #:double-buffer? #f
  ;;#:register-width (octets (+ #x54 2))
  #:contents
  ;; 0x00: TX_RAWS
  (tx-raw-timestamp (offset #x00 0) (octets 4))
  ;; 0x04: TX_ANTD
  (tx-antenna-dly (offset #x04 0) (octets 2))
  ;; 0x08: ACK_RESP_TIME
  (wait-for-resp-time (offset #x08  0) 20)
  (ack-time           (offset #x08 24)  8)
  ;; 0x0C TX_POWER (TODO: This form of register probably needs to be handled
  ;; specially)
  (data-tx-power-coarse (offset #x0c  0) 2)
  (data-tx-power-fine   (offset #x0c  2) 6)
  (phr-tx-power-coarse  (offset #x0c  8) 2)
  (phr-tx-power-fine    (offset #x0c 10) 6)
  (shr-tx-power-coarse  (offset #x0c 16) 2)
  (shr-tx-power-fine    (offset #x0c 18) 6)
  (sts-tx-power-coarse  (offset #x0c 24) 2)
  (sts-tx-power-fine    (offset #x0c 26) 6)
  ;; 0x14: CHAN_CTRL
  (rf-channel       (offset #x14 0) 1)
  (sfd-type         (offset #x14 1) 2 #:semantics lookup sfd-type-map)
  (tx-preamble-code (offset #x14 3) 5)
  (rx-preamble-code (offset #x14 8) 5)
  ;; 0x18 LE_PEND_01
  (le-address-0 (offset #x18  0) 16)
  (le-address-1 (offset #x18 16) 16)
  ;; 0x1C LE_PEND_23
  (le-address-2 (offset #x1c  0) 16)
  (le-address-3 (offset #x1c 16) 16)
  ;; 0x20: SPI_COLLISION
  (spi-collision-caused-by (offset #x20 0) 5 #:semantics lookup spi-collision-map)
  ;; 0x24: RDB_STATUS
  (db0-rx-fcs-good   (offset #x24 0) 1)
  (db0-rx-data-ready (offset #x24 1) 1)
  (db0-cia-done      (offset #x24 2) 1)
  (db0-sts-error     (offset #x24 3) 1)
  (db1-rx-fcs-good   (offset #x24 4) 1)
  (db1-rx-data-ready (offset #x24 5) 1)
  (db1-cia-done      (offset #x24 6) 1)
  (db1-sts-error     (offset #x24 7) 1)
  ;; 0x28: RDB-DIAG
  (db-rx-diag-mode (offset #x28 0) 3 #:semantics lookup db-diagnostics-map)
  ;; 0x30: AES_CFG
  (aes-mode            (offset #x30  0) 1 #:semantics lookup aes-mode-map)
  (aes-key-size        (offset #x30  1) 2 #:semantics lookup aes-key-size-map)
  (aes-key-addr-offset (offset #x30  3) 3)
  (load-aes-key        (offset #x30  6) 1)
  (aes-key-source      (offset #x30  7) 1 #:semantics lookup aes-key-source-map)
  (aes-tag-size        (offset #x30  8) 3 #:semantics lookup aes-tag-size-map)
  (aes-core-select     (offset #x30 11) 1)
  (aes-otp-key-source  (offset #x30 12) 1 #:semantics lookup aes-otp-key-source-map))

(define-register reg:sts-cfg
  #:address #x02
  #:description "Scrambled Timestamp Sequqnce Configuration and Status Registers"
  #:double-buffer? #f
  ;;#:register-width (octets 55)
  #:contents
  ;; 0x00: STS_CFG
  (sts-length (offset #x00 0) 8)
  (reserved   (offset #x00 8) 8 #:default #b00010000)
  ;; 0x04: STS_CTRL
  (load-sts-aes-init-vector    (offset #x04 0) 1)
  (restart-sts-from-last-count (offset #x04 1) 1)
  (reserved                    (offset #x04 2) 6)
  ;; 0x08: STS status
  (sts-acc-qual (offset #x08  0) 12)
  (reserved     (offset #x08 12)  4)
  ;; 0x0C: STS Key
  (sts-key (offset #x0c 0) (octets 16))
  ;; 0x1C: STS_IV
  (sts-init-vector (offset #x1c 0) (octets 16)))

(define-register reg:rx-tune
  #:address #x03
  #:description "Receiver Tuning Parameters"
  #:double-buffer? #f
  ;;#:register-width (octets 100)
  #:contents
  ;; 0x18: DGC_CFG
  (rx-tuning-enable    (offset #x18  0) 1 #:default #t)
  (reserved            (offset #x18  1) 8)
  (rx-tuning-threshold (offset #x18  9) 6 #:default #b111000)
  (reserved            (offset #x18 15) 1 #:default #t)
  ;; The following registers parts (dgc-cfg-* and dgc-lut-*) are not documented
  ;; in detail. There's just a setup table with binary values to use with
  ;; channel 5 and channel 9. The transcribed default values are for channel 5.
  (dgc-cfg-0 (offset #x1c 0) (octets 4) #:default #x10000240)
  (dgc-cfg-1 (offset #x20 0) (octets 4) #:default #x1b6da489)
  (dgc-lut-0 (offset #x38 0) (octets 4) #:default #x0001c0fd)
  (dgc-lut-1 (offset #x3c 0) (octets 4) #:default #x0001c43e)
  (dgc-lut-2 (offset #x40 0) (octets 4) #:default #x0001c6be)
  (dgc-lut-3 (offset #x44 0) (octets 4) #:default #x0001c77e)
  (dgc-lut-4 (offset #x48 0) (octets 4) #:default #x0001cf36)
  (dgc-lut-5 (offset #x4c 0) (octets 4) #:default #x0001cfb5)
  (dgc-lut-6 (offset #x50 0) (octets 4) #:default #x0001cff5)
  ;; 0x60: DGC_DBG
  (reserved           (offset #x60  0) 28)
  (dgc-decision-index (offset #x60 28)  3)
  (reserved           (offset #x60 31)  1))

(define-register reg:ext-sync
  #:address #x04
  #:description "External sync control and RX calibration"
  #:double-buffer? #f
  ;;#:register-width (octets 36)
  #:contents
  ;; 0x00: EC_CTRL
  (reserved              (offset #x00  0)  3 #:default #b100)
  (osts-wait-counter     (offset #x00  3)  8)
  (ext-timebase-rst-mode (offset #x00 11)  1)
  (reserved              (offset #x00 12) 20)
  ;; 0x0C: RX_CAL
  (rx-calibration-mode       (offset #x0c  0)  2)
  (reserved                  (offset #x0c  2)  2)
  (rx-calibration-enable     (offset #x0c  4)  1)
  (reserved                  (offset #x0c  5) 11)
  (rx-calibration-tuning-val (offset #x0c 16)  4)
  (reserved                  (offset #x0c 20) 12)
  ;; 0x14: RX_CAL_RESI
  (rx-cal-block-result-i (offset #x14  0) 29)
  (reserved              (offset #x14 29)  3)
  ;; 0x1c: RX_CAL_RESQ
  (rx-cal-block-result-q (offset #x1c  0) 29)
  (reserved              (offset #x1c 29)  3)
  ;; 0x20: RX_CAL_STS
  (rx-cal-block-status (offset #x20 0) 1)
  (reserved            (offset #x20 1) 7))

(define-register reg:gpio-ctrl
  #:address #x05
  #:description "General Purpose Input-Output control registers"
  #:double-buffer #f
  ;;#:register-width (octets 47)
  #:contents
  ;; 0x00: GPIO_MODE
  (mode-select-gpio-0 (offset #x00  0) 3
                      #:semantics lookup gpio-0-modes
                      #:default 'gpio)
  (mode-select-gpio-1 (offset #x00  3) 3
                      #:semantics lookup gpio-1-modes
                      #:default 'gpio)
  (mode-select-gpio-2 (offset #x00  6) 3
                      #:semantics lookup gpio-2-modes
                      #:default 'gpio)
  (mode-select-gpio-3 (offset #x00  9) 3
                      #:semantics lookup gpio-3-modes
                      #:default 'gpio)
  (mode-select-gpio-4 (offset #x00 12) 3
                      #:semantics lookup gpio-4-modes
                      #:default 'gpio)
  (mode-select-gpio-5 (offset #x00 15) 3
                      #:semantics lookup gpio-5-modes
                      #:default 'gpio)
  (mode-select-gpio-6 (offset #x00 18) 3
                      #:semantics lookup gpio-6-modes
                      #:default 'gpio)
  (mode-select-gpio-7 (offset #x00 21) 3
                      #:semantics lookup gpio-7-modes
                      #:default 'sync-input)
  (mode-select-gpio-8 (offset #x00 24) 3
                      #:semantics lookup gpio-8-modes
                      #:default 'irq-output)
  (reserved           (offset #x00 27) 5)
  ;; 0x04: GPIO_PULL_EN
  (gpio-0-pull-enable (offset #x04 0) 1)
  (gpio-1-pull-enable (offset #x04 1) 1)
  (gpio-2-pull-enable (offset #x04 2) 1)
  (gpio-3-pull-enable (offset #x04 3) 1)
  (gpio-4-pull-enable (offset #x04 4) 1)
  (gpio-5-pull-enable (offset #x04 5) 1)
  (gpio-6-pull-enable (offset #x04 6) 1)
  (gpio-7-pull-enable (offset #x04 7) 1)
  (gpio-8-pull-enable (offset #x04 8) 1)
  (reserved           (offset #x04 9) 7)
  ;; 0x08: GPIO_DIR
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
  (gpio-4-direction (offset #x08 4) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (gpio-5-direction (offset #x08 5) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (gpio-6-direction (offset #x08 6) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (gpio-7-direction (offset #x08 7) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (gpio-8-direction (offset #x08 8) 1
                    #:semantics lookup gpio-direction-map
                    #:default 'input)
  (reserved         (offset #x08 9) 7)
  ;; 0x0C: GPIO_OUT
  (gpio-0-output-value (offset #x0c 0) 1)
  (gpio-1-output-value (offset #x0c 1) 1)
  (gpio-2-output-value (offset #x0c 2) 1)
  (gpio-3-output-value (offset #x0c 3) 1)
  (gpio-4-output-value (offset #x0c 4) 1)
  (gpio-5-output-value (offset #x0c 5) 1)
  (gpio-6-output-value (offset #x0c 6) 1)
  (gpio-7-output-value (offset #x0c 7) 1)
  (gpio-8-output-value (offset #x0c 8) 1)
  (reserved            (offset #x0c 9) 7)
  ;; 0x10: GPIO_IRQE
  (gpio-0-irq-enable (offset #x10 0) 1)
  (gpio-1-irq-enable (offset #x10 1) 1)
  (gpio-2-irq-enable (offset #x10 2) 1)
  (gpio-3-irq-enable (offset #x10 3) 1)
  (gpio-4-irq-enable (offset #x10 4) 1)
  (gpio-5-irq-enable (offset #x10 5) 1)
  (gpio-6-irq-enable (offset #x10 6) 1)
  (gpio-7-irq-enable (offset #x10 7) 1)
  (gpio-8-irq-enable (offset #x10 8) 1)
  (reserved          (offset #x10 9) 7)
  ;; 0x14: GPIO_ISTS
  (gpio-0-irq-status (offset #x14 0) 1)
  (gpio-1-irq-status (offset #x14 1) 1)
  (gpio-2-irq-status (offset #x14 2) 1)
  (gpio-3-irq-status (offset #x14 3) 1)
  (gpio-4-irq-status (offset #x14 4) 1)
  (gpio-5-irq-status (offset #x14 5) 1)
  (gpio-6-irq-status (offset #x14 6) 1)
  (gpio-7-irq-status (offset #x14 7) 1)
  (gpio-8-irq-status (offset #x14 8) 1)
  (reserved          (offset #x14 9) 7)
  ;; 0x18: GPIO_ISEN
  (gpio-0-irq-sense-mode (offset #x18 0) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-1-irq-sense-mode (offset #x18 1) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-2-irq-sense-mode (offset #x18 2) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-3-irq-sense-mode (offset #x18 3) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-4-irq-sense-mode (offset #x18 4) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-5-irq-sense-mode (offset #x18 5) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-6-irq-sense-mode (offset #x18 6) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-7-irq-sense-mode (offset #x18 7) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (gpio-8-irq-sense-mode (offset #x18 8) 1
                         #:semantics lookup gpio-irq-mode-map
                         #:default 'active-high/rising-edge)
  (reserved              (offset #x18 9) 7)
  ;; 0x1C: GPIO_IMODE
  (gpio-0-irq-mode (offset #x1c 0) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-1-irq-mode (offset #x1c 1) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-2-irq-mode (offset #x1c 2) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-3-irq-mode (offset #x1c 3) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-4-irq-mode (offset #x1c 4) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-5-irq-mode (offset #x1c 5) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-6-irq-mode (offset #x1c 6) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-7-irq-mode (offset #x1c 7) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (gpio-8-irq-mode (offset #x1c 8) 1
                   #:semantics lookup gpio-irq-level/edge-map
                   #:default 'level)
  (reserved        (offset #x1c 9) 23)
  ;; 0x20: GPIO_IBES
  (gpio-0-bothedge-enable (offset #x20 0) 1)
  (gpio-1-bothedge-enable (offset #x20 1) 1)
  (gpio-2-bothedge-enable (offset #x20 2) 1)
  (gpio-3-bothedge-enable (offset #x20 3) 1)
  (gpio-4-bothedge-enable (offset #x20 4) 1)
  (gpio-5-bothedge-enable (offset #x20 5) 1)
  (gpio-6-bothedge-enable (offset #x20 6) 1)
  (gpio-7-bothedge-enable (offset #x20 7) 1)
  (gpio-8-bothedge-enable (offset #x20 8) 1)
  (reserved               (offset #x20 9) 7)
  ;; 0x24: GPIO_IBES
  (gpio-0-irq-latch-clear (offset #x24 0) 1)
  (gpio-1-irq-latch-clear (offset #x24 1) 1)
  (gpio-2-irq-latch-clear (offset #x24 2) 1)
  (gpio-3-irq-latch-clear (offset #x24 3) 1)
  (gpio-4-irq-latch-clear (offset #x24 4) 1)
  (gpio-5-irq-latch-clear (offset #x24 5) 1)
  (gpio-6-irq-latch-clear (offset #x24 6) 1)
  (gpio-7-irq-latch-clear (offset #x24 7) 1)
  (gpio-8-irq-latch-clear (offset #x24 8) 1)
  (reserved               (offset #x24 9) 7)
  ;; 0x28: GPIO_IDBE
  (gpio-0-debounce-enable (offset #x28 0) 1)
  (gpio-1-debounce-enable (offset #x28 1) 1)
  (gpio-2-debounce-enable (offset #x28 2) 1)
  (gpio-3-debounce-enable (offset #x28 3) 1)
  (gpio-4-debounce-enable (offset #x28 4) 1)
  (gpio-5-debounce-enable (offset #x28 5) 1)
  (gpio-6-debounce-enable (offset #x28 6) 1)
  (gpio-7-debounce-enable (offset #x28 7) 1)
  (gpio-8-debounce-enable (offset #x28 8) 1)
  (reserved               (offset #x28 9) 7)
  ;; 0x2c: GPIO_RAW
  (gpio-0-io-raw-state (offset #x2c 0) 1)
  (gpio-1-io-raw-state (offset #x2c 1) 1)
  (gpio-2-io-raw-state (offset #x2c 2) 1)
  (gpio-3-io-raw-state (offset #x2c 3) 1)
  (gpio-4-io-raw-state (offset #x2c 4) 1)
  (gpio-5-io-raw-state (offset #x2c 5) 1)
  (gpio-6-io-raw-state (offset #x2c 6) 1)
  (gpio-7-io-raw-state (offset #x2c 7) 1)
  (gpio-8-io-raw-state (offset #x2c 8) 1)
  (reserved            (offset #x2c 9) 7))

(define-register reg:digital-rx-cfg
  #:address #x06
  #:description "Digital receiver configuration"
  #:double-buffer #f
  ;;#:register-width (octets 47)
  #:contents
  ;; 0x00: DTUNE0
  (preamble-aquisition-chunk-size (offset #x00 0)  2)
  (reserved                       (offset #x00 2)  2)
  (digital-tuning-bit-4           (offset #x00 4)  1)
  (reserved                       (offset #x00 5) 11)
  ;; 0x02: RX_SFD_TOC
  (sfd-detection-timeout (offset #x02 0) 16 #:default 65)
  ;; 0x04: PRE_TOC
  (preamble-detection-timeout (offset #x04 0) 16)
  ;; 0x0C: DTUNE3
  ;; Again, the data-sheet says 0xaf5f35cc yields better performance than the
  ;; default.
  (digital-receiver-tuning-word-3 (offset #x0c 0) 32 #:default #xaf5f584c)
  ;; 0x14: DTUNE5
  (reserved (offset #x14 0) 32)
  ;; 0x29: DRX_CAR_INT
  (remote-tx-freq-offset-estimate (offset #x29 0) 24))

(define-register reg:analog-rf-cfg
  #:address #x07
  #:description "Analog RF configuration block"
  #:double-buffer #f
  ;;#:register-width (octets 86)
  #:contents
  ;; The data-sheet says to write the same value into RF_ENABLE and
  ;; RF_CTRL_MASK. The defaults transcribed are for channel 5. The values for
  ;; channel 9 are in the comment.
  ;; 0x00: RF_ENABLE
  (rf-enable    (offset #x00 0) 32 #:default #x02003c00) ;; 0x02001C00
  ;; 0x04: RF_CTRL_MASK
  (rf-ctrl-mask (offset #x04 0) 32 #:default #x02003c00) ;; 0x02001C00
  ;; 0x14: RF_SWITCH
  (antenna-auto-toggle-enable (offset #x14  0) 1
                              #:semantics* boolean/active-low #:default #t)
  (pdoa-starting-port-select  (offset #x14  1) 1)
  (reserved                   (offset #x14  2) 6)
  (manual-antenna-switch-ctrl (offset #x14  8) 1)
  (reserved                   (offset #x14  9) 3)
  (antenna-switch-ctrl        (offset #x14 12) 3)
  (reserved                   (offset #x14 15) 1)
  (manual-txrx-switch         (offset #x14 16) 1)
  (reserved                   (offset #x14 17) 7)
  (txrx-switch-ctrl           (offset #x14 24) 6)
  (reserved                   (offset #x14 30) 2)
  ;; 0x1A: RX_TX_CTRL_1
  (analog-tx-ctrl-1 (offset #x1a 0)  8 #:default #x0e)
  ;; 0x1C: RX_TX_CTRL_2
  ;; This is for ch5 and ch9 both.
  (pulse-generator-delay      (offset #x1c 0)  6 #:default #x34)
  ;; ch9: 0x0700400
  (reserved                   (offset #x1c 6) 26 #:default #x0701c44)
  ;; 0x28: TX_TEST
  (tx-test-select (offset #x28 0) 4)
  (reserved       (offset #x28 4) 4)
  ;; 0x34: SAR_TEST
  (reserved                    (offset #x34 0) 2)
  (sar-temperature-read-enable (offset #x34 2) 1)
  (reserved                    (offset #x34 3) 5)
  ;; 0x40: LDO_TUNE
  (internal-ldo-tuning-word (offset #x40  0) 60)
  (reserved                 (offset #x40 60)  4)
  ;; 0x48: LDO_CTRL (this is not documented)
  (ldo-control (offset #x48 0) 32)
  ;; 0x51: LDR_RLOAD
  (ldo-tuning-word (offset #x51 0) 8 #:default #x14))

(define-register reg:tx-calibration
  #:address #x08
  #:description "Transmitter calibration block"
  #:double-buffer #f
  ;;#:register-width (octets 31)
  #:contents
  (tx-calibration 0 (octets 31)))

(define-register reg:freq-synth-ctrl
  #:address #x09
  #:description "Frequency synthesizer control"
  #:double-buffer #f
  ;;#:register-width (octets 23)
  #:contents
  ;; 0x00: PLL_CFG
  ;; The default is for channel 5; for channel 9: 0x0f3c
  (pll-config (offset #x00 0) 16 #:default #x1f3c)
  ;; 0x04: PLL_CC
  (pll-coarse-code-ch9 (offset #x04  0)  8 #:default #b00001011)
  (pll-coarse-code-ch5 (offset #x04 14)  8 #:default #b00000000001111)
  (reserved            (offset #x04 22) 10)
  ;; 0x08: PLL_CAL
  (reserved        (offset #x08 0) 1)
  (pll-cal-use-old (offset #x08 1) 1)
  (reserved        (offset #x08 2) 2)
  ;; The documentation says, 0x31 is the default, but a better value to use
  ;; would be 0x81, which should be set during configuration.
  (pll-cal-config  (offset #x08 4) 4 #:default #x31)
  (pll-cal-enable  (offset #x08 8) 1)
  (reserved        (offset #x08 9) 7)
  ;; 0x14: PLL_XTAL
  (xtal-trim-value (offset #x14 0) 6)
  (reserved        (offset #x14 6) 2))

(define-register reg:always-on-system-control
  #:address #x0a
  #:description "Always-on system control interface"
  #:double-buffer #f
  #:register-width (octets 23)
  #:contents
  (aon-configuration 0 (octets 23)))

(define-register reg:otp-interface
  #:address #x0b
  #:description "OTP memory interface"
  #:double-buffer #f
  ;;#:register-width (octets 23)
  #:contents
  ;; 0x00: OTP_WDATA
  (otp-write-data-word  (offset #x00  0) 32)
  ;; 0x04: OTP_ADDR
  (otp-access-address   (offset #x04  0) 11)
  (reserved             (offset #x04 11)  5)
  ;; 0x08: OTP_ADDR
  (otp-manual-control   (offset #x08  0)  1)
  (otp-read-enable      (offset #x08  1)  1)
  (otp-write-enable     (offset #x08  2)  1)
  (otp-write-mode       (offset #x08  3)  1)
  (reserved             (offset #x08  4)  2)
  (otp-kick-dgc         (offset #x08  6)  1)
  (otp-kick-ldo         (offset #x08  7)  1)
  (reserved             (offset #x08  8)  2)
  (otp-kick-bias        (offset #x08 10)  1)
  (otp-kick-ops         (offset #x08 11)  2)
  (otp-kick-ops-params  (offset #x08 13)  1)
  (reserved             (offset #x08 14)  2)
  ;; 0x0C: OTP_STAT
  (otp-programming-done (offset #x0c  0)  1)
  (otp-vpp-ok           (offset #x0c  1)  1)
  (reserved             (offset #x0c  2)  6)
  ;; 0x10: OTP_RDATA
  (otp-read-data-word   (offset #x10  0) 32)
  ;; 0x14: OTP_SRDATA
  (otp-special-read-data-word (offset #x14 0) 32))

(define-register reg:cia-0
  #:address #x0c
  #:description "Channel Impulse response Analyser (CIA) Interface Part 0"
  #:double-buffer #f
  ;;#:register-width (octets 105)
  #:contents
  ;; 0x00: IP_TS
  (ipatov-toa        (offset #x00  0) 40)
  (ipatov-poa        (offset #x00 40) 14)
  (reserved          (offset #x00 54)  2)
  (ipatov-toa-status (offset #x00 56)  8)
  ;; 0x08: STS_TS
  (sts0-toa                         (offset #x08  0) 40)
  (sts0-poa                         (offset #x08 40) 14)
  (reserved                         (offset #x08 54)  2)
  (sts0-toast-fp-no-strong-edge     (offset #x08 56)  1)
  (sts0-toast-noise-thresh-lowered  (offset #x08 57)  1)
  (sts0-toast-cir-too-weak          (offset #x08 58)  1)
  ;; Coarse First path too close to be plausible.
  (sts0-toast-cfp-too-close         (offset #x08 59)  1)
  ;; First path too close to be plausible.
  (sts0-toast-fp-too-close          (offset #x08 60)  1)
  (sts0-toast-cq-enable             (offset #x08 61)  1)
  (sts0-toast-ss-enable             (offset #x08 62)  1)
  (sts0-toast-pgr-enable            (offset #x08 63)  1)
  ;; 0x10: STS1_TS
  (sts1-toa                         (offset #x10  0) 40)
  (sts1-poa                         (offset #x10 40) 14)
  (reserved                         (offset #x10 54)  2)
  (sts1-toast-fp-no-strong-edge     (offset #x10 56)  1)
  (sts1-toast-noise-thresh-lowered  (offset #x10 57)  1)
  (sts1-toast-cir-too-weak          (offset #x10 58)  1)
  (sts1-toast-cfp-too-close         (offset #x10 59)  1)
  (sts1-toast-fp-too-close          (offset #x10 60)  1)
  (sts1-toast-cq-enable             (offset #x10 61)  1)
  (sts1-toast-ss-enable             (offset #x10 62)  1)
  (sts1-toast-pgr-enable            (offset #x10 63)  1)
  ;; 0x18: TDOA
  (tdoa (offset #x18 0) (octets 6))
  ;; 0x1E: PDOA
  (pdoa           (offset #x1e 0)  14)
  (fp-thresh-test (offset #x1e 14)  1)
  (reserved       (offset #x1e 15)  1)
  ;; 0x20: CIA Diag 0
  (clock-offs-est (offset #x20  0) 13)
  (reserved       (offset #x20 13) 19)
  ;; 0x24: CIA Diag 1
  (reserved (offset #x24 0) 32)
  ;; 0x28: IP_DIAG_0
  (preamble-peak-amp   (offset #x28  0) 21)
  (preamble-peak-index (offset #x28 21) 10)
  (reserved            (offset #x28 31)  1)
  ;; 0x2C: IP_DIAG_1
  (preamble-channel-area (offset #x2c  0) 17)
  (reserved              (offset #x2c 17) 15)
  ;; 0x30: IP_DIAG_2
  (preamble-fp1-magnitude (offset #x30  0) 22)
  (reserved               (offset #x30 22) 10)
  ;; 0x34: IP_DIAG_3
  (preamble-fp2-magnitude (offset #x34  0) 22)
  (reserved               (offset #x34 22) 10)
  ;; 0x38: IP_DIAG_4
  (preamble-fp3-magnitude (offset #x38  0) 22)
  (reserved               (offset #x38 22) 10)
  ;; 0x3c: IP_DIAG_RES1
  (reserved (offset #x3c 0) (octets 12))
  ;; 0x48: IP_DIAG_8
  (preamble-fp-index (offset #x48  0) 16)
  (reserved          (offset #x48 16) 16)
  ;; 0x4c: IP_DIAG_RES1
  (reserved (offset #x4c 0) (octets 12))
  ;; 0x58: IP_DIAG_12
  (preamble-accumulated-symbols (offset #x58  0) 12)
  (reserved                     (offset #x58 12) 20)
  ;; 0x5C: STS_DIAG_0
  (sts0-peak-amplitude (offset #x5c  0) 21)
  (sts0-peak-index     (offset #x5c 21)  9)
  (reserved            (offset #x5c 30)  2)
  ;; 0x60: STS_DIAG_1
  (sts0-channel-area (offset #x60  0) 16)
  (reserved          (offset #x60 16) 16)
  ;; 0x64: STS_DIAG_2
  (sts0-fp1-magnitude (offset #x64  0) 22)
  (reserved           (offset #x64 22) 10)
  ;; 0x68: STS_DIAG_3
  (sts0-fp2-magnitude (offset #x68  0) 22)
  (reserved           (offset #x68 22) 10))

(define-register reg:cia-1
  #:address #x0d
  #:description "CIA Interface Part 1"
  #:double-buffer #f
  ;;#:register-width (octets 105)
  #:contents
  ;; 0x00: STS_DIAG_4
  (sts0-fp3-magnitude (offset #x00  0) 22)
  (reserved           (offset #x00 22) 10)
  ;; 0x04: STS_DIAG_RES1
  (reserved (offset #x04 0) (octets 12))
  ;; 0x10: STS_DIAG_8
  (sts0-fp-index (offset #x10  0) 15)
  (reserved      (offset #x10 15) 17)
  ;; 0x14: STS_DIAG_RES2
  (reserved (offset #x14 0) (octets 12))
  ;; 0x20: STS_DIAG12
  (sts0-accumulated-symbols (offset #x20  0) 11)
  (reserved                 (offset #x20 11) 21)
  ;; 0x24: STS_DIAG_RES3
  (reserved (offset #x24 0) (octets 20))
  ;; 0x38: STS1_DIAG_0
  (sts1-peak-amplitude (offset #x38  0) 21)
  (sts1-peak-index     (offset #x38 21)  9)
  (reserved            (offset #x38 30)  2)
  ;; 0x3C: STS1_DIAG_1
  (sts1-channel-area (offset #x3c  0) 16)
  (reserved          (offset #x3c 16) 16)
  ;; 0x40: STS1_DIAG_2
  (sts1-fp1-magnitude (offset #x40  0) 22)
  (reserved           (offset #x40 22) 10)
  ;; 0x44: STS1_DIAG_3
  (sts1-fp2-magnitude (offset #x44  0) 22)
  (reserved           (offset #x44 22) 10)
  ;; 0x48: STS1_DIAG_4
  (sts1-fp3-magnitude (offset #x48  0) 22)
  (reserved           (offset #x48 22) 10)
  ;; 0x4c: STS1_DIAG_RES1
  (reserved (offset #x4c 0) (octets 12))
  ;; 0x58: STS1_DIAG_8
  (sts1-fp-index (offset #x58  0) 15)
  (reserved      (offset #x58 15) 17)
  ;; 0x5c: STS1_DIAG_RES1
  (reserved (offset #x5c 0) (octets 12))
  ;; 0x68: STS1_DIAG12
  (sts1-accumulated-symbols (offset #x68  0) 11)
  (reserved                 (offset #x68 11) 21))

(define-register reg:cia-2-and-rx-antenna-delay
  #:address #x0e
  #:description "CIA Interface Part 2 and RX Antenna delay"
  #:double-buffer #f
  ;;#:register-width (octets 30)
  #:contents
  ;; 0x00: CIA_CONF
  (rx-antenna-delay    (offset #x00  0) 16 #:default #x4015)
  (reserved            (offset #x00 16)  4 #:default #b0001)
  (minimal-diag-enable (offset #x00 20)  1 #:default #t)
  (reserved            (offset #x00 21) 11)
  ;; 0x04: FP_CONF
  (reserved            (offset #x04  0)  8 #:default #x10)
  (fp-agreed-threshold (offset #x04  8)  3 #:default #b011)
  (calibration-temp    (offset #x04 11)  8)
  (reserved            (offset #x04 19)  1 #:default #t)
  (ant-delay-temp-comp (offset #x04 20)  1 #:default #t)
  (reserved            (offset #x04 21) 11 #:default #b00000000011))

(define-register reg:digital-diag
  #:address #x0f
  #:description "Digital diagnostics interface"
  #:double-buffer #f
  #:register-width (octets 77)
  #:contents
  ;; 0x00: EVC_CTRL
  (event-counters-enable (offset #x00 0) 1)
  (event-counters-clear  (offset #x00 1) 1)
  (reserved              (offset #x00 2) 6)
  ;; 0x04: EVC_PHE
  (phr-error-counter (offset #x04  0) 12)
  (reserved          (offset #x04 12)  4)
  ;; 0x06: EVC_RSD
  (rsd-error-counter (offset #x06  0) 12)
  (reserved          (offset #x06 12)  4)
  ;; 0x08: EVC_FCG
  (fcs-good-counter (offset #x08  0) 12)
  (reserved         (offset #x08 12)  4)
  ;; 0x0A: EVC_FCE
  (fcs-error-counter (offset #x0a  0) 12)
  (reserved          (offset #x0a 12)  4)
  ;; 0x0C: EVC_FFR
  (frame-filter-rejection (offset #x0c 0) 8)
  ;; 0x0E: EVC_OVR
  (rx-overrun-error-counter (offset #x0e 0) 8)
  ;; 0x10: EVC_STO
  (sfd-timeout-counter (offset #x10  0) 12)
  (reserved            (offset #x10 12)  4)
  ;; 0x12: EVC_PTO
  (preamble-detect-timeout-counter (offset #x12  0) 12)
  (reserved                        (offset #x12 12)  4)
  ;; 0x14: EVC_FWTO
  (rx-frame-timeout-counter (offset #x14 0) 8)
  ;; 0x16: EVC_TXFS
  (tx-frame-counter (offset #x16  0) 12)
  (reserved         (offset #x16 12)  4)
  ;; 0x18: EVC_HPW
  (half-period-warning-counter (offset #x18 0) 8)
  ;; 0x1A: EVC_SWCE
  (spi-write-crc-error-counter (offset #x1a 0) 8)
  ;; 0x1C: EVC_RES1
  (reserved (offset #x1c 0) (octets 8))
  ;; 0x24: DIAG_TMC
  (reserved                           (offset #x24  0)  4)
  (tx-power-spectrum-test-mode-enable (offset #x24  4)  1)
  (reserved                           (offset #x24  5) 16)
  (host-irq-polarity                  (offset #x24 21)  1)
  (reserved                           (offset #x24 22)  2)
  (cia-watchdog-enable                (offset #x24 24)  1)
  (reserved                           (offset #x24 25)  1)
  (cia-manual-run-enable              (offset #x24 26)  1)
  (reserved                           (offset #x24 27)  5)
  ;; 0x28: EVC_CPQE
  (sta-quality-error-counter (offset #x28 0) 8)
  ;; 0x2A: EVC_VWARN
  (low-voltage-warning-counter (offset #x2a 0) 8)
  ;; 0x2C: SPI_MODE
  (spi-mode-clk-polarity (offset #x2c 0) 1)
  (spi-mode-clk-phase    (offset #x2c 1) 1)
  (reserved              (offset #x2c 2) 6)
  ;; 0x30: SYS_STATE
  (tx-state   (offset #x30  0)  4)
  (reserved   (offset #x30  4)  4)
  (rx-state   (offset #x30  8)  6)
  (reserved   (offset #x30 14)  2)
  (psmc-state (offset #x30 16)  5)
  (reserved   (offset #x30 21) 11)
  ;; 0x3C: FCMD_STAT
  (fast-command-status (offset #x3c 0) 5)
  (reserved            (offset #x3c 5) 3)
  ;; 0x48: CTR_DBG
  (counter-debug-sts-iv (offset #x48 0) (octets 4))
  ;; 0x4C: SPICRCINIT
  (spi-crc-lfsr-init-code (offset #x4c 0) 8))

(define-register reg:pmsc
  #:address #x11
  #:description "PMSC control and status"
  #:double-buffer #f
  ;;#:register-width (octets 30)
  #:contents
  ;; 0x00: SOFT_RST
  (soft-arm-reset  (offset #x00 0) 1)
  (soft-prgn-reset (offset #x00 1) 1)
  (soft-cia-reset  (offset #x00 2) 1)
  (soft-bist-reset (offset #x00 3) 1)
  (soft-rx-reset   (offset #x00 4) 1)
  (soft-tx-reset   (offset #x00 5) 1)
  (soft-hif-reset  (offset #x00 6) 1)
  (soft-pmsc-reset (offset #x00 7) 1)
  (soft-gpio-reset (offset #x00 8) 1)
  (reserved        (offset #x00 9) 7)
  ;; 0x04: CLK_CTRL
  (sys-clock-ctrl             (offset #x04  0) 2)
  (rx-clock-ctrl              (offset #x04  2) 2)
  (tx-clock-ctrl              (offset #x04  4) 2)
  (acc-clock-enable           (offset #x04  6) 1)
  (reserved                   (offset #x04  7) 1)
  (cia-clock-enable           (offset #x04  8) 1)
  (reserved                   (offset #x04  9) 1 #:default #t)
  (sar-clock-enable           (offset #x04 10) 1)
  (reserved                   (offset #x04 11) 4)
  (acc-memory-clock-enable    (offset #x04 15) 1)
  (gpio-clock-enable          (offset #x04 16) 1)
  (reserved                   (offset #x04 17) 1)
  (gpio-debounce-clock-enable (offset #x04 18) 1)
  (gpio-debounce-reset        (offset #x04 19) 1
                              #:semantics* boolean/active-low
                              #:default #f)
  (reserved                   (offset #x04 20) 3 #:default #b011)
  (kilohertz-clock-enable     (offset #x04 23) 1)
  (reserved                   (offset #x04 24) 8 #:default #b11110000)
  ;; 0x08: SEQ_CTRL
  (reserved                 (offset #x08  0) 8 #:default #b00111000)
  (auto-idle-rc-to-pll      (offset #x08  8) 1)
  (reserved                 (offset #x08  9) 2 #:default #b11)
  (auto-sleep-after-tx      (offset #x08 11) 1)
  (auto-sleep-after-rx      (offset #x08 12) 1)
  (reserved                 (offset #x08 13) 2)
  (external-sync-pll-enable (offset #x08 15) 1)
  (reserved                 (offset #x08 16) 1)
  (cia-run-enable           (offset #x08 17) 1)
  (reserved                 (offset #x08 18) 5)
  (force-idle-rc-state      (offset #x08 23) 1)
  (reserved                 (offset #x08 24) 2)
  (kilohertz-clock-divider  (offset #x08 26) 6)
  ;; 0x12: TXFSEQ
  ;; To disable this, apparently you need to set this to 0x0d20874. The
  ;; individual parts of this register are undocumented.
  (tx-fine-pwr-sequencing (offset #x12 0) 32 #:default #x4d28874)
  ;; 0x16: LED_CTRL
  (led-blink-time    (offset #x16  0)  8 #:default #b00100000)
  (led-blink-enable  (offset #x16  8)  1)
  (reserved          (offset #x16  9)  7)
  (led-0-force-blink (offset #x16 16)  1)
  (led-1-force-blink (offset #x16 17)  1)
  (led-2-force-blink (offset #x16 18)  1)
  (led-3-force-blink (offset #x16 19)  1)
  (reserved          (offset #x16 20) 12)
  ;; 0x1A: RX_SNIFF
  (sniff-mode-on-time  (offset #x1a  0)  4)
  (reserved            (offset #x1a  4)  4)
  (sniff-mode-off-time (offset #x1a  8)  8)
  (reserved            (offset #x1a 16) 16)
  ;; 0x1F: BIAS_CTRL
  (bias-control (offset #x1f 0) 16))

(define-register reg:rx-buffer-0
  #:address #x12
  #:description "RX frame data buffer 0"
  #:double-buffer #f
  #:register-width (octets 1024)
  #:contents
  (rx-frame-buffer-0 (offset 0 0) (octets 1024)))

(define-register reg:rx-buffer-1
  #:address #x13
  #:description "RX frame data buffer 1"
  #:double-buffer #f
  #:register-width (octets 1024)
  #:contents
  (rx-frame-buffer-1 (offset 0 0) (octets 1024)))

(define-register reg:tx-buffer
  #:address #x14
  #:description "Transmit data buffer"
  #:double-buffer #f
  #:register-width (octets 1024)
  #:contents
  (tx-data-buffer (offset 0 0) (octets 1024)))

(define-register reg:acc-mem
  #:address #x15
  #:description "Accumulator CIR memory"
  #:double-buffer #f
  #:register-width (octets 12288)
  #:contents
  (cir-accumulator (offset 0 0) (octets 12288)))

(define-register reg:scratch-ram
  #:address #x16
  #:description "Scratch RAM memory buffer"
  #:double-buffer #f
  #:register-width (octets 127)
  #:contents
  (scratch-ram (offset 0 0) (octets 127)))

(define-register reg:aes-ram
  #:address #x17
  #:description "AES key RAM"
  #:double-buffer #f
  #:register-width (octets 75)
  #:contents
  (aes-key-ram (offset 0 0) (octets 75)))

(define-register reg:set-1/set-2
  #:address #x18
  #:description "Double Buffer diagnostic register set"
  #:double-buffer #t
  #:register-width (octets 460)
  #:contents
  (db-diag-reg (offset 0 0) (octets 460)))

(define-register reg:indirect-ptr-a
  #:address #x1d
  #:description "Indirect pointer A"
  #:double-buffer #f
  #:register-width (octets 4)
  #:contents
  (indirect-ptr-a (offset 0 0) (octets 4)))

(define-register reg:indirect-ptr-b
  #:address #x1e
  #:description "Indirect pointer B"
  #:double-buffer #f
  #:register-width (octets 4)
  #:contents
  (indirect-ptr-b (offset 0 0) (octets 4)))

(define-register reg:in-ptr-cfg
  #:address #x1f
  #:description "FINT status and indirect pointer interface"
  #:double-bugger #f
  ;;#:register-width (octets 19)
  #:contents
  ;; 0x00: FINT_STAT
  (tx-ok-status (offset 0 0) 1)
  (cca-fail-status (offset 0 1) 1)
  (rxter-error-status (offset 0 2) 1)
  (rx-ok-status (offset 0 3) 1)
  (rx-error-status (offset 0 4) 1)
  (rx-timeout-status (offset 0 5) 1)
  (sys-event-status (offset 0 6) 1)
  (sys-panic-status (offset 0 7) 1)
  ;; 0x04: PTR_ADDR_A
  (ptr-a-base-addr (offset 4 0) 5)
  ;; 0x08: PTR_OFFSET_A
  (ptr-a-offset (offset 8 0) 15)
  ;; 0x0C: PTR_ADDR_B
  (ptr-b-base-addr (offset 12 0) 5)
  ;; 0x10: PTR_OFFSET_B
  (ptr-b-offset (offset 16 0) 15))
