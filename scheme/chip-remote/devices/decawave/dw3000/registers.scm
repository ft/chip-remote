;; Copyright (c) 2023-2024 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices decawave dw3000 registers)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote item)
  #:use-module (chip-remote item builder)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:use-module (chip-remote devices decawave dw3000 tables)
  #:use-module ((chip-remote devices decawave dw1000 registers) #:prefix dw1000:)
  #:use-module ((chip-remote devices decawave dw1000 tables) #:prefix dw1000:))

(define double-buffered-registers '((reg:set-1/set-2 . 24)))

(define (octets n) (* n 8))

(define (offset byte bit) (+ (* byte 8) bit))

;; 0x00 GEN_CFG_AES #0
;; Main register bank and AES configuration.

(define-public reg:system-cfg
  (register
   (name 'system-cfg)
   (address #x10)
   (width (octets 4))
   (items
    (list
     (‣ frame-filtering-enable              0  1)
     (‣ disable-fcs-tx                      1  1)
     (‣ disable-frame-check-error-handling  2  1)
     (‣ disable-double-rx-buf               3  1 (default #t))
     (‣ phr-mode                            4  1)
     (‣ phr-6m8                             5  1)
     (‣ spi-crc-enable                      6  1)
     (‣ cia-ipatov-processing               7  1 (default #t))
     (‣ cia-sts-processing                  8  1 (default #t))
     (‣ rx-wait-timeout-enable              9  1)
     (‣ rx-auto-reenable                   10  1)
     (‣ auto-ack                           11  1)
     (‣ sts-packet-configuration           12  2 (default 1))
     (‣ reserved                           14  1)
     (‣ configure-sdc-mode                 15  1)
     (‣ pdoa-mode                          16  2)
     (‣ fast-aat                           18  1 (default #t))
     (‣ reserved                           19 13)))))

(define-public reg:frame-filter-cfg
  (register
   (name 'frame-filter-cfg)
   (address #x14)
   (width (octets 2))
   (items
    (list
     (‣ frame-filter-allow-beacon                  0 1)
     (‣ frame-filter-allow-data                    1 1)
     (‣ frame-filter-allow-ack                     2 1)
     (‣ frame-filter-allow-mac                     3 1)
     (‣ frame-filter-allow-reserved                4 1)
     (‣ frame-filter-allow-multipurpose            5 1)
     (‣ frame-filter-allow-frack                   6 1)
     (‣ frame-filter-allow-extended                7 1)
     (‣ frame-filter-behave-as-coordinator         8 1)
     (‣ frame-filter-allow-mac-implicit-broadcast  9 1)
     (‣ low-energy-0-pending                      10 1)
     (‣ low-energy-1-pending                      11 1)
     (‣ low-energy-2-pending                      12 1)
     (‣ low-energy-3-pending                      13 1)
     (‣ short-source-address-enable               14 1)
     (‣ long-source-address-enable                15 1)))))

(define-public reg:spi-crc-read-status
  (register
   (name 'spi-crc-read-status)
   (address #x18)
   (width (octets 1))
   (items (list (‣ spi-crc-read-status 0 8)))))

(define-public reg:system-time
  (register
   (name 'system-time)
   (address #x1c)
   (width (octets 4))
   (items (list (‣ system-time 0 32)))))

(define-public reg:tx-frame-ctrl
  (register
   (name 'tx-frame-ctrl)
   (address #x24)
   (width (octets 6))
   (items
    (list (‣ tx-frame-length                 0 10 (default 12))
          (‣ tx-bit-rate                    10  1 (default 1))
          (‣ tx-phr-ranging-enable          11  1 (default 1))
          (‣ tx-preamble-symbol-repetitions 12  4
                                            (semantics
                                             (tbl preamble-symbol-rep-map
                                                  #:default 64)))
          (‣ tx-buf-index-offset            16 10)
          (‣ reserved                       26 14)
          (‣ fine-psr-ctrl                  40  8)))))

(define-public reg:delayed-tx/rx-time
  (register
   (name 'delayed-tx/rx-time)
   (address #x2c)
   (width (octets 4))
   (items (list (‣ delay-time 0 (octets 4))))))

(define-public reg:delayed-reference-time
  (register
   (name 'delayed-reference-time)
   (address #x30)
   (width (octets 4))
   (items (list (‣ delayed-reference-time 0 (octets 4))))))

(define-public reg:rx-frame-wait-timeout
  (register
   (name 'rx-frame-wait-timeout)
   (address #x34)
   (width (octets 3))
   (items (list (‣ rx-frame-wait-timeout 0 20)
                (‣ reserved 20 4)))))

(define-public reg:system-ctrl
  (register
   (name 'system-ctrl)
   (address #x38)
   (width (octets 1))
   (items
    (list (‣ continuous-frame-test-start 0 1)
          (‣ reserved 1 7)))))

(define-public reg:system-event-mask
  (register
   (name 'system-event-mask)
   (address #x3c)
   (width (octets 6))
   (items (list (‣ reserved                                  0 1)
                (‣ cpll-lock-irq-enable                      1 1)
                (‣ spi-crc-error-irq-enable                  2 1)
                (‣ auto-ack-irq-enable                       3 1)
                (‣ tx-frame-begins-irq-enable                4 1)
                (‣ tx-preamble-sent-irq-enable               5 1)
                (‣ tx-phy-header-sent-irq-enable             6 1)
                (‣ tx-frame-sent-irq-enable                  7 1)
                (‣ rx-preamble-detected-irq-enable           8 1)
                (‣ rx-sfd-deteced-irq-enable                 9 1)
                (‣ rx-cia-done-irq-enable                   10 1)
                (‣ rx-phy-header-detected-irq-enable        11 1)
                (‣ rx-phy-header-error-irq-enable           12 1)
                (‣ rx-data-ready-irq-enable                 13 1)
                (‣ rx-fcs-good-irq-enable                   14 1)
                (‣ rx-fcs-error-irq-enable                  15 1)
                (‣ rx-rs-frame-sync-loss-irq-enable         16 1)
                (‣ rx-wait-timeout-irq-enable               17 1)
                (‣ cia-error-irq-enable                     18 1)
                (‣ voltage-warning-irq-enable               19 1)
                (‣ rx-overrun-irq-enable                    20 1)
                (‣ rx-preamble-detection-timeout-irq-enable 21 1)
                (‣ reserved                                 22 1)
                (‣ spi-ready-irq-enable                     23 1 (default #t))
                (‣ idle-rc-irq-enable                       24 1)
                (‣ pll-losing-lock-irq-enable               25 1)
                (‣ rx-sfd-timeout-irq-enable                26 1)
                (‣ hpd-warn-irq-enable                      27 1)
                (‣ sts-error-irq-enable                     28 1)
                (‣ aff-reject-irq-enable                    29 1)
                (‣ reserved                                 30 3)
                (‣ rx-preamble-reject-irq-enable            33 1)
                (‣ reserved                                 34 2)
                (‣ vt-detection-irq-enable                  36 1)
                (‣ gpio-irq-enable                          37 1)
                (‣ aes-done-irq-enable                      38 1)
                (‣ aes-error-irq-enable                     39 1)
                (‣ cmd-error-irq-enable                     40 1 (default #t))
                (‣ spi-overflow-irq-enable                  41 1 (default #t))
                (‣ spi-underflow-irq-enable                 42 1 (default #t))
                (‣ spi-error-irq-enable                     43 1 (default #t))
                (‣ cca-fail-irq-enable                      44 1)
                (‣ reserved                                 45 3)))))

(define-public reg:system-event-status
  (register
   (name 'system-event-status)
   (address #x44)
   (width (octets 6))
   (items (list (‣ interrupt-request-status           0 1)
                (‣ cpll-lock-irq                      1 1)
                (‣ spi-crc-error-irq                  2 1)
                (‣ auto-ack-irq                       3 1)
                (‣ tx-frame-begins-irq                4 1)
                (‣ tx-preamble-sent-irq               5 1)
                (‣ tx-phy-header-sent-irq             6 1)
                (‣ tx-frame-sent-irq                  7 1)
                (‣ rx-preamble-detected-irq           8 1)
                (‣ rx-sfd-deteced-irq                 9 1)
                (‣ rx-cia-done-irq                   10 1)
                (‣ rx-phy-header-detected-irq        11 1)
                (‣ rx-phy-header-error-irq           12 1)
                (‣ rx-data-ready-irq                 13 1)
                (‣ rx-fcs-good-irq                   14 1)
                (‣ rx-fcs-error-irq                  15 1)
                (‣ rx-rs-frame-sync-loss-irq         16 1)
                (‣ rx-wait-timeout-irq               17 1)
                (‣ cia-error-irq                     18 1)
                (‣ voltage-warning-irq               19 1)
                (‣ rx-overrun-irq                    20 1)
                (‣ rx-preamble-detection-timeout-irq 21 1)
                (‣ reserved                          22 1)
                (‣ spi-ready-irq                     23 1)
                (‣ idle-rc-irq                       24 1)
                (‣ pll-losing-lock-irq               25 1)
                (‣ rx-sfd-timeout-irq                26 1)
                (‣ hpd-warn-irq                      27 1)
                (‣ sts-error-irq                     28 1)
                (‣ aff-reject-irq                    29 1)
                (‣ reserved                          30 3)
                (‣ rx-preamble-reject-irq            33 1)
                (‣ reserved                          34 2)
                (‣ vt-detection-irq                  36 1)
                (‣ gpio-irq                          37 1)
                (‣ aes-done-irq                      38 1)
                (‣ aes-error-irq                     39 1)
                (‣ cmd-error-irq                     40 1)
                (‣ spi-overflow-irq                  41 1)
                (‣ spi-underflow-irq                 42 1)
                (‣ spi-error-irq                     43 1)
                (‣ cca-fail-irq                      44 1)
                (‣ reserved                          45 3)))))

(define-public reg:rx-frame-info
  (register
   (name 'rx-frame-info)
   (address #x4c)
   (width (octets 4))
   (items (list
           (‣ rx-frame-length          0 10)
           (‣ reserved                10  1)
           (‣ rx-preamble-length-low  11  2)
           (‣ rx-bit-rate             13  1 (semantics (tbl bit-rate-map)))
           (‣ reserved                14  1)
           (‣ rx-ranging-bit          15  1)
           (‣ rx-prf-report           16  2 (semantics (tbl prf-map)))
           (‣ rx-preamble-length-high 18  2)
           (‣ rx-accu-count           20 12)))))

(define-public reg:rx-time-stamp
  (register
   (name 'rx-time-stamp)
   (address #x64)
   (width (octets 5))
   (items (list (‣ rx-time-stamp 0 (octets 5))))))

(define-public reg:rx-time-stamp-raw
  (register
   (name 'rx-time-stamp-raw1)
   (address #x70)
   (width (octets 4))
   (items (list (‣ rx-time-stamp-raw 0 (octets 4))))))

(define-public reg:tx-time-stamp
  (register
   (name 'tx-time-stamp)
   (address #x74)
   (width (octets 5))
   (items (list (‣ tx-time-stamp 0 (octets 5))))))

(define-public page:general-cfg-0
  (register-map
   (name 'general-cfg-0)
   (address #x00)
   (description "Main Configuration Register Bank #0")
   (table (↔ (#x00 dw1000:reg:device-type)
             (#x02 dw1000:reg:identification)
             (#x04 dw1000:reg:euid-device)
             (#x09 dw1000:reg:euid-manufacturer)
             (#x0c dw1000:reg:short-address)
             (#x0e dw1000:reg:pan-id)
             (#x10 reg:system-cfg)
             (#x14 reg:frame-filter-cfg)
             (#x18 reg:spi-crc-read-status)
             (#x1c reg:system-time)
             (#x24 reg:tx-frame-ctrl)
             (#x2c reg:delayed-tx/rx-time)
             (#x30 reg:delayed-reference-time)
             (#x34 reg:rx-frame-wait-timeout)
             (#x38 reg:system-ctrl)
             (#x3c reg:system-event-mask)
             (#x44 reg:system-event-status)
             (#x4c reg:rx-frame-info)
             (#x64 reg:rx-time-stamp)
             (#x70 reg:rx-time-stamp-raw)
             (#x74 reg:tx-time-stamp)))))

;; 0x01 GEN_CFG_AES #1
;; Main register bank and AES configuration.

(define-public reg:tx-time-stamp-raw
  (register
   (name 'tx-time-stamp-raw1)
   (address #x00)
   (width (octets 4))
   (items (list (‣ tx-time-stamp-raw 0 (octets 4))))))

(define-public reg:tx-antenna-delay
  (register
   (name 'tx-antenna-delay)
   (address #x04)
   (width (octets 4))
   (items (list (‣ tx-antenna-delay 0 (octets 4))))))

(define-public reg:ack-and-response-turnaround-time
  (register
   (name 'ack-and-response-turnaround-time)
   (address #x08)
   (width (octets 4))
   (items (list (‣ wait-for-resp-turnaround-time  0 20)
                (‣ reserved                      20  4)
                (‣ ack-turnaround-time           24  8)))))

(define-public reg:tx-power
  (register
   (name 'tx-power)
   (address #x0c)
   (width (octets 4))
   (items (list (‣ data-tx-power-coarse                     0 2)
                (‣ data-tx-power-fine                       2 6)
                (‣ physical-header-tx-power-coarse          8 2)
                (‣ physical-header-tx-power-fine           10 6)
                (‣ synchronisation-header-tx-power-coarse  16 2)
                (‣ synchronisation-header-tx-power-fine    18 6)
                (‣ sts-tx-power-coarse                     24 2)
                (‣ sts-tx-power-fine                       26 6)))))

(define-public reg:channel-ctrl
  (register
   (name 'channel-ctrl)
   (address #x14)
   (width (octets 2))
   (items (list (‣ rf-channel        0 1) ;; Channel table?
                (‣ sfd-type          1 2 (semantics (tbl sfd-type-map)))
                (‣ tx-preamble-code  3 5)
                (‣ rx-preamble-code  8 5)
                (‣ reserved         13 3)))))

(define-public reg:low-energy-addresses
  (register
   (name 'low-energy-addresses)
   (address #x18)
   (width (octets 8))
   (items (list (‣ low-energy-address-0  0 16)
                (‣ low-energy-address-1 16 16)
                (‣ low-energy-address-2 32 16)
                (‣ low-energy-address-3 48 16)))))

(define-public reg:spi-collision-status
  (register
   (name 'spi-collision-status)
   (address #x20)
   (width (octets 1))
   (items
    (list (‣ spi-collision-status 0 5 (semantics (tbl spi-collision-map)))
          (‣ reserved             5 3)))))

(define-public reg:double-buffer-status
  (register
   (name 'double-buffer-status)
   (address #x24)
   (width (octets 1))
   (items (list (‣ db0-rx-fcs-good   0 1)
                (‣ db0-rx-data-ready 1 1)
                (‣ db0-cia-done      2 1)
                (‣ db0-sts-error     3 1)
                (‣ db1-rx-fcs-good   4 1)
                (‣ db1-rx-data-ready 5 1)
                (‣ db1-cia-done      6 1)
                (‣ db1-sts-error     7 1)))))

(define-public reg:double-buffer-rx-diagnostic-mode
  (register
   (name 'double-buffer-rx-diagnostic-mode)
   (address #x28)
   (width (octets 1))
   (items
    (list (‣ db-rx-diag-mode 0 3 (semantics (tbl db-diagnostics-map)))
          (‣ reserved        3 5)))))

(define-public reg:aes-cfg
  (register
   (name 'aes-cfg)
   (address #x30)
   (width (octets 2))
   (items
    (list
     (‣ aes-mode             0 1 (semantics (tbl aes-mode-map)))
     (‣ aes-key-size         1 2 (semantics (tbl aes-key-size-map)))
     (‣ aes-key-addr-offset  3 3)
     (‣ load-aes-key         6 1)
     (‣ aes-key-source       7 1 (semantics (tbl aes-key-source-map)))
     (‣ aes-tag-size         8 3 (semantics (tbl aes-tag-size-map)))
     (‣ aes-core-select     11 1)
     (‣ aes-otp-key-source  12 1 (semantics (tbl aes-otp-key-source-map)))
     (‣ reserved            13 3)))))

(define (make-initial-vector n)
  (let* ((name* (symbol-append 'aes-initial-vector '-
                               (string->symbol (number->string n))))
         (number-of-octets 4)
         (initial-iv-address #x34)
         (width* (octets number-of-octets))
         (address* (+ initial-iv-address
                      (* n number-of-octets))))
    (register
     (name name*)
     (address address*)
     (width width*)
     (items (list (item (name name*) (offset 0) (width width*)))))))

(define-public reg:dma-cfg
  (register
   (name 'dma-cfg)
   (address #x44)
   (width (octets 7))
   (items (list
           (‣ dma-source-port            0  3)
           (‣ dma-source-address         3 10)
           (‣ dma-destination-port      13  3)
           (‣ dma-destination-address   16 10)
           (‣ dma-crypto-endian-little? 26  1)
           (‣ reserved                  27  5)
           (‣ dma-header-size           32  7)
           (‣ dma-payload-size          39 10)
           (‣ reserved                  49  7)))))

(define-public reg:aes-commands
  (register
   (name 'aes-commands)
   (address #x4c)
   (width (octets 1))
   (items (list (‣ aes-start 0 1)
                (‣ reserved  1 7)))))

(define-public reg:aes-status
  (register
   (name 'aes-status)
   (address #x50)
   (width (octets 1))
   (items (list (‣ aes-done                    0 1)
                (‣ aes-authentication-error    1 1)
                (‣ aes-dma-error               2 1)
                (‣ aes-memory-misconfiguration 3 1)
                (‣ aes-scratch-ram-empty       4 1)
                (‣ aes-scratch-ram-full        5 1)
                (‣ reserved                    6 2)))))

(define-public reg:aes-key
  (register
   (name 'aes-key)
   (address #x54)
   (width (octets 16))
   (items (list (‣ aes-key 0 128)))))

(define-public page:general-cfg-1
  (register-map
   (name 'general-cfg-1)
   (address #x01)
   (description "Main Configuration Register Bank #1")
   (table (↔ (#x00 reg:tx-time-stamp-raw)
             (#x04 reg:tx-antenna-delay)
             (#x08 reg:ack-and-response-turnaround-time)
             (#x0c reg:tx-power)
             (#x14 reg:channel-ctrl)
             (#x18 reg:low-energy-addresses)
             (#x20 reg:spi-collision-status)
             (#x24 reg:double-buffer-status)
             (#x28 reg:double-buffer-rx-diagnostic-mode)
             (#x30 reg:aes-cfg)
             (#x34 (make-initial-vector 0))
             (#x38 (make-initial-vector 1))
             (#x3c (make-initial-vector 2))
             (#x40 (make-initial-vector 3))
             (#x44 reg:dma-cfg)
             (#x4c reg:aes-commands)
             (#x50 reg:aes-status)
             (#x54 reg:aes-key)))))

;; 0x02 STS_CFG
;; Scrambled Timestamp Sequence Configuration

(define-public reg:sts-cfg
  (register
   (name 'sts-key)
   (address #x00)
   (width (octets 1))
   (items (list (‣ sts-length 0 8)))))

(define-public reg:sts-ctrl
  (register
   (name 'sts-ctrl)
   (address #x04)
   (width (octets 1))
   (items (list (‣ load-sts-iv     0 1)
                (‣ start-from-last 1 1)
                (‣ reserved        2 6)))))

(define-public reg:sts-status
  (register
   (name 'sts-ctrl)
   (address #x08)
   (width (octets 2))
   (items (list (‣ sts-accumulator-quality  0 12)
                (‣ reserved                12  4)))))

(define-public reg:sts-key
  (register
   (name 'sts-key)
   (address #x0c)
   (width (octets 16))
   (items
    (list (‣ sts-key 0 128 (default #x738123bb5e5a4ed8df43a20c9a375fa))))))

(define-public reg:sts-iv
  (register
   (name 'sts-iv)
   (address #x1c)
   (width (octets 16))
   (items (list (‣ sts-iv 0 128 (default 1))))))

(define-public page:sts-cfg
  (register-map
   (name 'sts-cfg)
   (address #x02)
   (description
    "Scrambled Timestamp Sequence Configuration and Status Registers")
   (table (↔ (#x00 reg:sts-cfg)
             (#x04 reg:sts-ctrl)
             (#x08 reg:sts-status)
             (#x0c reg:sts-key)
             (#x1c reg:sts-iv)))))

;; 0x03 RX_TUNE
;; Receiver Tuning

(define-public reg:receiver-cfg
  (register
   (name 'receiver-cfg)
   (address #x18)
   (items (list (‣ rx-tuning-enable     0 1 (default #t))
                (‣ reserved             1 8 (default #b01111010))
                (‣ rx-tuning-threshold  9 6 (default #b111000))
                (‣ reserved            15 1 (default #t))))))

(define-public reg:dgc-cfg-0
  (register
   (name 'dgc-cfg-0)
   (address #x1c)
   (width (octets 4))
   (items (list (‣ dgc-cfg-0 0 32 (default #x10000240))))))

(define-public reg:dgc-cfg-1
  (register
   (name 'dgc-cfg-1)
   (address #x1c)
   (width (octets 4))
   (items (list (‣ dgc-cfg-1 0 32 (default #x1b6da489))))))

(define-public reg:dgc-lut-0
  (register
   (name 'dgc-lut-0)
   (address #x1c)
   (width (octets 4))
   (items (list (‣ dgc-lut-0 0 32 (default #x0001c0fd))))))

(define-public reg:dgc-lut-1
  (register
   (name 'dgc-lut-1)
   (address #x1c)
   (width (octets 4))
   (items (list (‣ dgc-lut-1 0 32 (default #x0001c43e))))))

(define-public reg:dgc-lut-2
  (register
   (name 'dgc-lut-2)
   (address #x1c)
   (width (octets 4))
   (items (list (‣ dgc-lut-2 0 32 (default #x0001c6be))))))

(define-public reg:dgc-lut-3
  (register
   (name 'dgc-lut-3)
   (address #x1c)
   (width (octets 4))
   (items (list (‣ dgc-lut-3 0 32 (default #x0001c77e))))))

(define-public reg:dgc-lut-4
  (register
   (name 'dgc-lut-4)
   (address #x1c)
   (width (octets 4))
   (items (list (‣ dgc-lut-4 0 32 (default #x0001cf36))))))

(define-public reg:dgc-lut-5
  (register
   (name 'dgc-lut-5)
   (address #x1c)
   (width (octets 4))
   (items (list (‣ dgc-lut-5 0 32 (default #x0001cfb5))))))

(define-public reg:dgc-lut-6
  (register
   (name 'dgc-lut-6)
   (address #x1c)
   (width (octets 4))
   (items (list (‣ dgc-lut-6 0 32 (default #x0001cff5))))))

(define-public reg:dgc-debug-information
  (register
   (name 'dgc-debug-information)
   (address #x60)
   (width (octets 4))
   (items (list (‣ reserved            0 28)
                (‣ dgc-decision-index 28  3)
                (‣ reserved           31  1)))))

(define-public page:receiver-tuning
  (register-map
   (name 'receiver-cfg)
   (address #x03)
   (description "Receiver Tuning")
   (table (↔ (#x18 reg:receiver-cfg)
             (#x1c reg:dgc-cfg-0)
             (#x20 reg:dgc-cfg-1)
             (#x38 reg:dgc-lut-0)
             (#x3c reg:dgc-lut-1)
             (#x40 reg:dgc-lut-2)
             (#x44 reg:dgc-lut-3)
             (#x48 reg:dgc-lut-4)
             (#x4c reg:dgc-lut-5)
             (#x50 reg:dgc-lut-6)
             (#x60 reg:dgc-debug-information)))))

;; 0x04 EXT_SYNC
;; External Sync Control and RX Calibration

(define-public reg:external-sync
  (register
   (name 'external-sync)
   (address #x00)
   (width (octets 4))
   (items (list (‣ reserved               0  3 (default 4))
                (‣ osts-wait-counter      3  8)
                (‣ ext-timebase-rst-mode 11  1)
                (‣ reserved              12 20)))))

(define-public reg:receiver-calibration
  (register
   (name 'receiver-calibration)
   (address #x0c)
   (width (octets 4))
   (items (list (‣ rx-calibration-mode        0  2)
                (‣ reserved                   2  2)
                (‣ rx-calibration-enable      4  1)
                (‣ reserved                   5 11)
                (‣ rx-calibration-tuning-val 16  4)
                (‣ reserved                  20 12)))))

(define-public reg:receiver-cal-result-i
  (register
   (name 'receiver-cal-result-i)
   (address #x14)
   (width (octets 4))
   (items (list (‣ rx-cal-block-result-i  0 29)
                (‣ reserved              29  3)))))

(define-public reg:receiver-cal-result-q
  (register
   (name 'receiver-cal-result-q)
   (address #x1c)
   (width (octets 4))
   (items (list (‣ rx-cal-block-result-q  0 29)
                (‣ reserved              29  3)))))

(define-public reg:receiver-cal-status
  (register
   (name 'receiver-cal-status)
   (address #x20)
   (width (octets 1))
   (items (list (‣ rx-cal-block-status 0 1)
                (‣ reserved            1 7)))))

(define-public page:external-sync-and-rx-calibration
  (register-map
   (name 'external-sync-and-rx-calibration)
   (address #x04)
   (description "External Sync Control and RX Calibration")
   (table (↔ (#x00 reg:external-sync)
             (#x0c reg:receiver-calibration)
             (#x14 reg:receiver-cal-result-i)
             (#x1c reg:receiver-cal-result-q)
             (#x20 reg:receiver-cal-status)))))

;; 0x05 GPIO_CTRL
;; General Purpose Input-Output Control Registers

(define-public reg:gpio-mode-ctrl
  (register
   (name 'gpio-mode-ctrl)
   (address #x00)
   (width (octets 4))
   (items
    (list
     (‣ mode-select-gpio-0  0 3 (semantics (tbl gpio-0-modes #:default 'gpio)))
     (‣ mode-select-gpio-1  3 3 (semantics (tbl gpio-1-modes #:default 'gpio)))
     (‣ mode-select-gpio-2  6 3 (semantics (tbl gpio-2-modes #:default 'gpio)))
     (‣ mode-select-gpio-3  9 3 (semantics (tbl gpio-3-modes #:default 'gpio)))
     (‣ mode-select-gpio-4 12 3 (semantics (tbl gpio-4-modes #:default 'gpio)))
     (‣ mode-select-gpio-5 15 3 (semantics (tbl gpio-5-modes #:default 'gpio)))
     (‣ mode-select-gpio-6 18 3 (semantics (tbl gpio-6-modes #:default 'gpio)))
     (‣ mode-select-gpio-7 21 3 (semantics (tbl gpio-7-modes #:default 'sync-input)))
     (‣ mode-select-gpio-8 24 3 (semantics (tbl gpio-8-modes #:default 'irq-output)))
     (‣ reserved           27 5)))))

(define-public reg:gpio-pull-ctrl
  (register
   (name 'gpio-pull-ctrl)
   (address #x04)
   (width (octets 2))
   (items (list (‣ gpio-0-pull-enable 0 1)
                (‣ gpio-1-pull-enable 1 1)
                (‣ gpio-2-pull-enable 2 1)
                (‣ gpio-3-pull-enable 3 1)
                (‣ gpio-4-pull-enable 4 1)
                (‣ gpio-5-pull-enable 5 1)
                (‣ gpio-6-pull-enable 6 1)
                (‣ gpio-7-pull-enable 7 1)
                (‣ gpio-8-pull-enable 8 1)
                (‣ reserved           9 7)))))

(define-public reg:gpio-direction-ctrl
  (register
   (name 'gpio-direction-ctrl)
   (address #x08)
   (width (octets 2))
   (items
    (list
     (‣ gpio-0-direction 0 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-1-direction 1 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-2-direction 2 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-3-direction 3 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-4-direction 4 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-5-direction 5 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-6-direction 6 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-7-direction 7 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ gpio-8-direction 8 1 (semantics (tbl gpio-direction-map #:default 'input)))
     (‣ reserved         9 7)))))

(define-public reg:gpio-data-output
  (register
   (name 'gpio-data-output)
   (address #x0c)
   (width (octets 2))
   (items (list (‣ gpio-0-output-value 0 1)
                (‣ gpio-1-output-value 1 1)
                (‣ gpio-2-output-value 2 1)
                (‣ gpio-3-output-value 3 1)
                (‣ gpio-4-output-value 4 1)
                (‣ gpio-5-output-value 5 1)
                (‣ gpio-6-output-value 6 1)
                (‣ gpio-7-output-value 7 1)
                (‣ gpio-8-output-value 8 1)
                (‣ reserved            9 7)))))

(define-public reg:gpio-interrupt-enable
  (register
   (name 'gpio-interrupt-enable)
   (address #x10)
   (width (octets 2))
   (items (list (‣ gpio-0-irq-enable 0 1)
                (‣ gpio-1-irq-enable 1 1)
                (‣ gpio-2-irq-enable 2 1)
                (‣ gpio-3-irq-enable 3 1)
                (‣ gpio-4-irq-enable 4 1)
                (‣ gpio-5-irq-enable 5 1)
                (‣ gpio-6-irq-enable 6 1)
                (‣ gpio-7-irq-enable 7 1)
                (‣ gpio-8-irq-enable 8 1)
                (‣ reserved          9 7)))))

(define-public reg:gpio-interrupt-status
  (register
   (name 'gpio-interrupt-status)
   (address #x14)
   (width (octets 2))
   (items (list (‣ gpio-0-irq-status 0 1)
                (‣ gpio-1-irq-status 1 1)
                (‣ gpio-2-irq-status 2 1)
                (‣ gpio-3-irq-status 3 1)
                (‣ gpio-4-irq-status 4 1)
                (‣ gpio-5-irq-status 5 1)
                (‣ gpio-6-irq-status 6 1)
                (‣ gpio-7-irq-status 7 1)
                (‣ gpio-8-irq-status 8 1)
                (‣ reserved          9 7)))))

(define-public reg:gpio-interrupt-sense
  (register
   (name 'gpio-interrupt-sense)
   (address #x18)
   (width (octets 2))
   (items
    (list
     (‣ gpio-0-irq-sense-mode 0 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-1-irq-sense-mode 1 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-2-irq-sense-mode 2 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-3-irq-sense-mode 3 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-4-irq-sense-mode 4 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-5-irq-sense-mode 5 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-6-irq-sense-mode 6 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-7-irq-sense-mode 7 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ gpio-8-irq-sense-mode 8 1 (semantics (tbl gpio-irq-mode-map #:default 'active-high/rising-edge)))
     (‣ reserved              9 7)))))

(define-public reg:gpio-interrupt-mode
  (register
   (name 'gpio-interrupt-mode)
   (address #x1c)
   (width (octets 4))
   (items
    (list
     (‣ gpio-0-irq-mode 0  1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-1-irq-mode 1  1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-2-irq-mode 2  1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-3-irq-mode 3  1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-4-irq-mode 4  1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-5-irq-mode 5  1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-6-irq-mode 6  1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-7-irq-mode 7  1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ gpio-8-irq-mode 8  1 (semantics (tbl gpio-irq-level/edge-map #:default 'level)))
     (‣ reserved        9 23)))))

(define-public reg:gpio-interrupt-bothedge
  (register
   (name 'gpio-interrupt-bothedge)
   (address #x20)
   (width (octets 2))
   (items (list (‣ gpio-0-bothedge-enable 0 1)
                (‣ gpio-1-bothedge-enable 1 1)
                (‣ gpio-2-bothedge-enable 2 1)
                (‣ gpio-3-bothedge-enable 3 1)
                (‣ gpio-4-bothedge-enable 4 1)
                (‣ gpio-5-bothedge-enable 5 1)
                (‣ gpio-6-bothedge-enable 6 1)
                (‣ gpio-7-bothedge-enable 7 1)
                (‣ gpio-8-bothedge-enable 8 1)
                (‣ reserved               9 7)))))

(define-public reg:gpio-interrupt-latch-clear
  (register
   (name 'gpio-interrupt-latch-clear)
   (address #x24)
   (width (octets 2))
   (items (list (‣ gpio-0-irq-latch-clear 0 1)
                (‣ gpio-1-irq-latch-clear 1 1)
                (‣ gpio-2-irq-latch-clear 2 1)
                (‣ gpio-3-irq-latch-clear 3 1)
                (‣ gpio-4-irq-latch-clear 4 1)
                (‣ gpio-5-irq-latch-clear 5 1)
                (‣ gpio-6-irq-latch-clear 6 1)
                (‣ gpio-7-irq-latch-clear 7 1)
                (‣ gpio-8-irq-latch-clear 8 1)
                (‣ reserved               9 7)))))

(define-public reg:gpio-interrupt-debounce
  (register
   (name 'gpio-interrupt-debounce)
   (address #x28)
   (width (octets 2))
   (items (list (‣ gpio-0-debounce-enable 0 1)
                (‣ gpio-1-debounce-enable 1 1)
                (‣ gpio-2-debounce-enable 2 1)
                (‣ gpio-3-debounce-enable 3 1)
                (‣ gpio-4-debounce-enable 4 1)
                (‣ gpio-5-debounce-enable 5 1)
                (‣ gpio-6-debounce-enable 6 1)
                (‣ gpio-7-debounce-enable 7 1)
                (‣ gpio-8-debounce-enable 8 1)
                (‣ reserved               9 7)))))

(define-public reg:gpio-raw-state
  (register
   (name 'gpio-raw-state)
   (address #x2c)
   (width (octets 2))
   (items (list (‣ gpio-0-io-raw-state 0 1)
                (‣ gpio-1-io-raw-state 1 1)
                (‣ gpio-2-io-raw-state 2 1)
                (‣ gpio-3-io-raw-state 3 1)
                (‣ gpio-4-io-raw-state 4 1)
                (‣ gpio-5-io-raw-state 5 1)
                (‣ gpio-6-io-raw-state 6 1)
                (‣ gpio-7-io-raw-state 7 1)
                (‣ gpio-8-io-raw-state 8 1)
                (‣ reserved            9 7)))))

(define-public page:gpio-ctrl
  (register-map
   (name 'gpio-ctrl)
   (address #x05)
   (description "General Purpose Input-Output Control Registers")
   (table (↔ (#x00 reg:gpio-mode-ctrl)
             (#x04 reg:gpio-pull-ctrl)
             (#x08 reg:gpio-direction-ctrl)
             (#x0c reg:gpio-data-output)
             (#x10 reg:gpio-interrupt-enable)
             (#x14 reg:gpio-interrupt-status)
             (#x18 reg:gpio-interrupt-sense)
             (#x1c reg:gpio-interrupt-mode)
             (#x20 reg:gpio-interrupt-bothedge)
             (#x24 reg:gpio-interrupt-latch-clear)
             (#x28 reg:gpio-interrupt-debounce)
             (#x2c reg:gpio-raw-state)))))

;; 0x06 DRX
;; Digital Receiver Configuration

(define-public reg:pac-cfg
  (register
   (name 'pac-cfg)
   (address #x00)
   (width (octets 2))
   (items (list (‣ preamble-aquisition-chunk-size 0  2)
                (‣ reserved                       2  2)
                (‣ digital-tuning-bit-4           4  1)
                (‣ reserved                       5 11)))))

(define-public reg:sfd-timeout
  (register
   (name 'sfd-timeout)
   (address #x02)
   (width (octets 2))
   (items (list (‣ sfd-detection-timeout 0 16 (default 65))))))

(define-public reg:preamble-timeout
  (register
   (name 'preamble-timeout)
   (address #x04)
   (width (octets 2))
   (items (list (‣ preamble-detection-timeout 0 16)))))

(define-public reg:receiver-tuning-3
  (register
   (name 'receiver-tuning-3)
   (address #x0c)
   (width (octets 4))
   (items
    (list (‣ digital-receiver-tuning-word-3 0 32 (default #xaf5f35cc))))))

(define-public reg:receiver-tuning-5
  (register
   (name 'receiver-tuning-5)
   (address #x14)
   (width (octets 4))
   (items (list (‣ reserved 0 32)))))

(define-public reg:carrier-recovery-integrator
  (register
   (name 'carrier-recovery-integrator)
   (address #x29)
   (width (octets 3))
   (items (list (‣ remote-tx-freq-offset-estimate 0 24)))))

(define-public page:digital-rx-cfg
  (register-map
   (name 'digital-rx-cfg)
   (address #x06)
   (description "Digital Receiver Configuration")
   (table (↔ (#x00 reg:pac-cfg)
             (#x02 reg:sfd-timeout)
             (#x04 reg:preamble-timeout)
             (#x0c reg:receiver-tuning-3)
             (#x14 reg:receiver-tuning-5)
             (#x29 reg:carrier-recovery-integrator)))))

;; 0x07 RF_CONF
;; Analog RF configuration Block

(define-public reg:rf-enable
  (register
   (name 'rf-enable)
   (address #x00)
   (width (octets 4))
   (items (list (‣ rf-enable 0 32 (default 33569792))))))

(define-public reg:rf-ctrl-mask
  (register
   (name 'rf-ctrl-mask)
   (address #x04)
   (width (octets 4))
   (items (list (‣ rf-ctrl-mask 0 32 (default 33569792))))))

(define-public reg:rf-switch-cfg
  (register
   (name 'rf-switch-cfg)
   (address #x14)
   (width (octets 4))
   (items
    (list
     (‣ antenna-auto-toggle-enable  0 1
                                    (semantics boolean/active-low)
                                    (default #t))
     (‣ pdoa-starting-port-select   1 1)
     (‣ reserved                    2 6)
     (‣ manual-antenna-switch-ctrl  8 1)
     (‣ reserved                    9 3)
     (‣ antenna-switch-ctrl        12 3)
     (‣ reserved                   15 1)
     (‣ manual-txrx-switch         16 1)
     (‣ reserved                   17 7)
     (‣ txrx-switch-ctrl           24 6)
     (‣ reserved                   30 2)))))

(define-public reg:rf-tx-ctrl-0
  (register
   (name 'rf-tx-ctrl-0)
   (address #x1a)
   (width (octets 1))
   (items (list (‣ analog-tx-ctrl-1 0 8 (default 14))))))

(define-public reg:rf-tx-ctrl-1
  (register
   (name 'rf-tx-ctrl-1)
   (address #x1c)
   (width (octets 4))
   (items (list (‣ pulse-generator-delay 0  6 (default 52))
                (‣ reserved              6 26 (default 7347268))))))

(define-public reg:tx-test-cfg
  (register
   (name 'tx-test-cfg)
   (address #x28)
   (width (octets 1))
   (items (list (‣ tx-test-select 0 4)
                (‣ reserved       4 4)))))

(define-public reg:tx-sar-test
  (register
   (name 'tx-sar-test)
   (address #x34)
   (width (octets 1))
   (items (list (‣ reserved                    0 2)
                (‣ sar-temperature-read-enable 2 1)
                (‣ reserved                    3 5)))))

(define-public reg:ldo-tune
  (register
   (name 'ldo-tune)
   (address #x40)
   (width (octets 8))
   (items (list (‣ internal-ldo-tuning-word 0 60)
                (‣ reserved                 60 4)))))

(define-public reg:ldo-ctrl
  (register
   (name 'ldo-ctrl)
   (address #x48)
   (width (octets 4))
   (items (list (‣ ldo-control 0 32)))))

(define-public reg:ldo-load
  (register
   (name 'ldo-load)
   (address #x51)
   (width (octets 1))
   (items (list (‣ ldo-tuning-word 0 8 (default 20))))))

(define-public page:analog-rf-cfg
  (register-map
   (name 'analog-rf-cfg)
   (address #x07)
   (description "Analog RF configuration Block")
   (table (↔ (#x00 reg:rf-enable)
             (#x04 reg:rf-ctrl-mask)
             (#x14 reg:rf-switch-cfg)
             (#x1a reg:rf-tx-ctrl-0)
             (#x1c reg:rf-tx-ctrl-1)
             (#x28 reg:tx-test-cfg)
             (#x34 reg:tx-sar-test)
             (#x40 reg:ldo-tune)
             (#x48 reg:ldo-ctrl)
             (#x51 reg:ldo-load)))))

;; 0x08 TX_CAL
;; Transmitter Calibration Block

(define-public reg:tx-cal-sar-ctrl
  (register
   (name 'tx-cal-sar-ctrl)
   (address #x00)
   (width (octets 1))
   (items (list (‣ sar-start 0 1)
                (‣ reserved  1 7)))))

(define-public reg:tx-cal-sar-status
  (register
   (name 'tx-cal-sar-status)
   (address #x04)
   (width (octets 2))
   (items (list (‣ sar-done 0  1)
                (‣ reserved 1 15)))))

(define-public reg:tx-cal-sar-value-latest
  (register
   (name 'tx-cal-sar-value-latest)
   (address #x08)
   (width (octets 2))
   (items (list (‣ sar-voltage      0 8)
                (‣ sar-temperature  8 8)))))

(define-public reg:tx-cal-sar-value-wakeup
  (register
   (name 'tx-cal-sar-value-wakeup)
   (address #x0c)
   (width (octets 2))
   (items (list (‣ sar-wake-up-voltage     0 8)
                (‣ sar-wake-up-temperature 8 8)))))

(define-public reg:tx-cal-pulsegen-ctrl
  (register
   (name 'tx-cal-pulsegen-ctrl)
   (address #x10)
   (width (octets 1))
   (items (list (‣ pulsegen-calibration-start      0 1)
                (‣ pulsegen-auto-calibration-start 1 1)
                (‣ pulsegen-calibration-time       2 4)
                (‣ reserved                        6 2 (default #b10))))))

(define-public reg:tx-cal-pulsegen-status
  (register
   (name 'tx-cal-pulsegen-status)
   (address #x14)
   (width (octets 2))
   (items (list (‣ pulsegen-delay-count         0 12)
                (‣ pulsegen-delay-autocal-done 12  1)
                (‣ reserved                    13  3)))))

(define-public reg:tx-cal-pulsegen-test
  (register
   (name 'tx-cal-pulsegen-test)
   (address #x18)
   (width (octets 2))
   (items (list (‣ pulsegen-mode 0 16)))))

(define-public reg:tx-cal-pulsegen-target
  (register
   (name 'tx-cal-pulsegen-target)
   (address #x1c)
   (width (octets 2))
   (items (list (‣ pulsegen-target-value 0 12 (default 166))
                (‣ reserved              12 4)))))

(define-public page:tx-calibration
  (register-map
   (name 'tx-calibration)
   (address #x08)
   (description "Transmitter Calibration Block")
   (table (↔ (#x00 reg:tx-cal-sar-ctrl)
             (#x04 reg:tx-cal-sar-status)
             (#x08 reg:tx-cal-sar-value-latest)
             (#x0c reg:tx-cal-sar-value-wakeup)
             (#x10 reg:tx-cal-pulsegen-ctrl)
             (#x14 reg:tx-cal-pulsegen-status)
             (#x18 reg:tx-cal-pulsegen-test)
             (#x1c reg:tx-cal-pulsegen-target)))))

;; 0x09 FS_CTRL
;; Frequency Synthesizer Control

(define-public reg:pll-cfg
  (register
   (name 'pll-cfg)
   (address #x00)
   (width (octets 2))
   (items (list (‣ pll-config 0 16 (default 7996))))))

(define-public reg:pll-coarse-code
  (register
   (name 'pll-coarse-code)
   (address #x04)
   (width (octets 4))
   (items (list (‣ pll-coarse-code-ch9  0  8 (default 11))
                (‣ pll-coarse-code-ch5 14  8 (default 15))
                (‣ reserved            22 10)))))

(define-public reg:pll-calibration
  (register
   (name 'pll-calibration)
   (address #x08)
   (width (octets 2))
   (items (list (‣ reserved        0 1)
                (‣ pll-cal-use-old 1 1)
                (‣ reserved        2 2)
                (‣ pll-cal-config  4 4 (default 49))
                (‣ pll-cal-enable  8 1)
                (‣ reserved        9 7)))))

(define-public reg:xtal-trim
  (register
   (name 'xtal-trim)
   (address #x14)
   (width (octets 1))
   (items (list (‣ xtal-trim-value 0 6)
                (‣ reserved        6 2)))))

(define-public page:frequency-synth-ctrl
  (register-map
   (name 'frequency-synth-ctrl)
   (address #x09)
   (description "Frequency Synthesizer Control")
   (table (↔ (#x00 reg:pll-cfg)
             (#x04 reg:pll-coarse-code)
             (#x08 reg:pll-calibration)
             (#x14 reg:xtal-trim)))))

;; 0x0a AON
;; Always-On System Control Interface

(define-public reg:aon-wake-up-cfg
  (register
   (name 'aon-wake-up-cfg)
   (address #x00)
   (width (octets 3))
   (items (list (‣ wake-up-always-on-data-download  0  1)
                (‣ wake-up-run-sar                  1  1)
                (‣ reserved                         2  6)
                (‣ wake-up-goto-pll-from-idle       8  1)
                (‣ wake-up-goto-rx-from-pll         9  1)
                (‣ reserved                        10  1)
                (‣ wake-up-run-rx-calibration      11  1)
                (‣ reserved                        12 12)))))

(define-public reg:aon-ctrl
  (register
   (name 'aon-ctrl)
   (address #x04)
   (width (octets 1))
   (items (list (‣ always-on-restore                  0 1)
                (‣ always-on-save                     1 1)
                (‣ always-on-cfg-upload               2 1)
                (‣ always-on-direct-access-read       3 1)
                (‣ always-on-direct-access-write      4 1)
                (‣ always-on-direct-access-write-high 5 1)
                (‣ reserved                           6 1)
                (‣ always-on-direct-access-enable     7 1)))))

(define-public reg:aon-direct-access-read-data
  (register
   (name 'aon-direct-access-read-data)
   (address #x08)
   (width (octets 1))
   (items (list (‣ always-on-access-read-data 0 8)))))

(define-public reg:aon-direct-access-address
  (register
   (name 'aon-direct-access-address)
   (address #x0c)
   (width (octets 2))
   (items (list (‣ always-on-access-address 0 16)))))

(define-public reg:aon-direct-access-write-data
  (register
   (name 'aon-direct-access-write-data)
   (address #x10)
   (width (octets 1))
   (items (list (‣ always-on-access-write-data 0 8)))))

(define-public reg:aon-cfg
  (register
   (name 'aon-cfg)
   (address #x14)
   (width (octets 1))
   (items (list (‣ always-on-sleep-enable           0 1)
                (‣ always-on-wake-count             1 1)
                (‣ always-on-brownout-detect-enable 2 1 (default #t))
                (‣ always-on-wake-on-spi            3 1 (default #t))
                (‣ always-on-wake-on-wakeup-pin     4 1)
                (‣ always-on-preserve-sleep         5 1)
                (‣ reserved                         6 2)))))

(define-public page:always-on-system-control
  (register-map
   (name 'always-on-system-control)
   (address #x0a)
   (description "Always-On System Control Interface")
   (table (↔ (#x00 reg:aon-wake-up-cfg)
             (#x04 reg:aon-ctrl)
             (#x08 reg:aon-direct-access-read-data)
             (#x0c reg:aon-direct-access-address)
             (#x10 reg:aon-direct-access-write-data)
             (#x14 reg:aon-cfg)))))

;; 0x0b OTP_IF
;; OTP Memory Interface

(define-public reg:otp-write-data
  (register
   (name 'otp-write-data)
   (address #x00)
   (width (octets 4))
   (items (list (‣ otp-write-data-word 0 32)))))

(define-public reg:otp-address
  (register
   (name 'otp-address)
   (address #x04)
   (width (octets 2))
   (items (list (‣ otp-access-address  0 11)
                (‣ reserved           11  5)))))

(define-public reg:otp-cfg
  (register
   (name 'otp-cfg)
   (address #x08)
   (width (octets 2))
   (items (list (‣ otp-manual-control   0 1)
                (‣ otp-read-enable      1 1)
                (‣ otp-write-enable     2 1)
                (‣ otp-write-mode       3 1)
                (‣ reserved             4 2)
                (‣ otp-kick-dgc         6 1)
                (‣ otp-kick-ldo         7 1)
                (‣ reserved             8 2)
                (‣ otp-kick-bias       10 1)
                (‣ otp-kick-ops        11 2)
                (‣ otp-kick-ops-params 13 1)
                (‣ reserved            14 2)))))

(define-public reg:otp-status
  (register
   (name 'otp-status)
   (address #x0c)
   (width (octets 1))
   (items (list (‣ otp-programming-done 0 1)
                (‣ otp-vpp-ok           1 1)
                (‣ reserved             2 6)))))

(define-public reg:otp-read-data
  (register
   (name 'otp-read-data)
   (address #x10)
   (width (octets 4))
   (items (list (‣ otp-read-data-word 0 32)))))

(define-public reg:otp-special
  (register
   (name 'otp-special)
   (address #x14)
   (width (octets 4))
   (items (list (‣ otp-special-read-data-word 0 32)))))

(define-public page:opt-interface
  (register-map
   (name 'opt-interface)
   (address #x0b)
   (description "OTP Memory Interface")
   (table (↔ (#x00 reg:otp-write-data)
             (#x04 reg:otp-address)
             (#x08 reg:otp-cfg)
             (#x0c reg:otp-status)
             (#x10 reg:otp-read-data)
             (#x14 reg:otp-special)))))

;; 0x0c CIA
;; Channel Impulse response Analyser (CIA) Interface #0

(define-public reg:preamble-rx-timestamp-status
  (register
   (name 'preamble-rx-timestamp-status)
   (address #x00)
   (description "Preamble sequence receive time stamp and status")
   (width (octets 8))
   (items (list (‣ ipatov-toa         0 40)
                (‣ ipatov-poa        40 14)
                (‣ reserved          54  2)
                (‣ ipatov-toa-status 56  8)))))

(define-public reg:sts-0-rx-timestamp-status
  (register
   (name 'sts-0-rx-timestamp-status)
   (address #x08)
   (description "STS 0 receive time stamp and status")
   (width (octets 8))
   (items (list (‣ sts-0-toa                   0 40)
                (‣ sts-0-poa                  40 14)
                (‣ reserved                   54  2)
                (‣ sts-0-fp-no-strong-edge    56  1)
                (‣ sts-0-noise-thresh-lowered 57  1)
                (‣ sts-0-cir-too-weak         58  1)
                (‣ sts-0-cfp-too-close        59  1)
                (‣ sts-0-fp-too-close         60  1)
                (‣ sts-0-cq-enable-fail       61  1)
                (‣ sts-0-ss-enable-fail       62  1)
                (‣ sts-0-pgr-enable-fail      63  1)))))

(define-public reg:sts-1-rx-timestamp-status
  (register
   (name 'sts-1-rx-timestamp-status)
   (address #x10)
   (description "STS 1 receive time stamp and status")
   (width (octets 8))
   (items (list (‣ sts-1-toa                   0 40)
                (‣ sts-1-poa                  40 14)
                (‣ reserved                   54  2)
                (‣ sts-1-fp-no-strong-edge    56  1)
                (‣ sts-1-noise-thresh-lowered 57  1)
                (‣ sts-1-cir-too-weak         58  1)
                (‣ sts-1-cfp-too-close        59  1)
                (‣ sts-1-fp-too-close         60  1)
                (‣ sts-1-cq-enable-fail       61  1)
                (‣ sts-1-ss-enable-fail       62  1)
                (‣ sts-1-pgr-enable-fail      63  1)))))

(define-public reg:cir-tdoa
  (register
   (name 'cir-tdoa)
   (address #x18)
   (description "The TDoA between the two CIRs")
   (width (octets 6))
   (items (list (‣ cir-tdoa 0 (octets 6))))))

(define-public reg:cir-pdoa
  (register
   (name 'cir-pdoa)
   (address #x1e)
   (description "The PDoA between the two CIRs")
   (width (octets 2))
   (items (list (‣ cir-pdoa        0 14)
                (‣ fp-thresh-test 14  1)
                (‣ reserved       15  1)))))

(define-public reg:cia-diagnostic-0
  (register
   (name 'cia-diagnostic-0)
   (address #x20)
   (description "CIA Diagnostic 0")
   (width (octets 4))
   (items (list
           (‣ clock-offset-estimation  0 13)
           (‣ reserved                13 19)))))

(define-public reg:preamble-diagnostic-peak
  (register
   (name 'preamble-diagnostic-peak)
   (address #x28)
   (description "Preamble Diagnostic: Peak")
   (width (octets 4))
   (items (list (‣ preamble-peak-magnitude  0 21)
                (‣ preamble-peak-index     21 10)
                (‣ reserved                31  1)))))

(define-public reg:preamble-diagnostic-power
  (register
   (name 'preamble-diagnostic-power)
   (address #x2c)
   (description "Preamble Diagnostic: Power Indication")
   (width (octets 4))
   (items (list (‣ preamble-channel-area  0 17)
                (‣ reserved              17 15)))))

(define-public reg:preamble-diagnostic-magnitude-fp1
  (register
   (name 'preamble-diagnostic-magnitude-fp1)
   (address #x30)
   (description "Preamble Diagnostic: Magnitude at FP + 1")
   (width (octets 4))
   (items (list (‣ preamble-magnitude-fp1  0 22)
                (‣ reserved               22 10)))))

(define-public reg:preamble-diagnostic-magnitude-fp2
  (register
   (name 'preamble-diagnostic-magnitude-fp2)
   (address #x34)
   (description "Preamble Diagnostic: Magnitude at FP + 2")
   (width (octets 4))
   (items (list (‣ preamble-magnitude-fp2  0 22)
                (‣ reserved               22 10)))))

(define-public reg:preamble-diagnostic-magnitude-fp3
  (register
   (name 'preamble-diagnostic-magnitude-fp3)
   (address #x38)
   (description "Preamble Diagnostic: Magnitude at FP + 3")
   (width (octets 4))
   (items (list (‣ preamble-magnitude-fp3  0 22)
                (‣ reserved               22 10)))))

(define-public reg:preamble-diagnostic-fp
  (register
   (name 'preamble-diagnostic-fp)
   (address #x48)
   (description "Preamble Diagnostic: First Path")
   (width (octets 4))
   (items (list (‣ preamble-fp-index  0 16)
                (‣ reserved          16 16)))))

(define-public reg:preamble-diagnostic-symbols
  (register
   (name 'preamble-diagnostic-symbols)
   (address #x58)
   (description "Preamble Diagnostic: Symbols Accumulated")
   (width (octets 4))
   (items (list (‣ preamble-accumulated-symbols  0 12)
                (‣ reserved                     12 20)))))

(define-public reg:sts-0-cir-peak
  (register
   (name 'sts-0-cir-peak)
   (address #x5c)
   (description "STS 0 Diagnostic: STS CIR Peak")
   (width (octets 4))
   (items (list (‣ sts-0-peak-magnitude  0 21)
                (‣ sts-0-peak-index     21  9)
                (‣ reserved             30  2)))))

(define-public reg:sts-0-cir-power
  (register
   (name 'sts-0-cir-power)
   (address #x60)
   (description "STS 0 Diagnostic: STS CIR Power Indication")
   (width (octets 4))
   (items (list (‣ sts-0-channel-area  0 16)
                (‣ reserved           16 16)))))

(define-public reg:sts-0-cir-magnitude-fp1
  (register
   (name 'sts-0-cir-magnitude-fp1)
   (address #x64)
   (description "STS 0 Diagnostic: STS CIR Magnitude at FP + 1")
   (width (octets 4))
   (items (list (‣ sts-0-fp1-magnitude  0 22)
                (‣ reserved            22 10)))))

(define-public reg:sts-0-cir-magnitude-fp2
  (register
   (name 'sts-0-cir-magnitude-fp2)
   (address #x68)
   (description "STS 0 Diagnostic: STS CIR Magnitude at FP + 2")
   (width (octets 4))
   (items (list (‣ sts0-fp2-magnitude  0 22)
                (‣ reserved           22 10)))))

(define-public page:channel-impulse-analyzer-0
  (register-map
   (name 'channel-impulse-analyzer-0)
   (address #x0c)
   (description "Channel Impulse response Analyser (CIA) Interface #0")
   (table (↔ (#x00 reg:preamble-rx-timestamp-status)
             (#x08 reg:sts-0-rx-timestamp-status)
             (#x10 reg:sts-1-rx-timestamp-status)
             (#x18 reg:cir-tdoa)
             (#x1e reg:cir-pdoa)
             (#x20 reg:cia-diagnostic-0)
             (#x28 reg:preamble-diagnostic-peak)
             (#x2c reg:preamble-diagnostic-power)
             (#x30 reg:preamble-diagnostic-magnitude-fp1)
             (#x34 reg:preamble-diagnostic-magnitude-fp2)
             (#x38 reg:preamble-diagnostic-magnitude-fp3)
             (#x48 reg:preamble-diagnostic-fp)
             (#x58 reg:preamble-diagnostic-symbols)
             (#x5c reg:sts-0-cir-peak)
             (#x60 reg:sts-0-cir-power)
             (#x64 reg:sts-0-cir-magnitude-fp1)
             (#x68 reg:sts-0-cir-magnitude-fp2)))))

;; 0x0d CIA
;; Channel Impulse response Analyser (CIA) Interface #1

(define-public reg:sts-0-cir-magnitude-fp3
  (register
   (name 'sts-0-cir-magnitude-fp3)
   (address #x00)
   (description "STS 0 Diagnostic: STS CIR Magnitude at FP + 3")
   (width (octets 4))
   (items (list (‣ sts-0-fp3-magnitude  0 22)
                (‣ reserved            22 10)))))

(define-public reg:sts-0-cir-fp
  (register
   (name 'sts-0-cir-fp)
   (address #x10)
   (description "STS 0 Diagnostic: STS CIR First Path")
   (width (octets 4))
   (items (list (‣ sts-0-fp-index  0 15)
                (‣ reserved       15 17)))))

(define-public reg:sts-0-cur-accumulated-length
  (register
   (name 'sts-0-cur-accumulated-length)
   (address #x20)
   (description "STS 0 Diagnostic: STS CIR Accumulated STS Length")
   (width (octets 4))
   (items (list (‣ sts-0-accumulated-symbols  0 11)
                (‣ reserved                  11 21)))))

(define-public reg:sts-1-cir-peak
  (register
   (name 'sts-1-cir-peak)
   (address #x38)
   (description "STS 1 Diagnostic: STS CIR Peak")
   (width (octets 4))
   (items (list (‣ sts-1-peak-amplitude  0 21)
                (‣ sts-1-peak-index     21  9)
                (‣ reserved             30  2)))))

(define-public reg:sts-1-cir-power
  (register
   (name 'sts-1-cir-power)
   (address #x3c)
   (description "STS 1 Diagnostic: STS CIR Power Indication")
   (width (octets 4))
   (items (list (‣ sts-1-channel-area  0 16)
                (‣ reserved           16 16)))))

(define-public reg:sts-1-cir-magnitude-fp1
  (register
   (name 'sts-1-cir-magnitude-fp1)
   (address #x40)
   (description "STS 1 Diagnostic: STS CIR Magnitude at FP + 1")
   (width (octets 4))
   (items (list (‣ sts-1-fp1-magnitude  0 22)
                (‣ reserved            22 10)))))

(define-public reg:sts-1-cir-magnitude-fp2
  (register
   (name 'sts-1-cir-magnitude-fp2)
   (address #x44)
   (description "STS 1 Diagnostic: STS CIR Magnitude at FP + 2")
   (width (octets 4))
   (items (list (‣ sts-1-fp2-magnitude  0 22)
                (‣ reserved            22 10)))))

(define-public reg:sts-1-cir-magnitude-fp3
  (register
   (name 'sts-1-cir-magnitude-fp3)
   (address #x48)
   (description "STS 1 Diagnostic: STS CIR Magnitude at FP + 3")
   (width (octets 4))
   (items (list (‣ sts-1-fp3-magnitude  0 22)
                (‣ reserved            22 10)))))

(define-public reg:sts-1-cir-fp
  (register
   (name 'sts-1-cir-fp)
   (address #x58)
   (description "STS 1 Diagnostic: STS CIR First Path")
   (width (octets 4))
   (items (list (‣ sts-1-fp-index  0 15)
                (‣ reserved       15 17)))))

(define-public reg:sts-1-cur-accumulated-length
  (register
   (name 'sts-1-cur-accumulated-length)
   (address #x68)
   (description "STS 1 Diagnostic: STS CIR Accumulated STS Length")
   (width (octets 4))
   (items (list (‣ sts-1-accumulated-symbols  0 11)
                (‣ reserved                  11 21)))))

(define-public page:channel-impulse-analyzer-1
  (register-map
   (name 'channel-impulse-analyzer-1)
   (address #x0d)
   (description "Channel Impulse response Analyser (CIA) Interface #1")
   (table (↔ (#x00 reg:sts-0-cir-magnitude-fp3)
             (#x10 reg:sts-0-cir-fp)
             (#x20 reg:sts-0-cur-accumulated-length)
             (#x38 reg:sts-1-cir-peak)
             (#x3c reg:sts-1-cir-power)
             (#x40 reg:sts-1-cir-magnitude-fp1)
             (#x44 reg:sts-1-cir-magnitude-fp2)
             (#x48 reg:sts-1-cir-magnitude-fp3)
             (#x58 reg:sts-1-cir-fp)
             (#x68 reg:sts-1-cur-accumulated-length)))))

;; 0x0e CIA
;; Channel Impulse response Analyser (CIA) Interface #2

(define-public reg:cia-general-cfg
  (register
   (name 'cia-general-cfg)
   (address #x00)
   (description "CIA General Configuration")
   (width (octets 4))
   (items (list (‣ rx-antenna-delay     0 16 (default 16405))
                (‣ reserved            16  4 (default 1))
                (‣ minimal-diag-enable 20  1 (default #t))
                (‣ reserved            21 11)))))

(define-public reg:fp-temperature-adjustment
  (register
   (name 'fp-temperature-adjustment)
   (address #x04)
   (description "First Path Temp Adjustment and Thresholds")
   (width (octets 4))
   (items (list (‣ reserved             0  8 (default 16))
                (‣ fp-agreed-threshold  8  3 (default 3))
                (‣ calibration-temp    11  8)
                (‣ reserved            19  1 (default #t))
                (‣ ant-delay-temp-comp 20  1 (default #t))
                (‣ reserved            21 11 (default 3))))))

(define-public reg:cia-preamble-cfg
  (register
   (name 'cia-preamble-cfg)
   (address #x0c)
   (description "Preamble Config: CIA Preamble Configuration")
   (width (octets 4))
   (items
    (list (‣ preamble-noise-threshold-multiplier    0  5 (default #b01101))
          (‣ preamble-peak-multiplier               5  2 (default #b10))
          (‣ reserved                               7  9 (default 4))
          (‣ preamble-replica-threshold-multiplier 16  5 (default #b00100))
          (‣ reserved                              21 11)))))

(define-public reg:cia-sts-0-cfg
  (register
   (name 'cia-sts-0-cfg)
   (address #x12)
   (description "STS Config 0: CIA STS Configuration")
   (width (octets 4))
   (items
    (list (‣ sts-noise-threshold-multiplier  0 5 (default #b01100))
          (‣ sts-peak-multiplier             5 2)
          (‣ reserved                        7 9 (default #b001110100))
          (‣ sts-minimum-threshold          16 7 (default #b0010000))
          (‣ reserved                       23 9)))))

(define-public reg:cia-sts-1-cfg
  (register
   (name 'cia-sts-1-cfg)
   (address #x16)
   (description "STS Config 1: CIA STS Configuration")
   (width (octets 4))
   (items
    (list
     (‣ tuning-value                             0  8 (default #x9b))
     (‣ reserved                                 8 20 (default #x03eed))
     (‣ toa-estimation-check-enable             28  1 (default #t))
     (‣ sts-cir-consistency-check-enable        29  1 (default #t))
     (‣ sts-statistics-compare-enable           30  1 (default #t))
     (‣ sts-vs-preamble-growth-rate-test-enable 31  1 (default #t))))))

(define-public reg:cia-pdoa-adjustment
  (register
   (name 'cia-pdoa-adjustment)
   (address #x1a)
   (description "User Adjustment to the PDoA")
   (width (octets 4))
   (items (list (‣ pdoa-adjustment  0 14)
                (‣ reserved        14 18)))))

(define-public page:channel-impulse-analyzer-2
  (register-map
   (name 'channel-impulse-analyzer-2)
   (address #x0e)
   (description "Channel Impulse response Analyser (CIA) Interface #2")
   (table (↔ (#x00 reg:cia-general-cfg)
             (#x04 reg:fp-temperature-adjustment)
             (#x0c reg:cia-preamble-cfg)
             (#x12 reg:cia-sts-0-cfg)
             (#x16 reg:cia-sts-1-cfg)
             (#x1a reg:cia-pdoa-adjustment)))))

;; 0x0f DIG_DIAG
;; Digital Diagnostics Interface

(define-public reg:event-counter-ctrl
  (register
   (name 'event-counter-ctrl)
   (address #x00)
   (description "Event Counter Control")
   (width (octets 1))
   (items (list (‣ event-counters-enable 0 1)
                (‣ event-counters-clear  1 1)
                (‣ reserved              2 6)))))

(define-public reg:physical-header-error-counter
  (register
   (name 'physical-header-error-counter)
   (address #x04)
   (description "Physical Header Error Event Counter")
   (width (octets 2))
   (items (list (‣ phr-error-counter  0 12)
                (‣ reserved          12  4)))))

(define-public reg:reed-solomon-decoder-error-counter
  (register
   (name 'reed-solomon-decoder-error-counter)
   (address #x06)
   (description "Reed Solomon Decoder Error Event Counter")
   (width (octets 2))
   (items (list (‣ rsd-error-counter  0 12)
                (‣ reserved          12  4)))))

(define-public reg:frame-check-seq-good-counter
  (register
   (name 'frame-check-seq-good-counter)
   (address #x08)
   (description "Frame Check Sequence Good Event Counter")
   (width (octets 2))
   (items (list (‣ fcs-good-counter  0 12)
                (‣ reserved         12  4)))))

(define-public reg:frame-check-seq-error-counter
  (register
   (name 'frame-check-seq-error-counter)
   (address #x0a)
   (description "Frame Check Sequence Error Counter")
   (width (octets 2))
   (items (list (‣ fcs-error-counter  0 12)
                (‣ reserved          12  4)))))

(define-public reg:frame-filter-rejection-counter
  (register
   (name 'frame-filter-rejection-counter)
   (address #x0c)
   (description "Frame Filter Rejection Counter")
   (width (octets 1))
   (items (list (‣ frame-filter-rejection 0 8)))))

(define-public reg:rx-overrun-error-counter
  (register
   (name 'rx-overrun-error-counter)
   (address #x0e)
   (description "RX Overrun Error Counter")
   (width (octets 1))
   (items (list (‣ rx-overrun-error-counter 0 8)))))

(define-public reg:sfd-timeout-counter
  (register
   (name 'sfd-timeout-counter)
   (address #x10)
   (description "SFD Timeout Error Counter")
   (width (octets 2))
   (items (list (‣ sfd-timeout-counter  0 12)
                (‣ reserved            12  4)))))

(define-public reg:preamble-detect-timeout-counter
  (register
   (name 'preamble-detect-timeout-counter)
   (address #x12)
   (description "Preamble Detection Timeout Event Counter")
   (width (octets 2))
   (items (list (‣ preamble-detect-timeout-counter  0 12)
                (‣ reserved                        12  4)))))

(define-public reg:rx-frame-wait-timeout-counter
  (register
   (name 'rx-frame-wait-timeout-counter)
   (address #x14)
   (description "RX Frame Wait Timeout Counter")
   (width (octets 1))
   (items (list (‣ rx-frame-wait-timeout-counter 0 8)))))

(define-public reg:tx-frame-sent-counter
  (register
   (name 'tx-frame-sent-counter)
   (address #x16)
   (description "TX Frame Sent Counter")
   (width (octets 2))
   (items (list (‣ tx-frame-counter  0 12)
                (‣ reserved         12  4)))))

(define-public reg:half-period-warning-counter
  (register
   (name 'half-period-warning-counter)
   (address #x18)
   (description "Half Period Warning Counter")
   (width (octets 1))
   (items (list (‣ half-period-warning-counter 0 8)))))

(define-public reg:spi-write-crc-error-counter
  (register
   (name 'spi-write-crc-error-counter)
   (address #x1a)
   (description "SPI Write CRC Error Counter")
   (width (octets 1))
   (items (list (‣ spi-write-crc-error-counter 0 8)))))

(define-public reg:digital-diagnostic-reserved
  (register
   (name 'digital-diagnostic-reserved)
   (address #x1c)
   (description "Digital Diagnostics Reserved Area")
   (width (octets 8))
   (items (list (‣ reserved 0 (octets 8))))))

(define-public reg:test-mode-ctrl
  (register
   (name 'test-mode-ctrl)
   (address #x24)
   (description "Test Mode Control Register")
   (width (octets 4))
   (items (list (‣ reserved                            0  4)
                (‣ tx-power-spectrum-test-mode-enable  4  1)
                (‣ reserved                            5 16)
                (‣ host-irq-polarity                  21  1)
                (‣ reserved                           22  2)
                (‣ cia-watchdog-enable                24  1)
                (‣ reserved                           25  1)
                (‣ cia-manual-run-enable              26  1)
                (‣ reserved                           27  5)))))

(define-public reg:sts-quality-error-counter
  (register
   (name 'sts-quality-error-counter)
   (address #x28)
   (description "STS Quality Error Counter")
   (width (octets 1))
   (items (list (‣ sta-quality-error-counter 0 8)))))

(define-public reg:low-voltage-warning-counter
  (register
   (name 'low-voltage-warning-counter)
   (address #x2a)
   (description "Low Voltage Warning Counter")
   (width (octets 1))
   (items (list (‣ low-voltage-warning-counter 0 8)))))

(define-public reg:spi-mode
  (register
   (name 'spi-mode)
   (address #x2c)
   (description "SPI Mode")
   (width (octets 1))
   (items (list (‣ spi-mode-clk-polarity 0 1)
                (‣ spi-mode-clk-phase    1 1)
                (‣ reserved              2 6)))))

(define-public reg:system-state
  (register
   (name 'system-state)
   (address #x30)
   (description "System State")
   (width (octets 4))
   (items (list (‣ tx-state    0  4)
                (‣ reserved    4  4)
                (‣ rx-state    8  6)
                (‣ reserved   14  2)
                (‣ psmc-state 16  5)
                (‣ reserved   21 11)))))

(define-public reg:fast-command-status
  (register
   (name 'fast-command-status)
   (address #x3c)
   (description "Fast Command Status")
   (width (octets 1))
   (items (list (‣ fast-command-status 0 5)
                (‣ reserved            5 3)))))

(define-public reg:current-sts-iv-lsb32
  (register
   (name 'current-sts-iv-lsb32)
   (address #x48)
   (description "Current Value of the Low 32-bits of the STS IV")
   (width (octets 4))
   (items (list (‣ counter-debug-sts-iv 0 (octets 4))))))

(define-public reg:spi-crc-lfsr-init-code
  (register
   (name 'spi-crc-lfsr-init-code)
   (address #x4c)
   (description "SPI CRC LFSR Initialisation Code")
   (width (octets 1))
   (items (list (‣ spi-crc-lfsr-init-code 0 8)))))

(define-public page:digital-diagnostics
  (register-map
   (name 'digital-diagnostics)
   (address #x0f)
   (description "Digital Diagnostics Interface")
   (table (↔ (#x00 reg:event-counter-ctrl)
             (#x04 reg:physical-header-error-counter)
             (#x06 reg:reed-solomon-decoder-error-counter)
             (#x08 reg:frame-check-seq-good-counter)
             (#x0a reg:frame-check-seq-error-counter)
             (#x0c reg:frame-filter-rejection-counter)
             (#x0e reg:rx-overrun-error-counter)
             (#x10 reg:sfd-timeout-counter)
             (#x12 reg:preamble-detect-timeout-counter)
             (#x14 reg:rx-frame-wait-timeout-counter)
             (#x16 reg:tx-frame-sent-counter)
             (#x18 reg:half-period-warning-counter)
             (#x1a reg:spi-write-crc-error-counter)
             (#x1c reg:digital-diagnostic-reserved)
             (#x24 reg:test-mode-ctrl)
             (#x28 reg:sts-quality-error-counter)
             (#x2a reg:low-voltage-warning-counter)
             (#x2c reg:spi-mode)
             (#x30 reg:system-state)
             (#x3c reg:fast-command-status)
             (#x48 reg:current-sts-iv-lsb32)
             (#x4c reg:spi-crc-lfsr-init-code)))))

;; 0x11 PMSC
;; Power Management System Control Block

(define-public reg:soft-reset-ctrl
  (register
   (name 'soft-reset-ctrl)
   (address #x00)
   (width (octets 2))
   (items (list (‣ soft-arm-reset  0 1)
                (‣ soft-prgn-reset 1 1)
                (‣ soft-cia-reset  2 1)
                (‣ soft-bist-reset 3 1)
                (‣ soft-rx-reset   4 1)
                (‣ soft-tx-reset   5 1)
                (‣ soft-hif-reset  6 1)
                (‣ soft-pmsc-reset 7 1)
                (‣ soft-gpio-reset 8 1)
                (‣ reserved        9 7)))))

(define-public reg:pmsc-clock-ctrl
  (register
   (name 'pmsc-clock-ctrl)
   (address #x04)
   (width (octets 4))
   (items
    (list
     (‣ sys-clock-ctrl              0 2)
     (‣ rx-clock-ctrl               2 2)
     (‣ tx-clock-ctrl               4 2)
     (‣ acc-clock-enable            6 1)
     (‣ reserved                    7 1)
     (‣ cia-clock-enable            8 1)
     (‣ reserved                    9 1 (default #t))
     (‣ sar-clock-enable           10 1)
     (‣ reserved                   11 4)
     (‣ acc-memory-clock-enable    15 1)
     (‣ gpio-clock-enable          16 1)
     (‣ reserved                   17 1)
     (‣ gpio-debounce-clock-enable 18 1)
     (‣ gpio-debounce-reset        19 1 (semantics boolean/active-low) (default #f))
     (‣ reserved                   20 3 (default 3))
     (‣ kilohertz-clock-enable     23 1)
     (‣ reserved                   24 8 (default 240))))))

(define-public reg:pmsc-sequencing-ctrl
  (register
   (name 'pmsc-sequencing-ctrl)
   (address #x08)
   (width (octets 4))
   (items (list (‣ reserved                  0 8 (default 56))
                (‣ auto-idle-rc-to-pll       8 1)
                (‣ reserved                  9 2 (default 3))
                (‣ auto-sleep-after-tx      11 1)
                (‣ auto-sleep-after-rx      12 1)
                (‣ reserved                 13 2)
                (‣ external-sync-pll-enable 15 1)
                (‣ reserved                 16 1)
                (‣ cia-run-enable           17 1)
                (‣ reserved                 18 5)
                (‣ force-idle-rc-state      23 1)
                (‣ reserved                 24 2)
                (‣ kilohertz-clock-divider  26 6)))))

(define-public reg:pmsc-fine-grain-tx-sequencing-ctrl
  (register
   (name 'pmsc-fine-grain-tx-sequencing-ctrl)
   (address #x12)
   (width (octets 4))
   ;; 0x0d20874 → Disable this mode, for things like continuous wave modes.
   (items (list (‣ tx-fine-pwr-sequencing 0 32 (default #x4d28874))))))

(define-public reg:led-ctrl
  (register
   (name 'led-ctrl)
   (address #x16)
   (width (octets 4))
   (items (list (‣ led-blink-time     0  8 (default 32))
                (‣ led-blink-enable   8  1)
                (‣ reserved           9  7)
                (‣ led-0-force-blink 16  1)
                (‣ led-1-force-blink 17  1)
                (‣ led-2-force-blink 18  1)
                (‣ led-3-force-blink 19  1)
                (‣ reserved          20 12)))))

(define-public reg:sniff-mode-ctrl
  (register
   (name 'sniff-mode-ctrl)
   (address #x1a)
   (width (octets 4))
   (items (list (‣ sniff-mode-on-time   0  4)
                (‣ reserved             4  4)
                (‣ sniff-mode-off-time  8  8)
                (‣ reserved            16 16)))))

(define-public reg:bias-ctrl
  (register
   (name 'bias-ctrl)
   (address #x1f)
   (width (octets 2))
   (items (list (‣ bias-control 0 16)))))

(define-public page:power-management-ctrl-block
  (register-map
   (name 'power-management-ctrl-block)
   (address #x11)
   (description "Power Management System Control Block")
   (table (↔ (#x00 reg:soft-reset-ctrl)
             (#x04 reg:pmsc-clock-ctrl)
             (#x08 reg:pmsc-sequencing-ctrl)
             (#x12 reg:pmsc-fine-grain-tx-sequencing-ctrl)
             (#x16 reg:led-ctrl)
             (#x1a reg:sniff-mode-ctrl)
             (#x1f reg:bias-ctrl)))))

;; 0x12 RX_BUFFER_0
;; Receive Data Buffer #0

(define-public reg:rx-buffer-0
  (register
   (name 'rx-buffer-0)
   (address #x00)
   (width (octets 1024))
   (items (list (‣ rx-buffer-0 0 (octets 1024))))))

(define-public page:rx-buffer-0
  (register-map
   (name 'rx-buffer-0)
   (address #x12)
   (description "Receive Data Buffer #0")
   (table (↔ (#x00 reg:rx-buffer-0)))))

;; 0x13 RX_BUFFER_1
;; Receive Data Buffer #1

(define-public reg:rx-buffer-1
  (register
   (name 'rx-buffer-1)
   (address #x00)
   (width (octets 1024))
   (items (list (‣ rx-buffer-1 0 (octets 1024))))))

(define-public page:rx-buffer-1
  (register-map
   (name 'rx-buffer-1)
   (address #x13)
   (description "Receive Data Buffer #1")
   (table (↔ (#x00 reg:rx-buffer-1)))))

;; 0x14 TX_BUFFER
;; Transmit Data Buffer

(define-public reg:tx-buffer
  (register
   (name 'tx-buffer)
   (address #x00)
   (width (octets 1024))
   (items (list (‣ tx-buffer 0 (octets 1024))))))

(define-public page:tx-buffer
  (register-map
   (name 'rx-buffer)
   (address #x12)
   (description "Transmit Data Buffer")
   (table (↔ (#x00 reg:tx-buffer)))))

;; 0x15 ACC_MEM
;; Accumulator CIR Memory

(define-public reg:accumulator-cir-memory
  (register
   (name 'accumulator-cir-memory)
   (address #x00)
   (width (octets 12288))
   (items (list (‣ accumulator-memory 0 (octets 12288))))))

(define-public page:accumulator-cir-memory
  (register-map
   (name 'accumulator-cir-memory)
   (address #x15)
   (description "Accumulator CIR Memory")
   (table (↔ (#x00 reg:accumulator-cir-memory)))))

;; 0x16 SCRATCH_RAM
;; Scratch RAM

(define reg:scratch-ram
  (register
   (name 'scratch-ram)
   (address #x00)
   (width (octets 127))
   (items (list (‣ scratch-ram 0 (octets 127))))))

(define-public page:scratch-ram
  (register-map
   (name 'scratch-ram)
   (address #x16)
   (description "Scratch RAM")
   (table (↔ (#x00 reg:scratch-ram)))))

;; 0x17 AES_RAM
;; AES Key RAM

(define reg:aes-ram
  (register
   (name 'aes-ram)
   (address #x00)
   (width (octets 75))
   (items (list (‣ aes-key-ram 0 (octets 75))))))

(define-public page:aes-ram
  (register-map
   (name 'aes-ram)
   (address #x17)
   (description "AES Key RAM")
   (table (↔ (#x00 reg:aes-ram)))))

;; 0x18 SET_1, SET_2
;; Double Buffer Diagnostic Sets

(define reg:double-buffer-diagnostic
  (register
   (name 'double-buffer-diagnostic)
   (address #x00)
   (width (octets 464))
   (items (list (‣ double-buffer-diagnostic 0 (octets 464))))))

(define-public page:double-buffer-diagnostic
  (register-map
   (name 'double-buffer-diagnostic)
   (address #x18)
   (description "Double Buffer Diagnostic Register Set")
   (table (↔ (#x00 reg:double-buffer-diagnostic)))))

;; 0x1d INDIRECT_PTR_A
;; Indirect Pointer A Buffer

(define reg:indirect-pointer-a
  (register
   (name 'indirect-pointer-a)
   (address #x00)
   (width (octets 4))
   (items (list (‣ indirect-pointer-a 0 (octets 4))))))

(define-public page:indirect-pointer-a
  (register-map
   (name 'indirect-pointer-a)
   (address #x1d)
   (description "Indirect Pointer A")
   (table (↔ (#x00 reg:indirect-pointer-a)))))

;; 0x1e INDIRECT_PTR_B
;; Indirect Pointer B Buffer

(define reg:indirect-pointer-b
  (register
   (name 'indirect-pointer-b)
   (address #x00)
   (width (octets 4))
   (items (list (‣ indirect-pointer-b 0 (octets 4))))))

(define-public page:indirect-pointer-b
  (register-map
   (name 'indirect-pointer-b)
   (address #x1e)
   (description "Indirect Pointer B")
   (table (↔ (#x00 reg:indirect-pointer-b)))))

;; 0x1f IN_PTR_CFG
;; Indirect Pointer Access Configuration, and “Fast” Status Register

(define-public reg:fast-system-event-status
  (register
   (name 'fast-system-event-status)
   (address #x00)
   (width (octets 1))
   (items (list (‣ tx-ok-status       0 1)
                (‣ cca-fail-status    1 1)
                (‣ rxter-error-status 2 1)
                (‣ rx-ok-status       3 1)
                (‣ rx-error-status    4 1)
                (‣ rx-timeout-status  5 1)
                (‣ sys-event-status   6 1)
                (‣ sys-panic-status   7 1)))))

(define-public reg:pointer-a-base-address
  (register
   (name 'pointer-a-base-address)
   (address #x04)
   (width (octets 1))
   (items (list (‣ pointer-a-base-addr 0 5)
                (‣ reserved            5 3)))))

(define-public reg:pointer-a-offset
  (register
   (name 'pointer-a-offset)
   (address #x08)
   (width (octets 2))
   (items (list (‣ pointer-a-offset  0 15)
                (‣ reserved         15  1)))))

(define-public reg:pointer-b-base-address
  (register
   (name 'pointer-b-base-address)
   (address #x0c)
   (width (octets 1))
   (items (list (‣ pointer-b-base-addr 0 5)
                (‣ reserved            5 3)))))

(define-public reg:pointer-b-offset
  (register
   (name 'pointer-b-offset)
   (address #x10)
   (width (octets 2))
   (items (list (‣ pointer-b-offset  0 15)
                (‣ reserved         15  1)))))

(define-public page:indirect-pointer-cfg&fast-status
  (register-map
   (name 'indirect-pointer-cfg&fast-status)
   (address #x16)
   (description
    "Indirect Pointer Access Configuration, and Fast Status Register")
   (table (↔ (#x00 reg:fast-system-event-status)
             (#x04 reg:pointer-a-base-address)
             (#x08 reg:pointer-a-offset)
             (#x0c reg:pointer-b-base-address)
             (#x10 reg:pointer-b-offset)))))
