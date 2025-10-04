;; Copyright (c) 2018-2025 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices decawave dw1000)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (chip-remote device)
  #:use-module (chip-remote device access)
  #:use-module (chip-remote item)
  #:use-module (chip-remote modify)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register common)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote manufacturer decawave)
  #:use-module (chip-remote utilities)
  #:use-module (chip-remote devices decawave dw1000 registers)
  #:export (dw1000))

;; Frame definitions from:
;; DW1000 User Manual, Version 2.18, pages 4ff.
;; §2.2.1.2 — Transaction Formats of the SPI Interface

;; frame-ctrl is there every time. If sub-address? is set, frame-sub-address
;; follows. In that, if extended-address? is set, extended-address follows.
;; After that, payload octets follow. Chip select has to be asserted before
;; frame-ctrl, and has to remain so until after the last octet of the payload
;; is transmitted.

(define-register frame-ctrl
  (items (list (‣ write-op?        7 1)
               (‣ sub-address?     6 1)
               (‣ register-address 0 6))))

(define-register frame-sub-address
  (items (list (‣ extended-address? 7 1)
               (‣ sub-address       0 7))))

(define-u8-register extended-address)

(define (setup-spi dev)
  ;; We don't need any information about the device to determine a useable SPI
  ;; configuration. 4Mbit/s is not at the limit of the device, but should be
  ;; plenty fast for most experimentation.
  '((frame-length             8)
    (clock-rate           #e4e6)
    (cs-active-low?          #t)
    (clock-idle-low?         #t)
    (clock-phase-delay?      #f)
    (bit-order-msb-first?    #t)))

;; The read/write pair here uses full register writes. For most registers this
;; is fine. But some of them are really large, and the frame buffer in the
;; firmware are not terribly large.
;;
;; The read-parse function takes all non-header response octets and puts them
;; into a large integer in little endian order. The write-parse function does
;; nothing at the moment. All response octets in those are to be ignored so
;; there is nothing to do.
(define (read-from-spi dev addr)
  (let* ((reg (apply device-ref dev (take addr 2)))
         (width (/ (register-width reg) 8)))
    (cons* (chain-modify* frame-ctrl
                          '(write-op? #f)
                          '(sub-address? #f)
                          `(register-address ,(cadr addr)))
           (make-list (min width 60) 0))))

(define (read-parse dev addr data)
  (let* ((bytes (cdr data))
         (size (length bytes)))
    (bytevector-uint-ref (u8-list->bytevector bytes) 0 'little size)))

(define (write-to-spi dev addr value)
  (let* ((reg (apply device-ref dev (take addr 2)))
         (width (/ (register-width reg) 8))
         (bv (make-bytevector width)))
    (bytevector-uint-set! bv 0 value 'little width)
    (cons* (chain-modify* frame-ctrl
                          '(write-op? #t)
                          '(sub-address? #f)
                          `(register-address ,(cadr addr)))
           (bytevector->u8-list bv))))

(define (write-parse dev addr value data)
  #t)

(define-device dw1000
  (manufacturer decawave)
  (homepage "https://www.decawave.com/products/dw1000")
  (datasheet "https://www.decawave.com/sites/default/files/DW1000-Datasheet-V2.15.pdf")
  (keywords '(uwb tranceiver ieee802.15.4-2011 positioning location))
  (access (device-operations
           (setup       setup-spi)
           (read        read-from-spi)
           (read-parse  read-parse)
           (write       write-to-spi)
           (write-parse write-parse)))
  (page-map
   (pm→
    (table
     (↔ (#f (rm→ (table (↔ (#x00 reg:device-id)
                           (#x01 reg:ieee-eui)
                           (#x03 reg:pan-id/short-address)
                           (#x04 reg:system-cfg)
                           (#x06 reg:system-time)
                           (#x08 reg:tx-frame-ctrl)
                           (#x09 reg:tx-buffer)
                           (#x0a reg:delayed-tx/rx-time)
                           (#x0c reg:rx-frame-wait-timeout)
                           (#x0d reg:system-ctrl)
                           (#x0e reg:system-event-mask)
                           (#x0f reg:system-status)
                           (#x10 reg:rx-frame-info)
                           (#x11 reg:rx-buffer)
                           (#x12 reg:rx-frame-quality-info)
                           (#x13 reg:rx-time-track-interval)
                           (#x14 reg:rx-time-track-offset)
                           (#x15 reg:rx-time-of-arrival)
                           (#x17 reg:tx-time-of-sending)
                           (#x18 reg:tx-antenna-delay)
                           (#x19 reg:system-state)
                           (#x1a reg:ack-time/response-time)
                           (#x1d reg:rx-sniff-mode-cfg)
                           (#x1e reg:tx-power-ctrl)
                           (#x1f reg:channel-ctrl)
                           (#x21 reg:user-sfd-sequences)
                           (#x23 reg:agc-ctrl)
                           (#x24 reg:external-sync-ctrl)
                           (#x25 reg:accumulator-memory)
                           (#x26 reg:gpio-ctrl)
                           (#x27 reg:digital-rx-cfg)
                           (#x28 reg:analog-rx-cfg)
                           (#x2a reg:tx-calibration-cfg)
                           (#x2b reg:frequency-synthesizer-ctrl)
                           (#x2c reg:always-on-ctrl)
                           (#x2d reg:otp-interface)
                           (#x2e reg:leading-edge-detect-ctrl)
                           (#x2f reg:digital-diagnostics)
                           (#x36 reg:power-management-ctrl))))))))))
