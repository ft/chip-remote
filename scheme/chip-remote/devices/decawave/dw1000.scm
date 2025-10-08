;; Copyright (c) 2018-2025 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices decawave dw1000)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (chip-remote bit-operations)
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

(define-public (make-header write? regaddr subaddr)
  (when (> subaddr #x7fff)
    (throw 'value-out-of-range subaddr))
  (let* ((with-subaddr? (not (zero? subaddr)))
         (ctrl (chain-modify* frame-ctrl
                              `(write-op? ,write?)
                              `(sub-address? ,with-subaddr?)
                              `(register-address ,regaddr))))
    (if (not with-subaddr?)
        (list ctrl)
        (let* ((extended? (> subaddr #x7f))
               (sub (chain-modify* frame-sub-address
                                   `(extended-address? ,extended?)
                                   `(sub-address ,(bit-extract-width subaddr
                                                                     0 7)))))
          (if (not extended?)
              (list ctrl sub)
              (list ctrl sub (bit-extract-width subaddr 7 8)))))))

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
         (width (/ (register-width reg) 8))
         (header (make-header #f (car addr) (cadr addr))))
    (values (append header (make-list (min width 60) 0))
            (length header))))

(define (read-parse dev addr data meta)
  (let* ((bytes (drop data meta))
         (size (length bytes)))
    (bytevector-uint-ref (u8-list->bytevector bytes) 0 'little size)))

(define (write-to-spi dev addr value)
  (let* ((reg (apply device-ref dev (take addr 2)))
         (width (/ (register-width reg) 8))
         (bv (make-bytevector width))
         (header (make-header #t (car addr) (cadr addr))))
    (bytevector-uint-set! bv 0 value 'little width)
    (values (append header (bytevector->u8-list bv))
            (length header))))

(define (write-parse dev addr value data meta)
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
  (page-map (pm→ (table (↔ (#x00 page:device-id)
                           (#x01 page:ieee-euid)
                           (#x03 page:pan-id/short-address)
                           (#x04 page:system-cfg)
                           (#x06 page:system-time)
                           (#x08 page:tx-frame-ctrl)
                           (#x09 page:tx-buffer)
                           (#x0a page:delayed-tx/rx-time)
                           (#x0c page:rx-frame-wait-timeout)
                           (#x0d page:system-ctrl)
                           (#x0e page:system-event-mask)
                           (#x0f page:system-status)
                           (#x10 page:rx-frame-info)
                           (#x11 page:rx-buffer)
                           (#x12 page:rx-frame-quality-info)
                           (#x13 page:rx-time-track-interval)
                           (#x14 page:rx-time-track-offset)
                           (#x15 page:rx-time-of-arrival)
                           (#x17 page:tx-time-of-sending)
                           (#x18 page:tx-antenna-delay)
                           (#x19 page:system-state)
                           (#x1a page:ack-time/response-time)
                           (#x1d page:rx-sniff-mode-cfg)
                           (#x1e page:tx-power-ctrl)
                           (#x1f page:channel-ctrl)
                           (#x21 page:user-sfd-sequences)
                           (#x23 page:agc-ctrl)
                           (#x24 page:external-sync-ctrl)
                           (#x25 page:accumulator-memory)
                           (#x26 page:gpio-ctrl)
                           (#x27 page:digital-rx-cfg)
                           (#x28 page:analog-rx-cfg)
                           (#x2a page:tx-calibration)
                           (#x2b page:frequency-synthesizer-ctrl)
                           (#x2c page:always-on-ctrl)
                           (#x2d page:otp-interface)
                           (#x2e page:leading-edge-detect-ctrl)
                           (#x2f page:digital-diagnostics)
                           (#x36 page:power-management-ctrl))))))
