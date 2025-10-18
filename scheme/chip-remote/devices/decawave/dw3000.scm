;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices decawave dw3000)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (chip-remote device)
  #:use-module (chip-remote device access)
  #:use-module (chip-remote modify)
  #:use-module (chip-remote item)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote manufacturer decawave)
  #:use-module (chip-remote utilities)
  #:use-module (chip-remote devices decawave dw3000 registers)
  #:export (dw3000))

;; Device Access

;; The DW3000 has a more complex access scheme then its predecessor (the
;; DW1000): It has fast-commands (being able to trigger common actions with
;; minimal overhead) and masked register accesses (applying logical AND and OR
;; masks to a register, which allows for modifications without explicit read-
;; modify-write accesses), in addition to the usual register read and write
;; primitives.
;;
;; Normal access in terms of (chip-remote device access) only requires the
;; usual read and write accesses, and that is what we must implement.

;; Frame definitions from:
;; DW3000 User Manual, Version 1.1, pages 11ff.
;; §2.3.1.2 — Transaction Formats of the SPI Interface

(define-register frame-ctrl
  (items (list (‣ write-op?     7 1)
               (‣ long-address? 6 1)
               (‣ base-address  1 5)
               (‣ fast-command? 0 1))))

(define-register frame-mode
  (items (list (‣ mask-length 0 2)
               (‣ sub-address 2 6))))

(define (make-header write? base-addr sub-addr)
  (when (> base-addr #b11111)
    (throw 'value-out-of-range base-addr))
  (when (> sub-addr #x111111)
    (throw 'value-out-of-range sub-addr))
  (let* ((with-subaddr? (not (zero? sub-addr)))
         (ctrl (chain-modify* frame-ctrl
                              `(write-op? ,write?)
                              `(long-address? ,with-subaddr?)
                              `(base-address ,base-addr)
                              `(fast-command?
                                ;; When a sub-address is given, the
                                ;; fast-command? field is used as the MSB of
                                ;; the subaddress. This gives us a five bit
                                ;; base-address and a seven bit sub-address.
                                ,(and with-subaddr?
                                      (not (zero? (logand #b1000000
                                                          sub-addr))))))))
    (if (not with-subaddr?)
        (list ctrl)
        (list ctrl
              (chain-modify* frame-mode
                             ;; When mask-length is zero, we're creating a
                             ;; primitive read or write action.
                             '(mask-length 0)
                             ;; Again, the MSB of the sub-address is encoded in
                             ;; the fast-command? bit of the control frame, so
                             ;; here we're masking out the lower six bits.
                             `(sub-address ,(logand #b111111 sub-addr)))))))

(define (setup-spi dev)
  ;; Same as DW1000.
  '((frame-length             8)
    (clock-rate           #e4e6)
    (cs-active-low?          #t)
    (clock-idle-low?         #t)
    (clock-phase-delay?      #f)
    (bit-order-msb-first?    #t)))

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
         (bv (make-bytevector (min width 60)))
         (header (make-header #t (car addr) (cadr addr))))
    (bytevector-uint-set! bv 0 value 'little (min width 60))
    (values (append header (bytevector->u8-list bv))
            (length header))))

(define (write-parse dev addr value data meta)
  #t)

;; Full Specification

(define-device dw3000
  (manufacturer decawave)
  (homepage "https://www.qorvo.com/products/p/DW3220")
  (datasheet "https://www.qorvo.com/products/d/da008154")
  (keywords '(uwb tranceiver ieee802.15.4z positioning location))
  (access (device-operations
           (setup       setup-spi)
           (read        read-from-spi)
           (read-parse  read-parse)
           (write       write-to-spi)
           (write-parse write-parse)))
  (page-map
   (pm→
    (table
     (↔ (#x00 page:general-cfg-0)
        (#x01 page:general-cfg-1)
        (#x02 page:sts-cfg)
        (#x03 page:receiver-tuning)
        (#x04 page:external-sync-and-rx-calibration)
        (#x05 page:gpio-ctrl)
        (#x06 page:digital-rx-cfg)
        (#x07 page:analog-rf-cfg)
        (#x08 page:tx-calibration)
        (#x09 page:frequency-synth-ctrl)
        (#x0a page:always-on-system-control)
        (#x0b page:opt-interface)
        (#x0c page:channel-impulse-analyzer-0)
        (#x0d page:channel-impulse-analyzer-1)
        (#x0e page:channel-impulse-analyzer-2)
        (#x0f page:digital-diagnostics)
        (#x11 page:power-management-ctrl-block)
        (#x12 page:rx-buffer-0)
        (#x13 page:rx-buffer-1)
        (#x14 page:tx-buffer)
        (#x15 page:accumulator-cir-memory)
        (#x16 page:indirect-pointer-cfg&fast-status))))))
