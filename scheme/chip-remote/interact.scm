;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote interact)
  #:use-module (chip-remote item)
  #:use-module (chip-remote modify)
  #:use-module (chip-remote register)
  #:export (control-data-frame:decode
            control-data-frame:encode))

;; When talking to a chip, you're using an interface that the chip implements.
;; Using that interface, chip interactions (reading and writing registers) are
;; implemented. Depending on that interface the protocol for interactions can
;; vary dramatically, since some interfaces are very free-form (like SPI) while
;; others have some notion of the protocol baked in (like I²C, which encodes
;; read and write access in its packets).
;;
;; The exact protocol that a device implements always depends, in large part,
;; on the specifics of that device. Users should not have to care too much
;; about these specifics, however. That's where this module comes into play.
;;
;; Its aim is to implement common access patterns used by chips in the wild so
;; that a device description can use them when specifying the transmission
;; scheme that the device employs.

;; One common approach is to use a command structure, where a series of bits
;; determines the details of the access in question. This might implement
;; distinction between read/write access and also control the register address
;; intended for the access.
;;
;; For example, the cdce72010 clock distribution chip uses this:
;;
;;    vutsrqponmlkjihgfedcba9876543210   ← bit address
;;    DDDDDDDDDDDDDDDDDDDDDDDDDDDDcccc   ← bit semantic
;;
;; where the Ds are are the data bits of a register and the cs bits control the
;; device access. With this chip, the address range is [0000]…[1100] and for
;; write access, the chip just uses the address as its command word [c]. Values
;; greater than [1100] are used to implement further access. For example for
;; read access, [c] is [1110] and [D] is [0…0aaaa], where [a] is the address of
;; the register to read. Other access schemes the chip implements is transfer-
;; ring its register table to its on-chip-eeprom and locking down that eeprom
;; for further write access using the [1111] command word with [0…001] and
;; [0…011] as its data word respectively.

;; The two most important access schemes that chip-remote cares about for basi-
;; cally all the chips it supports is reading and writing registers. Special
;; accesses like the eeprom interactions of the cdce72010 are out of scope for
;; this module. Chip support modules may implement them, if they so choose.

;; So an interaction implementation therefore has to take a register address, a
;; method and possibly a value (for the write method) and turn that into a
;; bit-string that can be transferred to the chip in question using RCCEP's
;; TRANSMIT request.

;; In order to specify the format of a serial frame, we'll reuse the register
;; facility.

(define (get-with-spec spec field frame)
  ((item-get (register-ref spec field)) frame))

(define (cons-with-spec spec field frame)
  (cons field (get-with-spec spec field frame)))

(define (control-data-frame:encode spec control data)
  (chain-modify* spec `(control ,control) `(data ,data)))

(define (control-data-frame:decode spec frame)
  (let ((get (lambda (e) (cons-with-spec spec e frame))))
    (list (get 'control)
          (get 'data))))
