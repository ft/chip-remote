;; Copyright (c) 2018-2024 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

;; Device access abstraction. Reading and writing registers to devices is a job
;; that requires access to the peripheral bus a chip is connected to. In Chip
;; remote, this access is implemented by its firmware component, which exposes
;; these peripheral busses via a register table interface. The protocol used to
;; interact with this register table is ufw's register protocol, as implemented
;; in (protocol ufw-regp). This protocol is minimal memory transfer protocol,
;; that can be used in many channels like UART, USB, and TCP. Chip remote's
;; custom register semantics on top of this low-level protocol is implemented
;; in (chip-remote protocol).
;;
;; The (chip-remote protocol) module exposes primitives to perform transactions
;; in the peripheral busses it supports. This module implements a data-type and
;; associated functions that allow chip specifications to implement register
;; read and write accesses in a uniform way, that can be used by very high
;; level modules such as (chip-remote commander) to perform these accesses as
;; it needs.
;;
;; The low level access function in (chip-remote protocol) need a connection
;; object, and a symbol identifying the peripheral bus that the chip to talk to
;; is connected to.
;;
;; This governs the API for the read, write, and setup function types:
;;
;;   - `(read  CONNECTION INTERFACE PAGE-ADDRESS REGISTER-ADDRESS)`
;;   - `(write CONNECTION INTERFACE PAGE-ADDRESS REGISTER-ADDRESS VALUE)`
;;   - `(setup CONNECTION INTERFACE)`
;;
;; The ‘data’ slot in `<device-access>` can be used freely by implementations
;; to store any sort of additional data they need.


(define-module (chip-remote device access)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote protocol)
  #:export (make-device-access
            device-access?
            device-access-data
            device-setup
            device-read
            device-write))

(define-immutable-record-type <device-access>
  (make-device-access* data setup read write)
  device-access?
  (data   device-access-data)
  (setup  device-setup)
  (read   device-read)
  (write  device-write))

(define (default-setup c ifc)
  (throw 'not-implemented 'device-access-setup))

(define (default-read c ifc p r)
  (throw 'not-implemented 'device-access-read))

(define (default-write c ifc p r v)
  (throw 'not-implemented 'device-access-write))

(define* (make-device-access #:key
                             (data #f)
                             (setup default-setup)
                             (read default-read)
                             (write default-write))
  (make-device-access* data setup read write))
