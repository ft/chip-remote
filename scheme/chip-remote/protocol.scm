;; Copyright (c) 2011-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote protocol)
  #:use-module (srfi srfi-1)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register common)
  #:use-module (chip-remote semantics)
  #:export (register-semantics-version))

;; The new  interaction protocol with the  chip-remote firmware is going  to be
;; based on  a register table. The  memory holding this register  table will be
;; transferred between  the firmware and  the host using ufw's  register proto-
;; col. Since chip-remote is a system that was made to work with chips that are
;; controlled via register tables, it has  many utilities to work with register
;; tables, which makes using this kind of protocol a neat fit.
;;
;; The chip-remote  protocol is therefore  an access scheme upon  this register
;; table. This scheme support service discovery by an index of peripheraly made
;; accessible by the firware. The address of  this index is encoded in the sta-
;; tic part of the register table.
;;
;; The index  itself consists of  a length  register (u16) followed  by exactly
;; that many  u32 registers that encode  the address of the  peripheral control
;; block.
;;
;; The format of  peripheral control blocks are dependant on  the type of peri-
;; pheral. But they all start with a u16 register that encodes the type of con-
;; trol block.
;;
;; This scheme allows a remote client  to dynamically work out the complete re-
;; gister table of the remote firmware,  offering the user a coherent interface
;; without prior knowledge.
;;
;; The register table is implemented using ufw's register-table implementation,
;; configured to use big-endian octet ordering.

;; This is the version of the semantics of the register-table. It is effective-
;; ly the version of the new chip-remote protocol.
(define register-semantics-version 0)

;; This is the  static section of the remote register-table.  It is mostly con-
;; cerned with  encoding versions (the  register table semantics,  the firmware
;; version, and the versions of the  zephyr operating system and the ufw libra-
;; ry). Crucially  important however  is the  interface-index-address register.
;; Its value specifies  the entry-point inside of the remote  memory to perform
;; the service discovery process.
(define-register-map crfw-static
  #:table*
  ;; The semantics version must match between the remote and local sides.
  (#x0000 (generate-u16-register register-semantics-version))
  ;; This is the important register to start service discovery.
  (#x0001 (generate-u32-register interface-index-address))
  ;; The rest is just versions of the remote firmware an some of its most
  ;; important components.
  (#x0010 (generate-u16-register firmware-version-major))
  (#x0011 (generate-u16-register firmware-version-minor))
  (#x0012 (generate-u16-register firmware-version-patch))
  (#x0013 (generate-u16-register firmware-git-increment))
  (#x0014 (generate-register #:contents
                             (git-candidate-level   0 8)
                             (git-pre-release-level 8 8)))
  (#x0015 (generate-register #:contents
                             (with-git-version?   0 1)
                             (git-dirty?          1 1)
                             (git-is-candidata?   2 1)
                             (git-is-pre-release? 3 1)
                             (git-clean-release?  4 1)))
  (#x0016 (generate-u16-register zephyr-version-major))
  (#x0017 (generate-u16-register zephyr-version-minor))
  (#x0018 (generate-u16-register zephyr-version-patch))
  (#x0019 (generate-u16-register ufw-version-major))
  (#x001a (generate-u16-register ufw-version-minor))
  (#x001b (generate-u16-register ufw-version-patch)))

;; This is are the registers types needed to build the interface index.
(define-u16-register interface-index-size)
(define-u32-register interface-index-address)

(define-semantics interface-type lookup '((spi . 0)
                                          (i2c . 1)))

;; This is the interface control block for SPI peripherals.
(define-register-map ifc:spi
  #:table*
  (#x0000 (generate-register #:contents (type 0 16 #:semantics* interface-type)))
  (#x0001 (generate-u16-register frame-length))
  (#x0002 (generate-u32-register clock-rate))
  (#x0004 (generate-register #:contents
                             (clock-phase-delay?   0 1)
                             (bit-order-msb-first? 1 1)
                             (cs-active-low?       2 1)
                             (clock-rising-edge?   3 1)))
  (#x0005 (generate-u16-register frame-buffer-size))
  (#x0006 (generate-u32-register frame-buffer-address))
  (#x0008 (generate-u16-register spi-command))
  (#x0009 (generate-u32-register spi-command-argument))
  (#x000b (generate-u32-register spi-command-status)))

(define (make-frame-buffer-register n)
  (generate-register #:contents (frame-buffer 0 (* 16 n))))
