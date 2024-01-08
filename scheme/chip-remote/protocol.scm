;; Copyright (c) 2011-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote protocol)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (chip-remote decode)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register common)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:use-module (protocol ufw-regp)
  #:export (make-cr-connection
            cr-connection?
            cr-static-info
            cr-index
            cr-interfaces
            cr-access
            proto-engage!
            proto-get-ifc-ctrl!
            proto-interfaces))

(define (pp obj) (pretty-print obj #:width 80 #:max-expr-width 100))

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

(define-register interface-type-register
  #:contents (type 0 16 #:semantics* interface-type))

(define (make-index-table size)
  (make-register-map #:table
                     `((0 . ,interface-index-size)
                       . ,(unfold-right (lambda (n) (< n 1))
                                        (lambda (n)
                                          (cons n interface-index-address))
                                        (lambda (n) (- n 2))
                                        (1+ (* 2 (1- size)))))))

;; This is the interface control block for SPI peripherals.
(define-register-map ifc:spi
  #:table*
  (#x0000 interface-type-register)
  (#x0001 (generate-u16-register frame-length))
  (#x0002 (generate-u32-register clock-rate))
  (#x0004 (generate-register #:contents
                             (cs-active-low?       0 1)
                             (bit-order-msb-first? 1 1)
                             (clock-idle-low?      2 1)
                             (clock-phase-delay?   3 1)))
  (#x0005 (generate-u16-register frame-buffer-size))
  (#x0006 (generate-u32-register frame-buffer-address))
  (#x0008 (generate-u16-register spi-command))
  (#x0009 (generate-u32-register spi-command-argument))
  (#x000b (generate-u32-register spi-command-status)))

(define (interface-size rm)
  (let* ((table (register-map-table:sorted rm))
         (start (car (first table)))
         (finish (last table))
         (end (+ (car finish)
                 (proto-register-width (cdr finish) 16))))
    (- end start)))

(define (make-frame-buffer-register n)
  (generate-register #:contents (frame-buffer 0 (* 16 n))))

;; Communication

(define-record-type <chip-remote-connection>
  (make-cr-connection* llcon static index interfaces access)
  cr-connection?
  (llcon      cr-low-level   set-cr-low-level!)
  (static     cr-static-info set-cr-static-info!)
  (index      cr-index       set-cr-index!)
  (interfaces cr-interfaces  set-cr-interfaces!)
  (access     cr-access      set-cr-access!))

(define (make-cr-connection ll)
  (make-cr-connection* ll #f #f #f #f))

(define (octet-address a)
  (* 2 a))

(define (proto-register-width register addressing)
  (let* ((w* (register-width register))
         (w (if (< w* 16) 16 w*)))
    (ash 1 (inexact->exact (ceiling (log2 (ceiling (/ w addressing))))))))

(define (proto-ref bv offset reg)
  (bytevector-uint-ref bv offset 'big (proto-register-width reg 8)))

(define (register-decode bv register)
  (let ((raw (proto-ref bv 0 register)))
    (cdr (decode register raw))))

(define (block-decode rm bv)
  (register-map-fold
   (lambda (address register acc)
     (let* ((a (octet-address address))
            (raw (proto-ref bv a register))
            (info (cdr (decode register raw))))
       (append acc info)))
   '()
   rm))

(define (proto-read-static! c)
  (let* ((table (register-map-table crfw-static))
         (semreg (first table))
         (end (last table))
         (size (+ (car end) (proto-register-width (cdr end) 16)))
         (response (regp:read-request! (cr-low-level c) (car semreg) size)))
    (unless (regp:valid-ack? response)
      (throw 'protocol-error response))
    (let ((payload (assq-ref response 'payload)))
      (set-cr-static-info! c (block-decode crfw-static payload)))))

(define (proto-read-index! c)
  (let* ((info (cr-static-info c))
         (address (assq-ref info 'interface-index-address))
         (rsize (proto-register-width interface-index-size 16))
         (response (regp:read-request! (cr-low-level c) address rsize)))

    (unless (regp:valid-ack? response)
      (throw 'protocol-error response))

    (let ((isize (cdar (register-decode (assq-ref response 'payload)
                                        interface-index-size))))
      (if (zero? isize)
          (set-cr-index! c '((interface-index-size . 0)))
          (let* ((bsize (+ rsize (* isize 2)))
                 (response (regp:read-request! (cr-low-level c) address bsize)))

            (unless (regp:valid-ack? response)
              (throw 'protocol-error response))

            (let ((table (make-index-table isize))
                  (payload (assq-ref response 'payload)))
              (set-cr-index! c (block-decode table payload ))))))))

(define (proto-read-interfaces! c)
  (let ((info (cr-index c)))
    (if (zero? (assq-ref info 'interface-index-size))
        (set-cr-interfaces! c '())
        (let ((idx (map cdr (filter (lambda (e)
                                      (eq? (car e) 'interface-index-address))
                                    info)))
              (size (proto-register-width interface-type-register 16)))
          (set-cr-interfaces!
           c
           (map (lambda (address)
                  (let ((response (regp:read-request! (cr-low-level c)
                                                      address size)))
                    (unless (regp:valid-ack? response)
                      (throw 'protocol-error response))
                    (cons address
                          (cdar (register-decode (assq-ref response 'payload)
                                                 interface-type-register)))))
                idx))))))

(define (proto-generate-access! c)
  (set-cr-access!
   c
   (let loop ((rest (cr-interfaces c)) (spi 0))
     (if (null? rest)
         '()
         (let ((address (caar rest))
               (type (cdar rest)))
           (cons (case type
                   ((spi) (list (symbol-append 'spi- (number->symbol spi))
                                address
                                (interface-size ifc:spi)
                                ifc:spi))
                   (else (throw 'unknown-interface-type type)))
                 (loop (cdr rest) (if (eq? type 'spi) (1+ spi) spi))))))))

(define (proto-engage! c)
  (let* ((table (register-map-table crfw-static))
         (semreg (first table))
         (response (regp:read-request! (cr-low-level c) (car semreg)
                                       (proto-register-width (cdr semreg) 16))))

    (unless (regp:valid-ack? response)
      (throw 'protocol-error response))

    (let ((sem (register-decode (assq-ref response 'payload) (cdr semreg))))
      (unless (equal? sem (list (cons 'register-semantics-version
                                      register-semantics-version)))
        (throw 'incompatible-register-semantics register-semantics-version sem)))

    (proto-read-static! c)
    (proto-read-index! c)
    (proto-read-interfaces! c)
    (proto-generate-access! c)
    c))

(define (proto-interfaces c)
  (map car (cr-access c)))

(define (proto-get-ifc-ctrl! c key)
  (let* ((info (cr-access c))
         (access (assq-ref info key)))
    (unless access
      (throw 'unknown-interface key info))
    (match access
      ((address size spec)
       (let ((response (regp:read-request! (cr-low-level c) address size)))
         (unless (regp:valid-ack? response)
           (throw 'protocol-error response))
         (block-decode spec (assq-ref response 'payload)))))))
