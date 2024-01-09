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
  #:use-module (chip-remote modify)
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
            proto-interfaces
            cr:setup-spi!
            cr:ctrl-comand!
            cr:load-frame-buffer!))

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
                             (cs-active-low?       0 1 #:default #t)
                             (bit-order-msb-first? 1 1 #:default #t)
                             (clock-idle-low?      2 1 #:default #t)
                             (clock-phase-delay?   3 1 #:default #f)))
  (#x0005 (generate-u16-register frame-buffer-size))
  (#x0006 (generate-u32-register frame-buffer-address))
  (#x0008 (generate-u16-register command))
  (#x0009 (generate-u32-register command-argument))
  (#x000b (generate-u32-register command-status)))

(define (interface-size rm)
  (let* ((table (register-map-table:sorted rm))
         (start (car (first table)))
         (finish (last table))
         (end (+ (car finish)
                 (proto-register-width (cdr finish) 16))))
    (- end start)))

(define (make-spi-config-rm rm)
  (let* ((table (take (drop (register-map-table:sorted rm) 1) 3))
         (start (caar table))
         (new (make-register-map #:table (map (lambda (e)
                                                (cons (- (car e) start)
                                                      (cdr e)))
                                              table))))
    (list start (interface-size new) new)))

(define ctrl-commands '((init     . 0)
                        (transmit . 1)))

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

(define (word-width-to-fit n g)
  "Return word-width in granularity g to fit n."
  (ash 1 (inexact->exact (ceiling (log2 (ceiling (/ n g)))))))

(define (proto-register-width register addressing)
  (let* ((w* (register-width register))
         (w (if (< w* 16) 16 w*)))
    (word-width-to-fit w addressing)))

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

(define (block->regmap rm bv)
  (register-map-fold-right
   (lambda (address register acc)
     (let* ((a (octet-address address))
            (raw (proto-ref bv a register)))
       (cons (cons address raw) acc)))
   '()
   rm))

(define (regmap->block rm value)
  (let* ((size (octet-address (interface-size rm)))
         (bv (make-bytevector size)))
    (register-map-fold
     (lambda (address register acc)
       (let ((offset (octet-address address))
             (n (proto-register-width register 8)))
         (bytevector-uint-set! bv offset (assv-ref value address) 'big n)
         bv))
     bv rm)))

(define (register-put reg value)
  (let* ((size (proto-register-width reg 8))
         (bv (make-bytevector size)))
    (bytevector-uint-set! bv 0 value 'big size)
    bv))

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

(define (proto-get-ifc-access c key)
  (let* ((info (cr-access c))
         (access (assq-ref info key)))
    (unless access
      (throw 'unknown-interface key info))
    access))

(define (proto-get-ifc-ctrl! c key)
  (let ((access (proto-get-ifc-access c key)))
    (match access
      ((address size spec)
       (let ((response (regp:read-request! (cr-low-level c) address size)))
         (unless (regp:valid-ack? response)
           (throw 'protocol-error response))
         (block-decode spec (assq-ref response 'payload)))))))

(define (cr:setup-spi! c spi-n . lst)
  (let ((access (proto-get-ifc-access c spi-n)))
    (match access
      ((address size spec)
       (match (make-spi-config-rm spec)
         ((start size cfg)
          (let ((response (regp:read-request! (cr-low-level c)
                                              (+ start address)
                                              size)))
            (unless (regp:valid-ack? response)
              (throw 'protocol-error response))
            (let* ((current (block->regmap cfg (assq-ref response 'payload)))
                   (next (apply chain-modify (cons* cfg current lst)))
                   (block (regmap->block cfg next))
                   (response (regp:write-request! (cr-low-level c)
                                                  (+ start address)
                                                  block)))
              (unless (regp:valid-ack? response)
                (throw 'protocol-error response))))))))))

(define (ifc:find-register rm name)
  (let* ((address* (register-map-canonical rm name))
         (address (and address* (car address*)))
         (register (and address (register-map-ref rm address))))
    (and register (cons address register))))

(define (proto-write-register! c ifc name value)
  (let* ((access (proto-get-ifc-access c ifc))
         (start (first access))
         (addr+reg (ifc:find-register (third access) name))
         (block (register-put (cdr addr+reg) value))
         (response (regp:write-request! (cr-low-level c)
                                        (+ start (car addr+reg))
                                        block)))
    (unless (regp:valid-ack? response)
      (throw 'protocol-error response))
    response))

(define (proto-read-register! c ifc name)
  (let* ((access (proto-get-ifc-access c ifc))
         (start (first access))
         (addr+reg (ifc:find-register (third access) name))
         (response
          (regp:read-request! (cr-low-level c)
                              (+ start (car addr+reg))
                              (proto-register-width (cdr addr+reg) 16))))
    (unless (regp:valid-ack? response)
      (throw 'protocol-error response))
    (bytevector-uint-ref (assq-ref response 'payload)
                         0 'big (* 2 (assq-ref response 'block-size)))))

(define* (cr:ctrl-comand! c ifc cmd #:optional arg)
  (and arg (proto-write-register! c ifc 'command-argument arg))
  (proto-write-register! c ifc 'command (assq-ref ctrl-commands cmd))
  ;; The protocol implementation guarantees, that when an ack for a command
  ;; register write request comes in, the corresponding status register will
  ;; already be loaded with the correct status value for the command that was
  ;; just processed. Thus, this is not a race.
  (proto-read-register! c ifc 'command-status))

(define (required-fb-size frame-length n)
  (let* ((bytes-per-word (word-width-to-fit frame-length 8))
         (k (* n bytes-per-word)))
    (+ k (modulo k 2))))

(define (cr:load-frame-buffer! c ifc lst)
  (let* ((frame-length (proto-read-register! c ifc 'frame-length))
         (fb-size (proto-read-register! c ifc 'frame-buffer-size))
         (fb-bytes (octet-address fb-size))
         (required-bytes (required-fb-size frame-length (length lst))))
    (unless (>= fb-bytes required-bytes)
      (throw 'frame-buffer-overflow frame-length required-bytes fb-bytes))
    (let ((block (make-bytevector required-bytes))
          (bytes-per-word (word-width-to-fit frame-length 8)))
      (fold (lambda (word offset)
              (bytevector-uint-set! block offset word 'big bytes-per-word)
              (+ offset bytes-per-word))
            0 lst)
      (let* ((address (proto-read-register! c ifc 'frame-buffer-address))
             (response (regp:write-request! (cr-low-level c) address block)))
        (unless (regp:valid-ack? response)
          (throw 'protocol-error response))))))
