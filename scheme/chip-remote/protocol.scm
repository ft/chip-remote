;; Copyright (c) 2011-2024 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; This is the implementation of chip-remote's  high-level protocol on top of a
;; low-level memory transferral protocol. From the  point of view of a chip-re-
;; mote user it is still fairly low-level.  You would normally not use it your-
;; self.  It is a vehicle  to interact  with chips behind  the interface-bridge
;; that is implemented  by the chip-remote firmware.  The high-level modules of
;; the library are the users of this module.
;;
;; The most important procedures in this module are:
;;
;;   - make-cr-connection!
;;       Generates a new chip-remote connection object, that will be used with
;;       all parts pf the API of this module.
;;
;;   - proto-engage!
;;       Performs service discovery with the  remote firmware. This exposes the
;;       peripheral interfaces the remote firmware  supports on its given hard-
;;       ware platform.
;;
;;   - proto-interfaces
;;       Returns a list  of symbols that name the  peripherals available within
;;       the remote firmware.
;;
;;   - cr:spi-transceive!
;;       This performs an SPI transaction with a given list of words to send to
;;       the remote firmware  and returns a list of words  received in the pro-
;;       cess.
;;
;;   - cr:i2c-transceive!
;;       This is similar to cr:spi-transceive!, except that it works for I²C
;;       peripherals. The specifications of I²C messages is a little more com-
;;       plicated than with SPI, and is described with the procedure.
;;
;; The other parts of the API  are mostly interesting for developers. The parts
;; mentioned here might be interesting for users that want to do extremely low-
;; level interactions without being interfered by the higher-level logic.
;;
;; The names of all procedures that  have notable side-effects end in an excla-
;; mation point.  Since this module  does the heavy  lifting of talking  to the
;; outside world, that makes up most of the API.
;;
;; Note that the API names may not be final, yet.

(define-module (chip-remote protocol)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs bytevectors gnu)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (termios)
  #:use-module (termios system)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote decode)
  #:use-module (chip-remote modify)
  #:use-module (chip-remote item)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register common)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:use-module (protocol ufw-regp)
  #:export (make-cr-connection!
            cr-connection?
            cr-static-info
            cr-index
            cr-interfaces
            cr-access
            cr-low-level
            proto-connected?
            proto-engage!
            proto-get-ifc-ctrl!
            proto-interfaces
            cr:setup-spi!
            cr:ctrl-comand!
            cr:load-tx-frame-buffer!
            cr:fetch-rx-frame-buffer!
            cr:load-i2c-message!
            cr:fetch-i2c-rx-sections!
            cr:i2c-transceive!
            cr:spi-transceive!))

(define (pp obj) (pretty-print obj #:width 80 #:max-expr-width 100))

;; The new  interaction protocol with the  chip-remote firmware is going  to be
;; based on  a register table. The  memory holding this register  table will be
;; transferred between  the firmware and  the host using ufw's  register proto-
;; col. Since chip-remote is a system that was made to work with chips that are
;; controlled via register tables, it has  many utilities to work with register
;; tables, which makes using this kind of protocol a neat fit.
;;
;; The chip-remote protocol is therefore  a protocol that specifies accesses on
;; top of this register table. This  scheme support service discovery by an in-
;; dex of peripherals made accessible by  the firmware. The address of this in-
;; dex is encoded in the static part of the register table.
;;
;; The index  itself consists of  a length  register (u16) followed  by exactly
;; that many  u32 registers that encode  the address of the  peripheral control
;; block.
;;
;; The format  of peripheral control blocks  depend on the type  of peripheral.
;; However, they all start with a u16 register that encodes the type of control
;; block.
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

(define (octet-address a)
  (* 2 a))

(define (word-width-to-fit n g)
  "Return word-width in granularity g to fit n."
  (ash 1 (inexact->exact (ceiling (log2 (ceiling (/ n g)))))))

(define (proto-register-width register addressing)
  (let* ((w* (register-width register))
         (w (if (< w* 16) 16 w*)))
    (word-width-to-fit w addressing)))

;; This is the  static section of the remote register-table.  It is mostly con-
;; cerned with  encoding versions (the  register table semantics,  the firmware
;; version, and the versions of the  zephyr operating system and the ufw libra-
;; ry). Crucially  important however  is the  interface-index-address register.
;; Its value specifies  the entry-point inside of the remote  memory to perform
;; the service discovery process.
(define crfw-static
  (rm→ (table
        (↔
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
         (#x0014 († (‣ git-candidate-level   0 8)
                    (‣ git-pre-release-level 8 8)))
         (#x0015 († (‣ with-git-version?     0 1)
                    (‣ git-dirty?            1 1)
                    (‣ git-is-candidata?     2 1)
                    (‣ git-is-pre-release?   3 1)
                    (‣ git-clean-release?    4 1)))
         (#x0016 (generate-u16-register zephyr-version-major))
         (#x0017 (generate-u16-register zephyr-version-minor))
         (#x0018 (generate-u16-register zephyr-version-patch))
         (#x0019 (generate-u16-register ufw-version-major))
         (#x001a (generate-u16-register ufw-version-minor))
         (#x001b (generate-u16-register ufw-version-patch))))))

;; This is are the registers types needed to build the interface index.
(define-u16-register interface-index-size)
(define-u32-register interface-index-address)

(define-semantics interface-type
  (range (table-lookup '((spi . 0)
                         (i2c . 1))))
  (default (const 'spi)))

(define interface-type-register († (‣ type 0 16 (semantics interface-type))))

(define (make-index-table size)
  (rm→ (table `((0 . ,interface-index-size)
                . ,(unfold-right (lambda (n) (< n 1))
                                 (lambda (n)
                                   (cons n interface-index-address))
                                 (lambda (n) (- n 2))
                                 (1+ (* 2 (1- size))))))))

(define control-table-header (list interface-type-register))
(define control-table-footer (list
                              (generate-u16-register tx-frame-buffer-size)
                              (generate-u32-register tx-frame-buffer-address)
                              (generate-u16-register tx-frame-buffer-size)
                              (generate-u32-register tx-frame-buffer-address)
                              (generate-u16-register command)
                              (generate-u32-register command-argument)
                              (generate-u32-register command-status)))

(define (generate-control-table . lst)
  (let loop ((rest (append control-table-header lst control-table-footer))
             (address 0))
    (if (null? rest)
        '()
        (let ((reg (car rest)))
          (cons (cons address reg)
                (loop (cdr rest)
                      (+ address (proto-register-width reg 16))))))))

(define-syntax-rule (define-control-block name exp ...)
  (define name (rm→ (table (generate-control-table exp ...)))))

(define (interface-size rm)
  (let* ((table (register-map-table:sorted rm))
         (start (car (first table)))
         (finish (last table))
         (end (+ (car finish)
                 (proto-register-width (cdr finish) 16))))
    (- end start)))

(define (make-config-rm rm)
  (let* ((table (drop-right (drop (register-map-table:sorted rm)
                                  (length control-table-header))
                            (length control-table-footer)))
         (start (caar table))
         (new (rm→ (table (map (lambda (e)
                                 (cons (- (car e) start)
                                       (cdr e)))
                               table)))))
    (list start (interface-size new) new)))

;; This is the interface control block for SPI peripherals.
(define-control-block ifc:spi
  (generate-u16-register frame-length)
  (generate-u32-register clock-rate)
  († (‣ cs-active-low?       0 1 (default #t))
     (‣ bit-order-msb-first? 1 1 (default #t))
     (‣ clock-idle-low?      2 1 (default #t))
     (‣ clock-phase-delay?   3 1 (default #f))))

;; This is the interface control block for I²C peripherals.
(define-semantics i2c-speed-grade
  (range (table-lookup '((standard  . 0)
                         (fast      . 1)
                         (fast-plus . 2)
                         (high      . 3)
                         (ultra     . 4))))
  (default (const 'standard)))

;; I2C messages are (potentially) made of multiple sections, that can be either
;; read or write accesses. A message can have an arbitrary amount of these.
;;
;; We will encode the message specification in the beginning of the TX frame
;; buffer of the chip-remote firmware interface. Each message section will be
;; encoded in either one or two octets (a: access [1: write, 0: read], l:
;; length [1: 2 octets, 0: 1 octet], x: end marker [1: end of section list, 0:
;; next word belongs to section spec]):
;;
;;    alxSSSSS [SSSSSSSS]
;;    00.SSSSS              read   5 bit length
;;    10.SSSSS              write  5 bit length
;;    01.SSSSS  SSSSSSSS    read  13 bit length
;;    11.SSSSS  SSSSSSSS    write 13 bit length
;;    ??1?????  ????????    end of message spec.
;;
;; Example:
;;
;;     0x4 0xa8:
;;       Message with two sections:
;;         - Read 4 octets
;;         - Write 8 octets
;;      The message spec here is 2 octets and there is one write access
;;      consisting of 8 octets, so the TX framebuffer needs to keep 10
;;      octets in total. The RX framebuffer must be able to fit 4 octets.
;;
;;     0xc0 0x20 0x82 0xe0 0x40:
;;       Message with three sections:
;;         - Write 32 octets
;;         - Read 2 octets
;;         - Write 64 octets
;;       Here the spec is 5 octets long, since there are two extended
;;       size sections. There are two write sections, one consisting of
;;       32 and one consisting of 64 octets. Thus, the TX framebuffer
;;       must be able to fit 5+32+64=101 octets. The RX framebuffer only
;;       has to fit 2 octets.
;;
;; With this, the message structures can point to places in the frame
;; buffers and the complexity of the messages is only limited by the
;; size of the framebuffer. (Plus the firmware implementation, which
;; will limit the maximum number of sections in a message.)

(define-control-block ifc:i2c
  († (‣ speed           0 3 (semantics i2c-speed-grade))
     (‣ address-10-bit? 3 1 (default #f)))
  (generate-u16-register chip-address))


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

(define* (make-cr-connection! #:key from serial tcp (port 1234) baudrate)
  "Make a new chip-remote connection.

If the #:from parameter is used, its value must be a ufw-regp connection as
created by the (protocol ufw-regp) module. Any other parameters are ignore in
this case.

If #:from is not used, one of #:serial #:and #:tcp is considered. When both are
used, the result is unspecified. With #:serial (specifying the serial device
file), #:baudrate can be used optionally with a value from the termios library.
The default is termios-B115200. With #:tcp, the #:port keyword can be used
optionally. Its default is 1234.

Examples:

  (make-cr-connection! #:serial   \"/dev/ttyACM0\"
                       #:baudrate termios-B921600)

  (make-cr-connection! #:tcp  \"192.168.22.1\"
                       #:port 1234)

Use #:from if you need more control."
  (let ((ll (cond (from from)
                  (serial (let ((tty (open-io-file serial))
                                (ts (make-termios-struct)))
                            (cf-make-raw! ts)
                            (cf-set-speed! ts (or baudrate termios-B921600))
                            (tc-set-attr tty ts)
                            (setvbuf tty 'none)
                            (regp:serial-connection tty #:word-size-16? #t)))
                  (tcp (let* ((s (socket PF_INET SOCK_STREAM 0))
                              (host (gethostbyname tcp))
                              (ip (car (hostent:addr-list host))))
                         (connect s AF_INET ip port)
                         (regp:tcp-connection s #:word-size-16? #t)))
                  (else (throw 'not-implemented-yet)))))
    (make-cr-connection* ll #f #f #f #f)))

(define (proto-connected? c)
  (and (cr-connection? c)
       (let ((llcon (cr-low-level c)))
         (regp:connection? llcon)
         (let ((p (regp:port llcon)))
           (and (port? p)
                (not (port-closed? p)))))))

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
  (define (make-access prefix index address ctrl)
    (list (symbol-append prefix (number->symbol index))
          address
          (interface-size ctrl)
          ctrl))
  (define (maybe+1 actual expect idx)
    (if (eq? actual expect) (1+ idx) idx))
  (set-cr-access! c (let loop ((rest (cr-interfaces c)) (spi 0) (i2c 0))
                      (if (null? rest)
                          '()
                          (let ((address (caar rest))
                                (type (cdar rest)))
                            (cons
                             (case type
                               ((spi) (make-access 'spi- spi address ifc:spi))
                               ((i2c) (make-access 'i2c- i2c address ifc:i2c))
                               (else (throw 'unknown-interface-type type)))
                             (loop (cdr rest)
                                   (maybe+1 type 'spi spi)
                                   (maybe+1 type 'i2c i2c))))))))

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
       (match (make-config-rm spec)
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
         (addr+reg (or (ifc:find-register (third access) name)
                       (throw 'unknown-register (third access) name)))
         (response
          (regp:read-request! (cr-low-level c)
                              (+ start (car addr+reg))
                              (proto-register-width (cdr addr+reg) 16))))
    (unless (regp:valid-ack? response)
      (throw 'protocol-error response))
    (bytevector-uint-ref (assq-ref response 'payload)
                         0 'big (* 2 (assq-ref response 'block-size)))))

(define (maybe-read-register! c ifc name)
  (catch 'unknown-register
    (lambda () (proto-read-register! c ifc name))
    (lambda _ #f)))

(define (resolve-command-status code)
  (or (assv-ref '((#x00000000 . success)
                  (#x00000001 . argument-out-of-range)
                  (#x00000002 . invalid-configuraion)
                  (#x00000003 . tx-framebuffer-overflow)
                  (#x00000004 . rx-framebuffer-overflow)
                  (#x00000005 . input-output-error)
                  (#x00000006 . invalid-value)
                  (#x00000007 . internal-error)
                  (#xffffffff . invalid-command))
                code)
      code))

(define* (cr:ctrl-comand! c ifc cmd #:optional arg)
  (and arg (proto-write-register! c ifc 'command-argument arg))
  (proto-write-register! c ifc 'command (assq-ref ctrl-commands cmd))
  ;; The protocol implementation guarantees, that when an ack for a command
  ;; register write request comes in, the corresponding status register will
  ;; already be loaded with the correct status value for the command that was
  ;; just processed. Thus, this is not a race.
  (resolve-command-status (proto-read-register! c ifc 'command-status)))

(define (required-fb-size frame-length n)
  (let* ((bytes-per-word (word-width-to-fit frame-length 8))
         (k (* n bytes-per-word)))
    (+ k (modulo k 2))))

(define (cr:load-tx-frame-buffer! c ifc lst)
  (let* ((frame-length (or (maybe-read-register! c ifc 'frame-length) 8))
         (fb-size (proto-read-register! c ifc 'tx-frame-buffer-size))
         (fb-bytes (octet-address fb-size))
         (required-bytes (required-fb-size frame-length (length lst))))
    (unless (>= fb-bytes required-bytes)
      (throw 'tx-frame-buffer-overflow frame-length required-bytes fb-bytes))
    (let ((block (make-bytevector required-bytes))
          (bytes-per-word (word-width-to-fit frame-length 8)))
      (fold (lambda (word offset)
              (bytevector-uint-set! block offset word 'big bytes-per-word)
              (+ offset bytes-per-word))
            0 lst)
      (let* ((address (proto-read-register! c ifc 'tx-frame-buffer-address))
             (response (regp:write-request! (cr-low-level c) address block)))
        (unless (regp:valid-ack? response)
          (throw 'protocol-error response))))))

(define (cr:fetch-rx-frame-buffer! c ifc n)
  (let* ((frame-length (or (maybe-read-register! c ifc 'frame-length) 8))
         (bytes-per-word (word-width-to-fit frame-length 8))
         (fb-size (proto-read-register! c ifc 'tx-frame-buffer-size))
         (fb-bytes (octet-address fb-size))
         (required-bytes (required-fb-size frame-length n))
         (address (proto-read-register! c ifc 'rx-frame-buffer-address)))
    (unless (>= fb-bytes required-bytes)
      (throw 'rx-frame-buffer-overflow frame-length required-bytes fb-bytes))
    (let ((response (regp:read-request! (cr-low-level c) address (/ required-bytes 2))))
      (unless (regp:valid-ack? response)
        (throw 'protocol-error response))
      (let ((data (assq-ref response 'payload)))
        (unfold-right (lambda (m) (< m 1))
                      (lambda (m)
                        (bytevector-uint-ref data (* bytes-per-word (1- m))
                                             'big bytes-per-word))
                      1-
                      n)))))

(define (in-range? n lower upper)
  (and (>= n lower)
       (<= n upper)))

(define (i2c-what-section-size? n)
  (cond ((in-range? n 0 31)    'short)
        ((in-range? n 32 8191) 'extended)
        (else 'too-large)))

(define (i2c-msg-size lst)
  (fold (lambda (e acc)
          (let* ((n (if (bytevector? e)
                        (bytevector-length e)
                        e))
                 (s (if (eq? 'short (i2c-what-section-size? n)) 1 2))
                 (m (if (bytevector? e) n 0)))
            (+ acc s m)))
        0 lst))

(define (make-i2c-spec-value w? e? l? n)
  (logior (ash (logior (if w? #x80 0)
                       (if e? #x40 0)
                       (if l? #x20 0))
               (if e? 8 0))
          (logand n (one-bits (if e? 13 5)))))

(define (i2c-encode-spec! block lst)
  (let loop ((rest lst) (offset 0))
    (if (null? rest)
        offset
        (let* ((e (car rest))
               (last? (null? (cdr rest)))
               (write? (bytevector? e))
               (n (if write? (bytevector-length e) e))
               (extended? (case (i2c-what-section-size? n)
                            ((short) #f)
                            ((extended) #t)
                            (else (throw 'i2c-section-too-large n))))
               (size (if extended? 2 1)))
          (bytevector-uint-set! block offset
                                (make-i2c-spec-value write? extended? last? n)
                                'big size)
          (loop (cdr rest) (+ offset size))))))

(define (i2c-encode-tx! block offset lst)
  (let loop ((rest lst) (offset offset))
    (if (null? rest)
        #t
        (let* ((chunk (car rest))
               (n (bytevector-length chunk)))
          (bytevector-copy! chunk 0 block offset n)
          (loop (cdr rest) (+ offset n))))))

(define (maybe-pad n)
  (+ n (logand n 1)))

(define (cr:load-i2c-message! c ifc lst)
  "Upload a message to the firmware's tx framebuffer.

The message is described by the contents of LST. Bytevectors describe write
sections of a message, and integers describe the length of read sections.

After loading, the message can be send using the transmit command.

The read back data will be available in the RX frame buffer afterwards."
  (let* ((n (i2c-msg-size lst))
         (block (make-bytevector (maybe-pad n)))
         (txoffset (i2c-encode-spec! block lst)))
    (i2c-encode-tx! block txoffset (filter bytevector? lst))
    (let* ((address (proto-read-register! c ifc 'tx-frame-buffer-address))
           (response (regp:write-request! (cr-low-level c) address block)))
      (unless (regp:valid-ack? response)
        (throw 'protocol-error response)))))

(define (cr:fetch-i2c-rx-sections! c ifc lst)
  (cr:fetch-rx-frame-buffer! c ifc (apply + lst)))

(define (cr:i2c-transceive! c ifc lst)
  "Perform a full I2C transaction.

This loads the TX framebuffer, performs a transaction and then retrieves the
data from the RX framebuffer."
  (cr:load-i2c-message! c ifc lst)
  (let ((rc (cr:ctrl-comand! c ifc 'transmit)))
    (if (eq? rc 'success)
        (cr:fetch-i2c-rx-sections! c ifc (filter integer? lst))
        (throw 'protocol-status rc))))

(define (cr:spi-transceive! c ifc lst)
  "Perform a full SPI transaction.

This loads the TX framebuffer, performs a transaction and then retrieves the
data from the RX framebuffer."
  (let ((n (length lst)))
    (cr:load-tx-frame-buffer! c ifc lst)
    (let ((rc (cr:ctrl-comand! c ifc 'transmit n)))
      (if (eq? rc 'success)
          (cr:fetch-rx-frame-buffer! c ifc n)
          (throw 'protocol-status rc)))))
