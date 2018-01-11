;; Copyright (c) 2014-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices fhac tiadc)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote assemble)
  #:use-module (chip-remote decode)
  #:use-module (chip-remote level-3)
  #:use-module (chip-remote protocol)
  #:use-module ((chip-remote devices fhac tiadc program)
                #:renamer (symbol-prefix-proc 'lvl2/))
  #:use-module (chip-remote devices fhac tiadc registers)
  #:export (decode-device
            decode-register
            decode-register-value
            setup-connection
            write-register))

;; Basic high-level functionality

(define (setup-connection conn index)
  (set conn index 'mode 'par-ex)
  (init conn index))

(define (write-register conn addr value)
  "The par-ex interface looks like this:

  [ADDR2,ADDR1,ADDR0][DAT7,DAT6,DAT5,DAT4,DAT3,DAT2,DAT1,DAT0]

It's a parallel bus. The ADDR* part is master->slave only. The DAT* part is
shared. A transmission consists of a WRITE and a READ cycle (from the view
point of the master, which this module implements).

The ADDR* part is used as register-addresses in here. So this function works
pretty simple, by putting the ADDR part at that place in the transmitted word
and the VALUE part in the DAT* part of the transmitted word.

Note: Reading registers works be writing to a special register. The
      `read-register' function is therefore implemented in terms of
      `write-register'to that particular register."
  (with-constraints (addr (>= 0) (<= 7))
    (transmit conn (logior (ash addr 8) value))))

;; ‘read-register’ is an extended-command on this device

;; Frontends for extended commands

(define extcmd-addr 7)

(define (write-extcmd-w/value conn cmd value)
  (write-register conn extcmd-addr
                  (logior (lvl2/set-extended-cmd-word 0 cmd)
                          (lvl2/set-extended-cmd-data 0 value))))

(define (write-extcmd conn cmd)
  (write-register conn extcmd-addr (lvl2/set-extended-cmd-word 0 cmd)))

(define-public (reset-device conn)
  (write-extcmd conn 'reset))

(define-public (trigger-readout conn)
  (write-extcmd conn 'usb-readout))

(define-public (abort-readout conn)
  (write-extcmd conn 'usb-abort))

(define-public (take-snapshot conn)
  (write-extcmd conn 'snapshot))

(define-public (read-register conn addr)
  (write-extcmd-w/value conn 'read-register addr))

;; Decoder frontends

(define* (decode-register-value addr value #:key (colour? (to-tty?)))
  (value-decoder tiadc-register-map tiadc-register-width addr value colour?))

(define* (decode-register conn addr #:key (colour? (to-tty?)))
  (decode-register-value addr (read-register conn addr) #:colour? colour?))

(define* (decode-device conn #:key (colour? (to-tty?)))
  (device-decoder #:register-map tiadc-register-map
                  #:reader (lambda (a) (read-register conn a))
                  #:decoder (lambda (a v) (decode tiadc-register-map a v))
                  #:interconnections tiadc-regmap-interconnections
                  #:filter-predicate (lambda (x) (<= x 6))
                  #:width tiadc-register-width
                  #:colour? colour?))

;; Other level-3 code

(define (set-readout-length conn value)
  (write-register conn regaddr:readout-length
                  (lvl2/set-readout-length 0 value)))

(define (set-readout-mask conn value)
  (write-register conn regaddr:readout-mask
                  (lvl2/set-readout-mask 0 value)))

(define (set-readout-mode conn value)
  (write-register conn regaddr:readout-mode
                  (lvl2/set-readout-mode 0 value)))

(define-public (set-buffer conn value)
  (with-constraints (value (>= 0) (<= #xffffffff))
    (let ((data (split-word value 8 8 8 8)))
      (write-register conn regaddr:buffer-d (list-ref data 0))
      (write-register conn regaddr:buffer-c (list-ref data 1))
      (write-register conn regaddr:buffer-b (list-ref data 2))
      (write-register conn regaddr:buffer-a (list-ref data 3)))))
