;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote commander)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module ((chip-remote decode) #:prefix cr:)
  #:use-module (chip-remote item)
  #:use-module (chip-remote device)
  #:use-module (chip-remote device access)
  #:use-module (chip-remote device transfer)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote io)
  #:use-module (chip-remote modify)
  #:use-module (chip-remote protocol)
  #:export (make-commander))

(define-record-type <cmdr-state>
  (make-cmdr-state dev con port address default data decode)
  cmdr-state?
  (dev get-device)
  (con get-connection)
  (port get-port)
  (address get-address)
  (default get-default)
  (data get-data set-data!)
  (decode show))

(define (must-be-connected state)
  (let* ((conn (get-connection state))
         (io-port (cr-connection-port conn)))
    (unless (and io-port
                 (port? io-port)
                 (not (port-closed? io-port)))
      (throw 'connection-not-opened conn)))
  #t)

(define (cmdr-command cmd state)
  (case cmd
    ((close!)
     (must-be-connected state)
     (let ((c (get-connection state)))
       (bye c)
       (io-close c)))
    ((data)
     (get-data state))
    ((decode)
     ((show state) (get-device state) (get-data state)))
    ((device)
     (get-device state))
    ((focus!)
     (must-be-connected state)
     (let ((c (get-connection state)))
       (focus c (get-port state))
       (address c (get-address state))))
    ((open!)
     (let ((c (get-connection state)))
       (io-open c)
       (hi* c)))
    ((reset!)
     (set-data! state (get-default state)))
    ((trace!)
     (assq 'trace (io-opt/set 'trace (not (io-opt/get 'trace)))))
    ((transmit!)
     (must-be-connected state)
     (let* ((dev (get-device state))
            (acc (device-access dev))
            (transfer (da-transfer acc))
            (transform (transfer-transform transfer))
            (write-data (da-write acc)))
       (for-each
        (lambda (addr) (cmdr-w/rest 'transmit! (list addr) state))
        (transform (address-map->addresses (device-address-map dev))))))

    ;; Unknown commands error out.
    (else (throw 'unknown-simple-command cmd))))

(define (transmit-data c dev data)
  (let ((addr (assq-ref data 'address))
        (part (assq-ref data 'part))
        (value (assq-ref data 'value))
        (write-data (da-write (device-access dev))))
    (transmit c (write-data (first addr) (second addr) value))))

(define (touched-registers dev lst)
  (let loop ((rest lst) (acc '()))
    (if (null? rest)
        acc
        (let ((this (car rest)))
          (loop (cdr rest) (if (member this acc) acc (cons this acc)))))))

(define (cmdr-w/rest cmd args state)
  (case cmd
    ((decode)
     (let ((extr (device-extract (get-device state) (get-data state) args)))
       ((show state) (assq-ref extr 'part) (assq-ref extr 'item))))
    ((change!)
     (must-be-connected state)
     (cmdr-w/rest 'set! args state)
     (let ((dev (get-device state)))
       (for-each
        (lambda (addr)
          (cmdr-w/rest 'transmit! (list addr) state))
        (touched-registers dev
                           (map (compose (lambda (addr)
                                           (take addr 2))
                                         (lambda (addr)
                                           (find-canonical-address dev addr))
                                         car)
                                args)))))
    ((load!)
     (set-data! state (car args)))
    ((set!)
     (set-data! state (apply chain-modify
                             (cons (get-device state)
                                   (cons (get-data state) args)))))
    ((transmit!)
     (must-be-connected state)
     (let ((c (get-connection state))
           (extr (device-extract (get-device state) (get-data state) args)))
       (transmit-data c (get-device state) extr)))

    ;; Unknown commands error out here as well.
    (else (throw 'unknown-complex-command cmd args))))

(define* (make-commander #:key
                         device connection data decode
                         (port 0) (address 0))
  "Return a device commander object

The chip-remote library provides a powerful framework to express configuration
interfaces. The original intend was to be able to interactively play with such
devices. The commander objects constructed by this function serve as frontends
that hide a lot of the flexibility of the library in order to reduce the amount
of typing required by interactive users.

Commanders tie together devices and connections. They also store a copy of the
chip's register tables. This representation can be manipulated and sent to the
device in question.

The constructor takes a number of keyword arguments:

- ‘#:device’ → Names the device that should be controlled.

- ‘#:connection’ → Refers to the connection the device can be accessed through.

- ‘#:port’ → The port (in RCCEP terms) the device is accessible through. This
  defaults to 0.

- ‘#:address’ → Configure the address of the device within the RCCEP port, if
  applicable. Defaults to 0.

- ‘#:data’ → This can be used to initialise the object's register memory. It
  has to fit the device referenced by the commander object. This defaults to the
  device's default values as generated by ‘device-default’.

- ‘#:decode’ → The object can decode parts of it register memory. This is the
  decoder frontend used to present the results. This defaults to ‘decode’.
  Another possible value is ‘decode-to-text’.

Objects returned by this constructor are designed for interactive use in a
Scheme REPL. It implements a command interface to interact with the connected
device. Example:

    (define pll (make-commander #:device adf4169
                                #:connection my-connection))
    (pll)
    (pll 'transmit!)
    (pll 'set! '(ramp-enabled #t))

As these examples suggest, the object can be called with zero or more
arguments. The first word in an argument list serves as a command designator.
Commands without further arguments are called \"simple commands\". They are:

- ‘open!’ → Opens the objects connection.

- ‘close!’ → Closes the objects connection

- ‘focus!’ → Focus port and addres of the device that the object is connected
  to via RCCEP.

- ‘reset!’ → Resets the register memory to the value supplied at contruction
  time.

- ‘trace!’ → Toggles tracing in the ‘(chip-remote io)’ module.

- ‘transmit!’ → Transmits the entire register memory into the device via RCCEP.

- ‘decode’ → Decode the entire register memory using the configured decoder
  frontnd.

- ‘data’ → Return the current state of the register memory.

- ‘device’ → Return the device description that was supplied at construction
  time.

Commands that accept additional arguments are called \"complex commands\". Some
of them are specialised forms of \"simple commands\":

- ‘`decode ADDRESS`’ → Decode part of the register memory.

- ‘`set! KEY-VALUE-SPEC ...`’ → Change parts of the object's register memory.
  The ‘KEY-VALUE-SPEC’ list uses the same format as with ‘chain-modify’.

- ‘`transmit! ADDRESS`’ → Transmit the register that gets touched by the part
  referenced by ‘ADDRESS’.

- ‘`change! KEY-VALUE-SPEC` ...’ → This is like ‘set!’, except that when it is
  done mutating the register memory, it uses ‘`transfer! ADDRESS`’ for all
  registers that where touched by the changes made to the register memory.

- ‘`load! VALUE`’ → Override the register memory using ‘VALUE’. This only
  changes the local memory. Use ‘transmit!’ to transfer the data into the
  connected device. ‘VALUE’ has to be a datum suitable for the device referenced
  by the object.

Calling the commander object without arguments is equivalent to calling it with
the ‘decode’ \"simple command\": All its register memory will be decoded. Note
what with the default decoder, it might be useful to ask the Guile REPL to
pretty print the result:

    ,pp (pll)

Examples:

    ;; Just decode the ramp-enabled? item
    ,pp (pll 'decode 'ramp-enabled?)
    ;; Decode register number 3. This short-hand works, because
    ;; the ADF4169 only has one single memory page.
    ,pp (pll 'decode 3)
    ;; Set a couple of value in the local register memory:
    (pll 'set! '(power-down? #f)
               '(reference-div-by-2? #t)
               '(ramp-mode triangular))
    ;; Transfer the local register memory into the device, while
    ;; respecting the proper order in which the registers need to
    ;; be transferred as specified in the ADF4169's description.
    (pll 'transmit!)"
  (unless (device? device)
    (throw 'cr-missing-data 'device device))
  (unless (cr-connection? connection)
    (throw 'cr-missing-data 'connection connection))
  (let* ((default (or data (device-default device)))
         (state (make-cmdr-state device connection port
                                 address default default
                                 (or decode cr:decode))))
    (case-lambda
      (()
       ((show state) (get-device state) (get-data state)))
      ((cmd)
       (cmdr-command cmd state))
      ((cmd . rest)
       (cmdr-w/rest cmd rest state)))))
