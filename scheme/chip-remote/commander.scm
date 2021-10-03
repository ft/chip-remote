;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
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
  #:use-module (chip-remote device transmit)
  #:use-module (chip-remote frontend)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote io)
  #:use-module (chip-remote modify)
  #:use-module (chip-remote protocol)
  #:export (make-commander))

(define-record-type <cmdr-state>
  (make-cmdr-state device connection port address default decode open-hook)
  cmdr-state?
  (device get-device update-device!)
  (connection get-connection)
  (port get-port)
  (address get-address)
  (default get-default)
  (decode get-decoder set-decoder!)
  (open-hook get-open-hook))

(define (show cmdr spec value)
  (let ((run-decoder (get-decoder cmdr)))
    (run-decoder spec value)))

(define *void* (if #f #f))

(define (must-be-connected state)
  (let* ((conn (get-connection state))
         (io-port (cr-connection-port conn)))
    (unless (and io-port
                 (port? io-port)
                 (not (port-closed? io-port)))
      (throw 'connection-not-opened conn)))
  *void*)

(define (cmdr-command cmd state)
  (case cmd
    ((close!)
     (must-be-connected state)
     (io-close (get-connection state)))
    ((data)
     (current-device-state (get-device state)))
    ((decode)
     (let ((device (get-device state)))
       (show state device (current-device-state device))))
    ((device)
     (get-device state))
    ((focus!)
     (must-be-connected state)
     (let ((c (get-connection state))
           (port (get-port state))
           (addr (get-address state)))
       (cr:setup-port! c port (get-device state))
       ;;(focus c port)
       (when addr (address c addr))))
    ((open!)
     (let ((c (get-connection state)))
       (io-open c)
       ((get-open-hook state) (get-connection state) (get-port state))))
    ((reset!)
     (cr:reset (get-device state) (get-default state)))
    ((trace!)
     (assq 'trace (io-opt/set 'trace (not (io-opt/get 'trace)))))
    ((push!)
     (must-be-connected state)
     (update-device! state (cr:push! (get-connection state) (get-device state)))
     *void*)

    ;; Unknown commands error out.
    (else (throw 'unknown-simple-command cmd))))

(define (transmit-data c dev data)
  (let ((addr (assq-ref data 'address))
        (part (assq-ref data 'part))
        (value (assq-ref data 'value))
        (write-data (da-write (device-access dev))))
    (transmit c (write-data (first addr) (second addr) value))))

(define (cmdr-w/rest cmd args state)
  (case cmd
    ((decode)
     (let* ((device (get-device state))
            (extr (device-extract device (current-device-state device) args)))
       (show state (assq-ref extr 'part) (assq-ref extr 'item))))
    ((change!)
     (must-be-connected state)
     (update-device! state (apply cr:change! (cons* (get-connection state)
                                                    (get-device state)
                                                    args)))
     *void*)
    ((load!)
     (update-device! state (cr:load (get-device state) (car args)))
     *void*)
    ((set!)
     (update-device! state (apply cr:load (cons (get-device state) args)))
     *void*)
    ((transmit!)
     (must-be-connected state)
     (let* ((device (get-device state))
            (c (get-connection state))
            (extr (device-extract device (current-device-state device) args)))
       (transmit-data c (get-device state) extr)))

    ;; Unknown commands error out here as well.
    (else (throw 'unknown-complex-command cmd args))))

(define* (make-commander #:key
                         device connection default (decode cr:decode)
                         (port 0) address (open-hook (lambda (c n) #t)))
  "Return a device commander object

The chip-remote library provides a powerful framework to express configuration
interfaces. The original intend was to be able to interactively play with
devices that employ these interfaces (hence the name *chip-remote*). The
commander objects constructed by this function serve as frontends that hide a
lot of the flexibility of the library in order to reduce the amount of typing
required by interactive users.

Commanders tie together devices and connections. They also store a copy of the
chip's register memory. This representation can be manipulated and sent to the
device in question.

The constructor takes a number of keyword arguments:

- ‘#:device’ → References the description of the device that is to be
  controlled.

- ‘#:connection’ → Refers to the connection the device can be accessed through.

- ‘#:port’ → The port (in RCCEP terms) the device is accessible through. This
  defaults to 0.

- ‘#:address’ → Configure the address of the device within the RCCEP port, if
  applicable. Defaults to 0.

- ‘#:data’ → This can be used to initialise the object's register memory. The
  datum has to fit the device referenced by the commander object. This defaults
  to the device's default values as generated by ‘device-default’.

- ‘#:decode’ → The object can decode parts of it register memory. This is the
  decoder frontend used to present the results. The default is the ‘decode’
  function from the ‘`(chip-remote decode)`’ module. Another possible value is
  ‘decode-to-text’.

- ‘#:open-hook’ → A function of two arguments (‘connection’, ‘port-index’) that
  is called after the object's ‘open!’ operation has finished. This can be used
  to initialise firmwares running in a remote controller.

Objects returned by this constructor are designed for interactive use in a
Scheme REPL. The objects implement a command interface to interact with the
connected device. Example:

    (define pll (make-commander #:device adf4169
                                #:connection my-connection))
    (pll)
    (pll 'push!)
    (pll 'set! '(ramp-enabled #t))

As these examples suggest, the object can be called with zero or more
arguments. The first word in an argument list serves as a *command designator*.
Commands without further arguments are called *\"simple commands\"*. They are:

- ‘open!’ → Opens the objects connection and initialises the RCCEP
  conversation. This calls the *open-hook* afterwards.

- ‘close!’ → Closes the objects connection.

- ‘focus!’ → Focus port and address of the device that the object is connected
  to via RCCEP. The operation also initialises the port to the bus logic as
  described in the referenced device description.

- ‘reset!’ → Resets the register memory to the value supplied at construction
  time.

- ‘trace!’ → Toggles tracing in the ‘(chip-remote io)’ module.

- ‘push!’ → Transmits the entire register memory into the device via RCCEP.
  This order transformations contained in a device description into account in
  order to transfer the all registers in a manner suitable for the connected
  device.

- ‘decode’ → Decode the entire register memory using the configured decoder
  frontend.

- ‘data’ → Return the current state of the register memory.

- ‘device’ → Return the device description that was supplied at construction
  time.

Commands that accept additional arguments are called *\"complex commands\"*.
Some of them are specialised forms of \"simple commands\":

- ‘`decode ADDRESS`’ → Decode part of the register memory.

- ‘`set! KEY-VALUE-SPEC ...`’ → Change parts of the object's register memory.
  The ‘KEY-VALUE-SPEC’ list uses the same format as with ‘chain-modify’.

- ‘`transmit! ADDRESS`’ → Transmit the register that gets touched by the part
  referenced by ‘ADDRESS’.

- ‘`change! KEY-VALUE-SPEC ...`’ → This is like ‘set!’, except that when it is
  done mutating the register memory, it uses ‘`transmit! ADDRESS`’ for all
  registers that where touched by the changes made to the register memory.

- ‘`load! VALUE`’ → Override the register memory using ‘VALUE’. This only
  changes the local memory. Use ‘transmit!’ to transfer the data into the
  connected device. ‘VALUE’ has to be a datum suitable for the device
  referenced by the object. Note that this does *not* override the initial
  value supplied via ‘#:data’ at construction time. The ‘reset!’ operation will
  reset to that initial value, not to a value specified by ‘load!’

Calling the commander object without arguments is equivalent to calling it with
the \"simple command\" ‘decode’: All its register memory will be decoded. Note
that with the default decoder, it might be useful to ask the Guile REPL to
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
    ;; Transmit the local register memory into the device, while
    ;; respecting the proper order in which the registers need to
    ;; be transferred as specified in the ADF4169's description.
    (pll 'push!)

Note that these objects, unlike most of the rest of the library perform lots of
mutations: On connected device of course, and also on their local register
memory copy."
  (unless (device? device)
    (throw 'cr-missing-data 'device device))
  (unless (cr-connection? connection)
    (throw 'cr-missing-data 'connection connection))
  (let ((state (make-cmdr-state device connection port address
                                default decode open-hook)))
    (case-lambda
      (()
       (let ((device (get-device state)))
         (show state device (current-device-state device))))
      ((cmd)
       (cmdr-command cmd state))
      ((cmd . rest)
       (cmdr-w/rest cmd rest state)))))
