;; Copyright (c) 2014-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; This is a fairly elaborate example script to outline the basic use of the
;; ‘chip-remote’ client library. This script assumes that the remote firmware
;; implements pretty much all of the protocol specification from
;; ‘rccep-spec.org’.
;;
;; It also assumes that the port that is being used has enough lines to
;; implement SPI (so at least four lines in the chosen port).
;;
;; This also doesn't catch exceptions, that may be thrown in case of error
;; conditions. Thus it'll exit whenever a problem is encountered. Proper error
;; handling is out of the scope of a mere example script.
;;
;; Finally, this script does everything it has to in one go: It configures the
;; remote firmware and then talks to the connected device. In real-world
;; experimentation those tasks might rather be put into separate scripts.
;; Experimentation may even take place on a REPL instead of from a script.

;; Load some modules, that we'll use:
(use-modules
 ;; This is the module that implements the low-level input/output
 ;; functionality. This script needs it to get a connection object
 ;; and the API to open and close it
 (chip-remote io)
 ;; This module implements the chip-remote protocol.
 (chip-remote protocol)
 ;; And finally this module offers utilities to work with the CDCE72010 device
 ;; from Texas Instruments. See its datasheet for details. Symbols from this
 ;; module will be prefixed with ‘cdce/’, which would make more sense in a
 ;; script that talks to multiple different devices; but showing the
 ;; functionality makes sense in an example script.
 ((chip-remote legacy ti cdce72010) #:renamer (symbol-prefix-proc 'cdce/)))

;; The script is to be called like this:
;;
;;   guile examples/cdce-read-all-registers.scm [serial-device]
;;
;; Where [serial-device] is optional and, if supplied, defines the device to
;; connect to. If it is not supplied, this script defaults to "/dev/ttyUSB0".
(define serial-device (let ((arg (cdr (command-line))))
                        (if (null? arg)
                            "/dev/ttyUSB0"
                            (car arg))))

;; Guile is currently missing a POSIX termios interface. So setting up things
;; like baud-rate, frame-length etc. of the serial connection to the remote
;; firmware is not easily possible. On a POSIX system however (I feel for you,
;; if you're stuck on windoze), there's the ‘stty’ utility, that can do the
;; required setup for us, until Guile gains the required functionality. The
;; following external command call, sets the serial link to 9600bd 8N1 mode:
(let ((base-cmd "stty raw 9600 -crtscts cs8 -parenb -cstopb < "))
  (system (string-concatenate (list base-cmd serial-device))))

;; First of all, we need a handle for the connection to the board we'll be
;; remote-controlling:
(define connection (make-cr-connection serial-device))

;; Enable the IO trace feature. This will show a trace of the protocol exchange
;; between the client library and the remote controller, that looks like this:
;;
;; >>> HI
;; <<< Hi there, stranger.
;; >>> SET 0 MODE SPI
;; <<< OK
;; >>> SET 0 FRAME-LENGTH 20
;; <<< OK
;; >>> SET 0 BIT-ORDER LSB-FIRST
;; <<< OK
;; [...]
;; >>> LINE 0 0 CS
;; <<< OK
;; >>> LINE 0 1 CLK
;; <<< OK
;; [...]
;; >>> INIT 0
;; <<< OK
;; >>> FOCUS 0
;; <<< OK
;; >>> TRANSMIT e
;; <<< 683c0ed0
;; [...]
;; >>> BYE
;; <<< Have a nice day.
(io-opt/set 'trace #t)

;; The board may have multiple ports. This parameter holds the index of the
;; port the CDCE72010 device is connected to.
(define port-index 0)

;; Since we'll be talking to an SPI device, it makes sense to define which line
;; of a port gets to take which job:
(define pin-chip-select 0)
(define pin-clock 1)
(define pin-master-out-slave-in 2)
(define pin-master-in-slave-out 3)

;; Open the connection and greet the device.
(io-open connection)
(hi connection)

;; Assume the remote controlled firmware has support for configurable ports.
;; Set up the port indexed by ‘port-index’ to the interface the CDCE72010 uses.
(map (lambda (x) (set connection port-index (car x) (cdr x)))
     '(;; The CDCE72010 is a device controlled via SPI.
       (mode . spi)
       ;; Transmissions are 32 bits wide.
       (frame-length . 32)
       ;; The bits are transmitted least-significant-bit first.
       (bit-order . lsb-first)
       ;; The datasheet recommends a phase delay.
       (clk-phase-delay . 1)
       ;; The CLK idles low, which means that it trigger on its rising edge.
       (clk-polarity . rising-edge)
       ;; The Chip-Select (in the datasheet: latch-enable) polarity is active
       ;; low (which also means, that it idles high).
       (cs-polarity . active-low)))

;; After a port's mode is configured, its lines (or pins or whatever you want
;; to call them) can be assigned a job. Let's do that quickly:
(map (lambda (x) (line connection port-index (cdr x) (car x)))
     `((cs . ,pin-chip-select)
       (clk . ,pin-clock)
       (mosi . ,pin-master-out-slave-in)
       (miso . ,pin-master-in-slave-out)))

;; After changing settings of a port, it needs to be initialised:
(init connection port-index)
;; Focus the configured and initialised port:
(focus connection port-index)

;; The (chip-remote legacy ti cdce72010) module features a number of very
;; high-level routines for handling the device. Reading and decoding all
;; registers inthe CDCE72010 device is achieved by one single procedure:
(cdce/decode-device connection) ;; Note, that the way the cdce72010 module is
                                ;; imported (with the renamer), this function
                                ;; is imported as ‘cdce/decode-device’ rather
                                ;; than simply ‘decode-device’.

;; End the conversation and close the device.
(bye connection)
(io-close connection)
