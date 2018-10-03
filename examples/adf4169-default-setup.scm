;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 match)
             (srfi srfi-1)
             (chip-remote)
             (chip-remote io)
             (chip-remote protocol)
             (chip-remote devices analog-devices adf4169)
             (chip-remote device)
             (termios)
             (termios system))

(define serial-device
  (match (command-line)
    ((_ device) device)
    (else (format #t "Provide the serial device as the script's argument!~%")
          (quit 0))))

;; A couple of parameters describing the pin-out of the remote-control
;; firmware's GPIO port.
(define port-index 0)
(define pin-chip-select 0)
(define pin-clock 1)
(define pin-master-out-slave-in 2)
(define pin-master-in-slave-out 3)

;; Set 4Mbd 8N1 for the serial port via guile-termios. The Nucleo-144's MCU has
;; a built-in USB interface that's capable of data rates like this.
(define tty (open-io-file serial-device))
(define ts (make-termios-struct))

(cf-make-raw! ts)
(cf-set-speed! ts termios-B4000000)
(tc-set-attr tty ts)

(close-port tty)

;; Initialise the connection with the io module.
(define connection (make-cr-connection serial-device))
(io-opt/set 'trace #t)
(io-open connection)
(hi connection)

;; Configure lines in a port, so we can talk SPI.
(for-each
 (lambda (x)
   (line connection port-index (cdr x) (car x)))
 `((cs . ,pin-chip-select)
   (clk . ,pin-clock)
   (mosi . ,pin-master-out-slave-in)
   (miso . ,pin-master-in-slave-out)))

;; Configure SPI for the adf4169.
(for-each
 (lambda (x)
   (set connection port-index (car x) (cdr x)))
 '((mode . spi)
   (frame-length . 32)
   (bit-order . lsb-first)
   (clk-phase-delay . 1)
   (clk-polarity . rising-edge)
   (cs-polarity . active-low)))

;; Take the port up and running.
(init connection port-index)
(focus connection port-index)

;; Transfer the whole register table into the device. Note that the registers
;; are transfered in reverse order like the chip's data sheet suggests.
(for-each
 (lambda (x)
   (transmit connection x))
 (reverse (car (device-default adf4169))))

;; Tear down the chip-remote connection and close the serial device.
(bye connection)
(io-close connection)
