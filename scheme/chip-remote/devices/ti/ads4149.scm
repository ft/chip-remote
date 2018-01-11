;; Copyright (c) 2014-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti ads4149)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote decode)
  #:use-module (chip-remote level-3)
  #:use-module (chip-remote protocol)
  #:use-module ((chip-remote devices ti ads4149 program)
                #:renamer (symbol-prefix-proc 'lvl2/))
  #:use-module (chip-remote devices ti ads4149 registers)
  #:export (decode-device
            decode-register
            decode-register-value
            read-register
            setup-connection
            write-register

            enable-readout
            disable-readout
            reset-device
            enable-high-performance-mode
            disable-high-performance-mode
            set-custom-pattern))

(define (setup-connection conn index)
  "Set up a port in the remote controller to work with the ADS4149 ADC.

The ADS4149 uses an SPI interface for configuration. It transmits data in
frames of 16 bits, that are build like this:

  <REGISTER-ADDRESS><VALUE>

Where both REGISTER-ADDRESS and VALUE are eight bit values aligned within the
frame like this like this:

  A7:A6:A5:A4:A3:A2:A1:A0:V7:V6:V5:V4:V3:V2:V1:V0

That frame is then transmitted from left to right (ie. MSB-FIRST).

The chip-select pin works as active-low and data is latched at the falling edge
of the clock-pin (with no further phase delay on the clock line).

This procedure performs the according configuration and initialises the port at
the given index. However, it does NOT switch focus to the configured port."
  (set conn index 'mode 'spi)
  (set conn index 'frame-length 16)
  (set conn index 'bit-order 'msb-first)
  (set conn index 'clk-polarity 'falling-edge)
  (set conn index 'clk-phase-delay #f)
  (init conn 0))

(define (valid-readback-address? a)
  (and (not (= a 0))
       (valid-register-address? a)))

(define (read-register conn addr)
  "Read the value of a register back from an ADS4149 device.

Unfortunately, register-readback is a bit of a mess with this ADC. Its
overrange pin can be made to perform the task of SPI's MISO pin.

To make it do that, the `enable-readout' function can be used. After that, the
chip ONLY allows writes to the register at address 0 (where the READOUT bit
resides), which also CANNOT be read back.

In readback-mode the VALUE bits on SPI's MOSI pin are ignored by the device.

So, what this procedure does is this:

  - Enable register readout.
  - Read the requested register value back from the device.
  - Disable register readout mode again.
  - Return the retrieved register value.

So, for initial setup it is probably best to assemble complete register values
using the second-level access API from the (... ads4149 program) module and
transmit those using `write-register' directly; and only keep the third-level
API for experimentation purposes."
  (if (not (valid-readback-address? addr))
      (throw 'cr-invalid-address addr))
  (enable-readout conn)
  (let ((return-value (write-register* conn addr 0)))
    (disable-readout conn)
    (logand #xff return-value)))

(define (build-frame address value)
  (logior (ash address 8) value))

(define (write-register* conn addr value)
  (transmit conn (build-frame addr value)))

(define (write-register conn addr value)
  (if (not (valid-register-address? addr))
      (throw 'cr-invalid-address addr))
  (write-register* conn addr value))

(define* (decode-register-value addr value #:key (colour? (to-tty?)))
  (value-decoder ads4149-register-map ads4149-register-width addr value colour?))

(define* (decode-register conn addr #:key (colour? (to-tty?)))
  (decode-register-value addr (read-register conn addr) #:colour? colour?))

(define* (decode-device conn #:key (colour? (to-tty?)))
  (device-decoder #:register-map ads4149-register-map
                  #:reader (lambda (a) (read-register conn a))
                  #:decoder (lambda (a v) (decode ads4149-register-map a v))
                  #:interconnections ads4149-regmap-interconnections
                  #:filter-predicate valid-readback-address?
                  #:width ads4149-register-width
                  #:colour? colour?))

(define-bit-field-frontends
  (disable-clkout-fall-control regaddr:enable-clkout-fall
                               lvl2/disable-clkout-fall-control)
  (disable-clkout-rise-control regaddr:enable-clkout-rise
                               lvl2/disable-clkout-rise-control)
  (disable-freeze-offset-correction regaddr:freeze-offset-correction
                                    lvl2/disable-freeze-offset-correction)
  (disable-gain regaddr:disable-gain lvl2/disable-gain)
  (disable-low-latency regaddr:disable-low-latency lvl2/disable-low-latency)
  (disable-low-speed regaddr:low-speed lvl2/disable-low-speed)
  (disable-lvds-swing-control regaddr:enable-lvds-swing
                              lvl2/disable-lvds-swing-control)
  (disable-offset-correction regaddr:enable-offset-correction
                             lvl2/disable-offset-correction)
  (disable-power-down-global regaddr:power-down-global
                             lvl2/disable-power-down-global)
  (disable-power-down-outputs regaddr:power-down-outputs
                              lvl2/disable-power-down-outputs)
  (disable-standby regaddr:standby lvl2/disable-standby)
  (enable-freeze-offset-correction regaddr:freeze-offset-correction
                                   lvl2/enable-freeze-offset-correction)
  (enable-gain regaddr:disable-gain lvl2/enable-gain)
  (enable-low-latency regaddr:disable-low-latency lvl2/enable-low-latency)
  (enable-low-speed regaddr:low-speed lvl2/enable-low-speed)
  (enable-lvds-swing-control regaddr:enable-lvds-swing
                             lvl2/enable-lvds-swing-control)
  (enable-offset-correction regaddr:enable-offset-correction
                            lvl2/enable-offset-correction)
  (enable-power-down-global regaddr:power-down-global
                            lvl2/enable-power-down-global)
  (enable-power-down-outputs regaddr:power-down-outputs
                             lvl2/enable-power-down-outputs)
  (enable-standby regaddr:standby lvl2/enable-standby)
  (set-clkout-fall-posn regaddr:clkout-fall-posn
                        lvl2/set-clkout-fall-posn (mode value))
  (set-clkout-rise-posn regaddr:clkout-rise-posn
                        lvl2/set-clkout-rise-posn (mode value))
  (set-cmos-clkout-strength regaddr:cmos-clkout-strength
                            lvl2/set-cmos-clkout-strength (value))
  (set-data-format regaddr:data-format lvl2/set-data-format (value))
  (set-gain regaddr:gain lvl2/set-gain (value))
  (set-lvds-clkout-strength regaddr:lvds-clkout-strength
                            lvl2/set-lvds-clkout-strength (value))
  (set-lvds-cmos regaddr:lvds-cmos lvl2/set-lvds-cmos (value))
  (set-lvds-data-strength regaddr:lvds-data-strength
                          lvl2/set-lvds-data-strength (value))
  (set-lvds-swing regaddr:lvds-swing lvl2/set-lvds-swing (value))
  (set-offset-correction-time-constant
   regaddr:offset-correction-time-constant
   lvl2/set-offset-correction-time-constant (value))
  (set-offset-pedestal regaddr:offset-pedestal lvl2/set-offset-pedestal (value))
  (set-test-pattern regaddr:test-pattern lvl2/set-test-pattern (value)))

(define (enable-readout conn)
  (write-register conn regaddr:readout (lvl2/enable-readout 0)))

(define (disable-readout conn)
  (write-register conn regaddr:readout (lvl2/disable-readout 0)))

(define (reset-device conn)
  (write-register conn regaddr:reset (lvl2/reset-device 0)))

(define (enable-high-performance-mode conn)
  (replay-register conn regaddr:high-performance-mode-1
                   lvl2/enable-high-performance-mode-1)
  (replay-register conn regaddr:high-performance-mode-2
                   lvl2/enable-high-performance-mode-2))

(define (disable-high-performance-mode conn)
  (replay-register conn regaddr:high-performance-mode-1
                   lvl2/disable-high-performance-mode-1)
  (replay-register conn regaddr:high-performance-mode-2
                   lvl2/disable-high-performance-mode-2))

(define (set-custom-pattern conn value)
  (let ((l (logand #x3f value))
        (h (bit-extract-width value 6 8)))
    (write-register conn regaddr:custom-pattern-high
                          (lvl2/set-custom-pattern-high 0 h))
    (write-register conn regaddr:custom-pattern-low
                          (lvl2/set-custom-pattern-low 0 l))))
