;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; This is the top level cdce72010 manipulation module. It is intended to
;; make changing settings of the device as straight forward as possible. If
;; you prefer the symbols from this module to have a `cdce/' prefix, use:
;;
;;      (use-modules
;;       ((chip-remote devices ti cdce72010)
;;        #:renamer (symbol-prefix-proc 'cdce/)))
;;
;; Note, that the code in this module assumes that the serial connection to
;; the microcontroller is stable. If you need this for unattended control in
;; unstable environments, you may want to use the backend code directly.
;;
;; This code concentrates on sanitising procedure parameters to avoid
;; problems during interactive use. For example, trying to change divider 9
;; will cause an error, because divider 9 does not exist on the device.

(define-module (chip-remote devices ti cdce72010)
  #:use-module (bitops)
  #:use-module (chip-remote protocol)
  #:use-module (chip-remote devices ti cdce72010 decode)
  #:use-module (chip-remote devices ti cdce72010 messages)
  #:use-module (chip-remote devices ti cdce72010 prg)
  #:use-module (chip-remote devices ti cdce72010 validate)
  #:export (decode-device
            decode-register
            disable-output-divider
            enable-output-divider
            export-registers
            power-down-device
            power-down-pll
            power-up-device
            power-up-pll
            read-registers
            read-register
            set-feedback-divider
            set-m-divider
            set-n-divider
            set-output-divider
            set-output-mode
            set-reference-divider
            write-eeprom
            write-register
            cdce-write-eeprom-locked-forever
            cdce-write-eeprom-unlocked))


(define cdce-write-eeprom-locked-forever #xa03f)
(define cdce-write-eeprom-unlocked #x1f)

;; I'm not adding a function that writes the EEPROM locked: I've had something
;; like that in very early versions, and locked the EEPROM of a device by
;; accident. So, if you want to really really do it, use the constant defined
;; above, and transmit it manually. Note, that you need to wait for at least
;; 50ms, after writing EEPROM in either case!
;;
;; In the CDCE72010 datasheet (SCAS858B, the revision from August 2011), see
;; table 4 on page 20 and the section "Writing to EEPROM" in page 21 for
;; details.
(define (write-eeprom conn)
  (let ((reply (transmit conn cdce-write-eeprom-unlocked)))
    (usleep 100000)
    reply))

(define (write-register conn ridx value)
  "Write VALUE to register RIDX.

In the CDCE72010, you got thirteen 28 Bit registers, which require four bits to
address (it actually uses the full four bits, to implement things like reading
registers and writing the current configuration to EEPROM). Hence, you write 32
Bits to it via SPI LSB-first, with data aligned like this:

  vvvvvvvvvvvvvvvvvvvvvvvvvvvvAAAA   (v: value; A: address)"
  (unless (and (>= value 0)
               (< value (ash 1 28)))
    (throw 'value-out-of-range value))
  (unless (and (>= ridx 0)
               (< ridx 16))
    (throw 'register-index-out-of-range ridx))
  (transmit conn (logior (ash value 4) ridx)))

(define (read-register conn ridx)
  "Read the value of register RIDX.

On the CDCE72010 this works be first writing the address you want to read into
a pseudo register 0x0e. Like this:

  xxxxxxxxxxxxxxxxxxxxxxxxAAAA1110   (x: don't-care; A: address)

That'll make the device spit out the value of register AAAA to MISO in the next
32 Bit transfer (in which case the value on MOSI is don't-care).

That's almost all, except for a hardware bug: When you read a register from a
CDCE72010 device, the LSB will *always* be zero. Hence if you'd read all
registers from a device, the last nibble (so the address bits) would come up
like this: 0, 0, 2, 2, 4, 4, 6, 6 ...

In this function, we can fix the return value, since we know the address we
have requested (it's in RIDX). This is the reason for the final `logior' at the
end of the function."
  (unless (and (>= ridx 0)
               (< ridx 16))
    (throw 'register-index-out-of-range ridx))
  ;; Tell the chip which register's data to send:
  (write-register conn #x0e ridx)
  ;; Collect the data (with the hardware bug-fix):
  (logior (write-register conn 0 0) ridx))

(define (read-registers conn)
  (let ((a '()))
    (let loop ((i 0))
      (set! a (append a (list (read-register conn i))))
      (if (< i 12)
          (loop (+ i 1))))
    a))

(define (export-registers-c-array registers)
  (display "/* for `uint32_t' */\n#include <stdint.h>\n\n")
  (display "uint32_t\nregs[] = {\n")
  (let nr ((r registers))
    (cond
     ((null? r) #t)
     (else
      (format #t "    0x~8,'0x" (car r))
      (if (null? (cdr r))
          (newline)
          (display ",\n"))
      (nr (cdr r)))))
  (display "};\n"))

(define (export-registers-scheme-script registers)
  (display "(use-modules ((chip-remote devices ti cdce72010)\n")
  (display "              #:renamer (symbol-prefix-proc 'cdce/)))\n")
  (display "(use-modules ((chip-remote io)\n")
  (display "              (chip-remote protocol))\n\n")
  (display "(define connection\n")
  (display "        (make-cr-connection \"/dev/ttyUSB0\")\n")
  (display "(io-open connection)\n")
  (display "(hi connection)\n\n")
  (let nr ((i 0)
           (r registers))
    (cond ((null? r) #t)
          (else
           (format #t "(write-register cconnection ~2d #x~8,'0x)\n" i (car r))
           (nr (1+ i) (cdr r)))))
  (display "\n(bye connection)\n")
  (display "(io-close connection)\n"))

(define (export-registers-scheme-list registers type)
  (display "(define registers\n  '(")
  (let ((fmt (cond
              ((equal? type 'hex)    "#x~8,'0x")
              ((equal? type 'binary) "#b~32,'0b")
              (else "Unknown scheme-list export type ~d"))))
    (let nr ((r registers))
      (cond
       ((null? r)
        (display "))\n")
        #t)
       (else
        (format #t fmt (car r))
        (if (not (null? (cdr r)))
            (display "\n    "))
        (nr (cdr r)))))))

(define (export-registers registers . mode)
  (let ((argc (length mode)))
    (cond
     ((and (not (= argc 1))
           (not (= argc 0)))
      (format #t "usage: (export-registers <register-list> [mode])\n")
      #f)
     (else
      (let ((m (if (= argc 1)
                   (car mode)
                   'c-array)))
        (cond
         ((equal? m 'c-array)
          (export-registers-c-array registers))
         ((equal? m 'scheme-script)
          (export-registers-scheme-script registers))
         ((equal? m 'scheme-list-hex)
          (export-registers-scheme-list registers 'hex))
         ((equal? m 'scheme-list-binary)
          (export-registers-scheme-list registers 'binary))
         (else
          (format #t "Unknown export mode: `~a'\n" (symbol->string m))
          #f)))))))

(define (set-feedback-divider conn val)
  (let ((fbdivreg 11))
    (write-register conn fbdivreg
                         (set-bits-fbdiv (read-register conn fbdivreg) val))))

(define (set-output-divider conn div val)
  ;; There are ten outputs but only eight dividers. Outputs 0,1 and 8,9 each
  ;; share one divider. Unlike the outputs, the dividers are numbered starting
  ;; at `1'. Thus the outputs 0 and 1 share the divider number 1.
  ;;
  ;; Unfortunately, you can't just set a divider to `4' and expect the divider
  ;; to be set to 4 (or 5 - which would be way more sensible). No, we need to
  ;; look up the right bit settings in a table.
  ;;
  ;; There is something good, still: All dividers are configured in the same
  ;; position in registers 1..8, one divider per register.

  (if (not (divider? div))
      (error-divider div)
      (write-register conn div
                           (set-bits-odiv (read-register conn div) val))))

(define (set-output-mode conn output mode)
  (if (not (output-index? output))
      (error-output-index)
      (write-register conn output
                           (set-bits-output-mode (read-register conn output)
                                                 mode))))

(define (change-output-divider conn with-what div)
  (if (not (divider? div))
      (error-divider div)
      (write-register conn div (with-what (read-register conn div)))))

(define (disable-output-divider conn div)
  (change-output-divider conn clear-odiv-enable-bit div))

(define (enable-output-divider conn div)
  (change-output-divider conn set-odiv-enable-bit div))

(define (change-mn-divider conn with-what value)
  (if (not (mn-divider-value? value))
      (error-mn-divider-value value)
      (let ((mn-div-reg 10))
        (write-register conn mn-div-reg
                             (with-what (read-register conn mn-div-reg)
                                        value)))))

(define (set-n-divider conn value)
  (change-mn-divider conn set-bits-ndiv value))

(define (set-m-divider conn value)
  (change-mn-divider conn set-bits-mdiv value))

(define (set-reference-divider conn type state)
  (cond
   ((not (r-divider? type))
    (error-invalid-r-divider)
    #f)
   ((not (boolean? state))
    (error-not-boolean "r divider state")
    #f)
   (else
    (let ((rdiv-reg 11))
      (write-register conn rdiv-reg
                           (set-bits-rdiv (read-register conn rdiv-reg)
                                          type state))))))

(define (change-power-down-pll conn with-what)
  (let ((pll-pd-reg 11))
    (write-register conn pll-pd-reg
                         (with-what (read-register conn pll-pd-reg)))))

(define (power-down-pll conn)
  (change-power-down-pll conn set-pll-power-down-bit))

(define (power-up-pll conn)
  (change-power-down-pll conn clear-pll-power-down-bit))

(define (change-power-down-device conn with-what)
  (let ((device-pd-reg 10))
    (write-register conn device-pd-reg
                         (with-what (read-register conn device-pd-reg)))))

(define (power-down-device conn)
  (change-power-down-device conn set-device-power-down-bit))

(define (power-up-device conn)
  (change-power-down-device conn clear-device-power-down-bit))

(define (decode-register conn idx)
  (cond
   ((not (register-index? idx))
    (error-invalid-reg-index idx))
   (else
    (decode-register-by-value (read-register conn idx)))))

(define (decode-device conn)
  (let nextreg ((reg (read-registers conn)))
    (cond ((null? reg) (display "Done.\n"))
          (else
           (decode-register-by-value (car reg))
           (nextreg (cdr reg))))))
