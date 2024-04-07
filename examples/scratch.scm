(use-modules (ice-9 match)
             (ice-9 control)
             (ice-9 pretty-print)
             (rnrs bytevectors)
             (rnrs bytevectors gnu)
             ((scheme base) #:select (bytevector-append))
             (srfi srfi-1)
             (termios)
             (termios system)
             (data-structures loadable-fifo)
             (data-structures sized-stack)
             ((protocol saleae-spi) #:prefix saleae:)
             (protocol ufw-regp)
             (chip-remote bit-operations)
             (chip-remote codecs)
             (chip-remote combination)
             (chip-remote commander)
             (chip-remote decode)
             (chip-remote decode to-c-values)
             (chip-remote decode to-text)
             (chip-remote decode types)
             (chip-remote device)
             (chip-remote device access)
             (chip-remote frontend)
             (chip-remote interact)
             (chip-remote interpreter)
             (chip-remote item)
             (chip-remote item access)
             (chip-remote item builder)
             (chip-remote manufacturer)
             (chip-remote manufacturer analog-devices)
             (chip-remote manufacturer bosch)
             (chip-remote manufacturer decawave)
             (chip-remote manufacturer invensense)
             (chip-remote manufacturer linear-technology)
             (chip-remote manufacturer microchip)
             (chip-remote manufacturer texas-instruments)
             (chip-remote modify)
             (chip-remote named-value)
             (chip-remote page-map)
             (chip-remote protocol)
             (chip-remote register)
             (chip-remote register common)
             (chip-remote register modifiers)
             (chip-remote register predicates)
             (chip-remote register-window)
             (chip-remote register-map)
             (chip-remote register-map utilities)
             (chip-remote semantics)
             (chip-remote simplify)
             (chip-remote type-operations)
             ;;(chip-remote units)
             (chip-remote utilities)
             ;; Analog Devices
             (chip-remote devices analog-devices adf4158)
             ((chip-remote devices analog-devices adf4158 registers) #:prefix adf4158:)
             ((chip-remote devices analog-devices adf4158 tables) #:prefix adf4158:)
             (chip-remote devices analog-devices adf4169)
             ((chip-remote devices analog-devices adf4169 registers) #:prefix adf4169:)
             ((chip-remote devices analog-devices adf4169 tables) #:prefix adf4169:)
             (chip-remote devices analog-devices ad9262)
             ((chip-remote devices analog-devices ad9262 registers) #:prefix ad9262:)
             ((chip-remote devices analog-devices ad9262 tables) #:prefix ad9262:)
             ;; Bosch
             (chip-remote devices bosch bme280)
             ((chip-remote devices bosch bme280 registers) #:prefix bme280:)
             ((chip-remote devices bosch bme280 tables) #:prefix bme280:)
             (chip-remote devices bosch bno055)
             ;; Decawave
             (chip-remote devices decawave dw1000)
             ((chip-remote devices decawave dw1000 registers) #:prefix dw1000:)
             ((chip-remote devices decawave dw1000 tables) #:prefix dw1000:)
             (chip-remote devices decawave dw3000)
             ((chip-remote devices decawave dw3000 commands) #:prefix dw3000:)
             ((chip-remote devices decawave dw3000 registers) #:prefix dw3000:)
             ((chip-remote devices decawave dw3000 tables) #:prefix dw3000:)
             ;; Invensense
             (chip-remote devices invensense icm-20602)
             ;; Linear Technology
             (chip-remote devices linear-technology ltc6603)
             ;; Microchip
             (chip-remote devices microchip mcp4351)
             ((chip-remote devices microchip mcp4351 registers) #:prefix mcp4351:)
             ;; Texas Instruments
             (chip-remote devices texas-instruments ads4149)
             (chip-remote devices texas-instruments cdce72010)
             (chip-remote devices texas-instruments lmh6517))

;; Set up a serial connection if CR_SERIAL_DEVICE is set in the execution
;; environment. ‘s’ will be the name of the device, ‘cr’ is the associated
;; chip-remote connection object and ‘c’ will be the underlying ufw-regp
;; connection object.

(define s (getenv "CR_SERIAL_DEVICE"))
(define c #f)
(define cr #f)
(define tty #f)

(when s
  (set! cr  (make-cr-connection! #:serial s))
  (set! c   (cr-low-level cr))
  (set! tty (regp:port c)))

(define (ignore x) (if #f #t))
(define (pp obj) (pretty-print obj #:width 80 #:max-expr-width 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when cr
  (proto-engage! cr))
