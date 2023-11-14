;;; ./tools/guile-in-here ./examples/read-dw3000-saleae-log.scm FILE.csv

(use-modules (ice-9 match)
             (ice-9 control)
             (ice-9 getopt-long)
             (ice-9 pretty-print)
             (rnrs bytevectors)
             (srfi srfi-1)
             ((protocol saleae-spi) #:prefix saleae:)
             (chip-remote bit-operations)
             (chip-remote decode)
             (chip-remote decode to-text)
             (chip-remote device)
             (chip-remote register)
             (chip-remote register-window)
             (chip-remote register-map)
             (chip-remote register-map utilities)
             (chip-remote devices decawave dw3000)
             ((chip-remote devices decawave dw3000 commands) #:prefix dw3000:)
             ((chip-remote devices decawave dw3000 registers) #:prefix dw3000:)
             ((chip-remote devices decawave dw3000 tables) #:prefix dw3000:))

;;; Command line argument handling

(define *name*    'read-dw3000-saleae-log)
(define *version* '((major . 0)
                    (minor . 1)
                    (patch . 0)))

(define (pp-version v)
  (string-join (map (compose number->string cdr) v) "."))

(define option-spec
  '((help        (single-char #\h))
    (colour      (single-char #\C))
    (show-data   (single-char #\d))
    (show-window (single-char #\w))
    (show-spi-ts (single-char #\t))
    (debug       (single-char #\D))
    (verbose     (single-char #\v))
    (version     (single-char #\V))))

(define opts (getopt-long (command-line) option-spec
                          #:stop-at-first-non-option #t))

(define (opt o)
  (option-ref opts o #f))

(define (arguments)
  (opt '()))

(define (with-log-file?)
  (= 1 (length (arguments))))

(define show-data?           (opt 'show-data))
(define show-window?         (opt 'show-window))
(define verbose-decode?      (opt 'verbose))
(define with-spi-timestamps? (opt 'show-spi-ts))
(define with-colour?         (opt 'colour))

(define (usage retval)
  (newline)
  (format #t " Usage:   read-dw3000-saleae-log [OPTION(s)...] LOGFILE.csv~%")
  (newline)
  (format #t "  --help, -h             Print this help text.~%")
  (format #t "  --version, -V          Print version information about the program.~%")
  (format #t "  --colour, -C           Enable colour in frame summaries.~%")
  (format #t "  --debug, -D            Print raw SPI logs after reading file.~%")
  (format #t "  --verbose, -v          Enable colourful verbose decoder.~%")
  (format #t "  --show-data, -d        Show data used for decoding.~%")
  (format #t "  --show-window, -w      Show register window used for decoding.~%")
  (format #t "  --show-spi-ts, -t      Show SPI timestamps in transactions.~%")
  (newline)
  (quit retval))

(when (opt 'version)
  (format #t "~a version ~a~%" *name* (pp-version *version*))
  (quit 0))

(when (opt 'help)
  (usage 0))

(unless (with-log-file?)
  (usage 1))

;;; Utilities

(define (pp obj)
  "Short-hand for pretty-printing values."
  (pretty-print obj #:width 80 #:max-expr-width 100))

(define (start-green-on)
  (display "# ")
  (when with-colour?
    (display #\esc)
    (display "[01;32m")))

(define (start-red-on)
  (display "# ")
  (when with-colour?
    (display #\esc)
    (display "[01;31m")))

(define (colour-off-newline)
  (when with-colour?
    (display #\esc)
    (display "[0m"))
  (newline))

(define (without-time frame)
  (if with-spi-timestamps?
      frame
      (filter (lambda (x) (or (not (pair? x))
                              (not (eq? 'time (car x)))))
              frame)))

;;; Parser and processors for DW3000 SPI transactions.

(define (read-request? o)
  (zero? (logand o #b10000000)))

(define (write-request? o)
  (not (read-request? o)))

(define (short-match mosi mask pattern cmp)
  (and (cmp (length mosi) 1)
         (= (logand (car mosi) mask) pattern)))

(define (long-match mosi mask pattern mode tf)
  (and (> (length mosi) 1)
       (= (logand (car mosi) mask) pattern)
       (tf (zero? (logand (cadr mosi) mode)))))

(define (fast-command? mosi)
  (short-match mosi #b11000001 #b10000001 =))

(define (short-addr-trx? mosi)
  (short-match mosi #b01000001 #b00000000 >))

(define (full-addr-trx? mosi)
  (long-match mosi #b01000000 #b01000000 #b00000011 identity))

(define (masked-write-trx? mosi)
  (long-match mosi #b01000000 #b01000000 #b00000011 not))

(define (get-full-address h0 h1)
  (let ((base-address (bit-extract-width h0 1 5))
        (sub-address (logior 0
                             (ash (logand h0 1) 6)
                             (bit-extract-width h1 2 6))))
    (cons base-address sub-address)))

(define (extract-data mode miso mosi n)
  (bytevector-uint-ref (list->u8vector (cdr (if (eq? mode 'read) miso mosi)))
                       0 'little n))

(define (decode-dw3000 data address width)
  (let* ((base-address (car address))
         (sub-address  (cdr address))
         (offset (* 8 sub-address))
         (register (device-ref dw3000 #f base-address))
         (window (make-register-window register offset width)))
    (when show-window? (format #t "# win: ~a~%" window))
    (if verbose-decode?
        (decode-to-text window data)
        (pp (decode window data)))))

(define (process-fast-command frame)
  (let* ((mosi (assq-ref frame 'mosi))
         (code (bit-extract-width (car mosi) 1 5))
         (cmd (dw3000:short-code->symbol code)))
    (start-green-on)
    (format #t "fast command: ~a [0x~2,'0x] ~a" cmd code (without-time frame))
    (colour-off-newline)))

(define (process-short-addr-trx frame)
  (let* ((mosi (assq-ref frame 'mosi))
         (miso (assq-ref frame 'miso))
         (mode (if (read-request? (car mosi)) 'read 'write))
         (address (cons (bit-extract-width (car mosi) 1 5) 0))
         (data-length (1- (length mosi)))
         (width (* 8 data-length))
         (data (extract-data mode miso mosi data-length)))
    (start-green-on)
    (format #t
            "short addressed transaction (~a 0x~2,'0x:~2,'0x, ~a bits): ~a"
            mode (car address) (cdr address) width (without-time frame))
    (colour-off-newline)
    (when show-data? (format #t "# data ~x~%" data))
    (decode-dw3000 data address width)))

(define (process-full-addr-trx frame)
  (let* ((mosi (assq-ref frame 'mosi))
         (miso (assq-ref frame 'miso))
         (mode (if (read-request? (car mosi)) 'read 'write))
         (address (get-full-address (car mosi) (cadr mosi)))
         (data-length (- (length mosi) 2))
         (width (* 8 data-length))
         (data (extract-data mode miso mosi data-length)))
    (start-green-on)
    (format #t
            "fully addressed transaction (~a 0x~2,'0x:~2,'0x, ~a bits): ~a"
            mode (car address) (cdr address) width (without-time frame))
    (colour-off-newline)
    (when show-data? (format #t "# data ~x~%" data))
    (decode-dw3000 data address width)))

(define (extract-mask mosi offset mask-size)
  (bytevector-uint-ref (list->u8vector (take (drop mosi offset) mask-size))
                       0 'little mask-size))

(define (process-masked-write-trx frame)
  (let* ((mosi (assq-ref frame 'mosi))
         (miso (assq-ref frame 'miso))
         (mode 'write)
         (address (get-full-address (car mosi) (cadr mosi)))
         (mask-size (ash 1 (1- (logand 3 (cadr mosi)))))
         (data-length (- (length mosi) 2)))
    (unless (= (* 2 mask-size) data-length)
      (throw 'dw3000/invalid-masked-write data-length mask-size))
    (let ((and-mask (extract-mask mosi 2 mask-size))
          (or-mask  (extract-mask mosi (+ 2 mask-size) mask-size)))
      (start-green-on)
      (format #t
              "masked write transaction (0x~2,'0x:~2,'0x, size: ~a, and: 0x~v,'0x, or: 0x~v,'0x): ~a"
              (car address) (cdr address) mask-size
              (* 2 mask-size) and-mask
              (* 2 mask-size) or-mask
              (without-time frame))
      (colour-off-newline))))

(define (process-invalid-frame frame)
  (start-red-on)
  (format #t "invalid: ~a" frame)
  (colour-off-newline))

(define (parse-frame raw)
  (when (zero? (length raw))
    (throw 'dw3000-encountered-empty-frame))
  (let ((mosi (assq-ref raw 'mosi)))
    ((cond ((fast-command?     mosi) process-fast-command)
           ((short-addr-trx?   mosi) process-short-addr-trx)
           ((full-addr-trx?    mosi) process-full-addr-trx)
           ((masked-write-trx? mosi) process-masked-write-trx)
           (else                     process-invalid-frame))
     raw)))

;;; Parse Saleae CSV log file into SPI transaction frames and run the DW3000
;;; SPI parser/processor on each frame from the specified log file.

(define saleae-log (car (opt '())))
(define frames (saleae:read-frames #:file saleae-log))
(when (opt 'debug)
  (pp (map without-time frames)))
(for-each parse-frame frames)
