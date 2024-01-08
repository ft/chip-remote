;;; ./tools/guile-in-here ./examples/hello-firmware.scm [OPTION(s)...] CONN

(use-modules (ice-9 getopt-long)
             (ice-9 pretty-print)
             (srfi srfi-1)
             (termios)
             (termios system)
             (protocol ufw-regp)
             (chip-remote protocol))

(define *name* 'hello-firmware)
(define *version* '((major . 0)
                    (minor . 0)
                    (patch . 0)))

(define (pp-version v)
  (string-join (map (compose number->string cdr) v) "."))

;; None of the TCP stuff is implemented, this is an early example.
(define option-spec
  '((help         (single-char #\h))
    (force-serial (single-char #\S))
    (force-tcp    (single-char #\T))
    (port         (single-char #\p) (value #t))
    (version      (single-char #\V))))

(define opts (getopt-long (command-line) option-spec
                          #:stop-at-first-non-option #t))

(define (opt o)
  (option-ref opts o #f))

(define (arguments)
  (opt '()))

(define (with-target?)
  (= 1 (length (arguments))))

(define force-serial? (opt 'force-serial))
(define force-tcp?    (opt 'force-tcp))
(define force-tty?    (opt 'force-tty))

(define (usage retval)
  (newline)
  (format #t " Usage:   hello-firmware [OPTION(s)...] CONNECTION~%")
  (newline)
  (format #t "  --help, -h             Print this help text.~%")
  (format #t "  --version, -V          Print version information about the program.~%")
  (format #t "  --port PORT, -p PORT   Force setting up serial line parameters.~%")
  (format #t "  --force-serial, -S     Force serial behaviour regarless of channel.~%")
  (format #t "  --force-tcp, -T        Force TCP behaviour regarless of channel.~%")
  (newline)
  (quit retval))

(when (opt 'version)
  (format #t "~a version ~a~%" *name* (pp-version *version*))
  (quit 0))

(when (opt 'help)
  (usage 0))

(define (pp obj)
  "Short-hand for pretty-printing values."
  (pretty-print obj #:width 80 #:max-expr-width 100))

(unless (with-target?)
  (usage 1))

(define target (car (arguments)))
(define tty (open-io-file target))
(let ((ts (make-termios-struct)))
  (cf-make-raw! ts)
  (cf-set-speed! ts termios-B921600)
  (tc-set-attr tty ts))
(setvbuf tty 'none)
(define regp-connection (regp:serial-connection tty #:word-size-16? #t))
(define connection (make-cr-connection regp-connection))

(proto-engage! connection)
(pp (cr-static-info connection))
(pp (cr-index connection))
(pp (cr-interfaces connection))
(pp (map (lambda (x) (take x 3)) (cr-access connection)))
(pp (map (lambda (name) (cons name (proto-get-ifc-ctrl! connection name)))
         (proto-interfaces connection)))
(quit 0)
