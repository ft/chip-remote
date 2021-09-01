;; Copyright (c) 2011-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test chip-remote)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-9)
  #:use-module (chip-remote io)
  #:use-module (chip-remote protocol)
  #:use-module (chip-remote utilities)
  #:export (init-connection
            close-connection
            test-with-tag
            native-firmware-built?
            make-test-io
            tio-unknown!
            tio-connection
            set-tio-connection!
            tio-fw-port
            set-tio-fw-port!
            tio-pid
            set-tio-pid!
            tio-terminal
            set-tio-terminal!
            connect-test-io!
            handle-stdin!
            kill-fw!
            boot-fw!
            $))

(define* (init-connection #:key (device (getenv "CR_BOARD_DEVICE")))
  (let ((c (make-cr-connection device)))
    (test-open c)
    (test-hi c)
    c))

(define (close-connection c)
  (test-bye c)
  (test-close c))

(define (test-with-tag tag value)
  (unless value
    (format #t "# FAIL with-tag: ~a~%" tag)
    (quit 1)))

(define (test-open c)
  (unless (io-open c)
    (format #t "# FAIL Establishing serial connection failed.~%")
    (quit 1)))

(define (test-close c)
  (unless (io-close c)
    (format #t "# FAIL Closing serial connection failed.~%")
    (quit 1)))

(define (test-hi c)
  (unless (hi c)
    (format #t "# FAIL Protocol HI failed.~%")
    (quit 1)))

(define (test-bye c)
  (unless (bye c)
    (format #t "# FAIL Protocol BYE failed.~%")
    (quit 1)))

(define-record-type <test-io>
  (make-test-io* fw-port terminal connection pid)
  test-io?
  (fw-port tio-fw-port set-tio-fw-port!)
  (terminal tio-terminal set-tio-terminal!)
  (connection tio-connection set-tio-connection!)
  (pid tio-pid set-tio-pid!))

(define* (make-test-io #:optional terminal)
  (make-test-io* #f terminal #f #f))

(define (connect-test-io! io)
  (set-tio-connection! io (chip-remote-open! #:uri (tio-terminal io))))

(define (handle-xread-timeout tio rv)
  (format #t "# xread timeout from firmware. Giving up!~%")
  (kill-fw! tio)
  (quit 1))

(define (tio-unknown!)
  (format #t "# Could not determine chip-remote terminal. Giving up.~%")
  (quit 1))

(define (handle-stdin! tio)
  (match (xread (tio-fw-port tio)
                #:timeout 5
                #:handle-timeout (lambda (x)
                                   (handle-xread-timeout tio x)))
    (('firmware-pid pid)
     (unless tio
       (kill pid SIGINT)
       (tio-unknown!))
     (format #t "# Registering firmware PID: ~a~%" pid)
     (set-tio-pid! tio pid))
    (x (format #t "# Unhandled firmware message: ~s~%" x))))

(define (kill-fw! tio)
  (let ((pid (tio-pid tio)))
    (if pid
        (begin
          (format #t "# Terminating firmware PID as indicated by itself: ~a~%" pid)
          (kill pid SIGINT)))))

(define ($ tio)
  (tio-connection tio))

(define (native-fw)
  (string-append (getcwd) "/native-fw/zephyr/zephyr.exe"))

(define (native-firmware-built?)
  (file-exists? (native-fw)))

(define *s-exp-boot-tag* "(activated!)")
(define *cr-terminal* "UART_0")

(define (boot-fw! tio)
  (format #t "# Booting native firmware: ~a~%" (native-fw))
  (set-tio-fw-port! tio (open-pipe* OPEN_READ (native-fw)))
  (let loop ((line (read-line (tio-fw-port tio) 'trim)))
    (unless (string= line *s-exp-boot-tag*)
      (let ((lst (string-split line #\space)))
        (when (and (not (null? lst))
                   (string= (car lst) *cr-terminal*))
          (set-tio-terminal! tio (car (reverse lst)))))
      (loop (read-line (tio-fw-port tio) 'trim))))

  (if tio
      (begin
        (format #t "# Chip-Remote Terminal at: ~a~%" (tio-terminal tio))
        (format #t "# S-Expression Interface up.~%")
        (connect-test-io! tio))
      (tio-unknown!))

  ;; The firmware should indicate its PID first thing in s-exp mode.
  (handle-stdin! tio))
