;; Copyright (c) 2011-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test chip-remote)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-9)
  #:use-module (test tap)
  #:use-module (chip-remote protocol)
  #:use-module (chip-remote utilities)
  #:use-module (protocol ufw-regp)
  #:export (with-fw-test-bundle
            native-firmware-built?
            make-test-io
            tio-connection
            tio-iconnection
            tio-instrumentation
            tio-parameters
            tio-timeout
            set-tio-connection!
            tio-fw-port
            set-tio-fw-port!
            tio-pid
            tio-push-parm!
            set-tio-pid!
            tio-terminal
            set-tio-terminal!
            set-tio-instrumentation!
            set-tio-parameters!
            set-tio-timeout!
            connect-test-io!
            flush-stdin!
            handle-stdin!
            kill-fw!
            boot-fw!
            debug-fw!
            $
            instrument!
            fw-expect!))

(define-record-type <test-io>
  (make-test-io* fw-port terminal instrumentation connection iconnection pid
                 timeout parameters)
  test-io?
  (fw-port tio-fw-port set-tio-fw-port!)
  (terminal tio-terminal set-tio-terminal!)
  (instrumentation tio-instrumentation set-tio-instrumentation!)
  (connection tio-connection set-tio-connection!)
  (iconnection tio-iconnection set-tio-iconnection!)
  (pid tio-pid set-tio-pid!)
  (timeout tio-timeout set-tio-timeout!)
  (parameters tio-parameters set-tio-parameters!))

(define* (make-test-io #:key terminal instrumentation (timeout 2))
  (make-test-io* #f terminal instrumentation #f #f #f timeout '()))

(define (tio-push-parm! tio p)
  (set-tio-parameters! tio (cons p (tio-parameters tio))))

(define (tio-got-param? tio p)
  (!! (member p (tio-parameters tio))))

(define (connect-test-io! io)
  (set-tio-connection! io (make-cr-connection! #:tcp "127.0.0.1"
                                               #:port (tio-terminal io)))
  (set-tio-iconnection! io
                        (let ((sock (socket PF_INET SOCK_STREAM 0)))
                          (connect sock AF_INET (inet-pton AF_INET "127.0.0.1")
                                   (tio-instrumentation io))
                          sock)))

(define (handle-xread-timeout tio rv)
  (format #t "# xread timeout from firmware. Giving up!~%")
  (kill-fw! tio)
  (quit 1))

(define (trace-read sel tio tag . args)
  (let ((exp (apply xread (cons (sel tio) args))))
    (when (tio-got-param? tio 'trace?)
      (format #t "# tio:~a:read: ~s~%" tag exp))
    exp))

(define (init-done? tio)
  (and (tio-terminal tio)
       (tio-instrumentation tio)))

(define (flush-stdin! tio)
  (when (has-data? (tio-fw-port tio))
    (let loop ((input (read-line (tio-fw-port tio) 'trim)))
      (unless (eof-object? input)
        (when (tio-got-param? tio 'trace?)
          (format #t "# tio:inst:read: ~s~%" input))
        (when (has-data? (tio-fw-port tio))
          (loop (read-line (tio-fw-port tio) 'trim)))))))

(define (handle-stdin! tio)
  (match (trace-read tio-fw-port tio 'inst
                     #:timeout (tio-timeout tio)
                     #:handle-timeout (lambda (x)
                                        (handle-xread-timeout tio x)))
    (('booted!) #f)
    (('cr-server-port port)
     (format #t "# Chip Remote Server Port: ~a~%" port)
     (set-tio-terminal! tio port))
    (('ni-server-port port)
     (format #t "# Chip Remote Instrumentation Port: ~a~%" port)
     (set-tio-instrumentation! tio port))
    (('firmware-pid pid)
     (format #t "# Registering firmware PID: ~a~%" pid)
     (set-tio-pid! tio pid))
    (x (format #t "# Unhandled firmware message: ~s~%" x)))
  (init-done? tio))

(define (kill-fw! tio)
  (let ((pid (tio-pid tio)))
    (if (tio-got-param? tio 'dont-kill)
        (format #t "# Not terminating firmware (PID: ~a), as requested.~%" pid)
        (when pid
          (format #t "# Terminating firmware PID as indicated by itself: ~a~%" pid)
          (kill pid SIGINT)))))

(define ($ tio)
  (tio-connection tio))

(define (native-fw)
  (string-append (getcwd) "/native-fw/zephyr/zephyr.exe"))

(define (native-firmware-built?)
  (file-exists? (native-fw)))

(define *s-exp-boot-tag* "(activated!)")
(define *cr-shell* "uart")

(define (debug-fw! tio)
  (format #t "# Debug Mode (PID: ~a): Press ENTER to continue!"
          (tio-pid tio))
  (force-output (current-output-port))
  (read-line))

;; Output that is interesting for chip-remote on the stdout of a Zephyr native-
;; build binary starts with "\x1b[0m", which is an ANSI terminal code for "turn
;; off all attributes", followed by a tag (a limited symbol), followed by a co-
;; lon character and space characters. Finally a single s-expression carrying
;; arbitrary information will follow.
;;
;; This function can be used to await such messages.
;;
;;   (fw-expact! tio 'spi-text '(spi-tx #x23) '(spi-rx #x42))
;;
;; This will wait for two messages tagged "spi-text", and register tests for
;; their s-expressions being equal to the two additional parameters passed. If
;; one of the parameters satisfies procedure?, it is assumed to work as a pre-
;; dicate. The read s-expression is fed into it and the result is used in a
;; test in terms of pass-if-true. If LST is empty, it will wait for one message
;; tagged "spi-text" and return its s-expression, without registering a test.
;; This can be used to perform arbitrary actions on such messages, not just
;; perform tests in terms of (test tap).
(define (fw-expect! tio tag . lst)
  (define (read-tag)
    (let loop ((strprefix (string-append "\x1b[0m"
                                         (symbol->string tag)
                                         ": ")))
      (let ((data (read-line/timeout (tio-fw-port tio) (tio-timeout tio))))
        (cond ((not data) (throw 'fw-expect-timeout tio tag))
              ((eof-object? data) (throw 'fw-expect-eof tio tag))
              ((string-prefix? strprefix data)
               (string->sexp (substring data (string-length strprefix))))
              (else (loop strprefix))))))

  (if (null? lst)
      (read-tag)
      (let loop ((rest lst))
        (unless (null? rest)
          (let ((this (car rest)))
            (if (procedure? this)
                (define-test (format #f "firmware expect (~a): ~a" tag this)
                  (pass-if-true (this (read-tag))))
                (define-test (format #f "firmware expect (~a): ~a" tag this)
                  (pass-if-equal? this (read-tag)))))
          (loop (cdr rest))))))

(define* (boot-fw! tio #:key (suspend-execution? #t))
  (define shell #f)
  (format #t "# Booting native firmware: ~a~%" (native-fw))
  (set-tio-fw-port! tio (open-pipe* OPEN_READ (native-fw) "-no-color"))
  (let loop ((line (read-line (tio-fw-port tio) 'trim)))
    (when (tio-got-param? tio 'trace?)
      (format #t "# tio:inst:read: ~s~%" line))
    (unless (string= line *s-exp-boot-tag*)
      (let ((lst (string-split line #\space)))
        (cond ((and (not (null? lst))
                    (string= (car lst) *cr-shell*))
               (set! shell (car (reverse lst))))))
      (loop (read-line (tio-fw-port tio) 'trim))))

  (when shell
    (format #t "# Firmware shell at: ~a~%" shell))
  (format #t "# S-Expression Interface up.~%")

  (format #t "# Gathering connectivity information...~%")
  (let loop ()
    ;; This will block operation until all vital information from the
    ;; native-sim firmware was collected. If this fails to arrive, the
    ;; underlying read operation will time out, and the run will be
    ;; aborted.
    (unless (handle-stdin! tio) (loop)))

  (format #t "# Connecting to native-sim build of firmware...~%")
  (connect-test-io! tio)
  (regp:change-param (cr-low-level (tio-connection tio))
                     'trace? (tio-got-param? tio 'trace?))

  (format #t "# Connection established.~%")
  (when (and (> (length (command-line)) 1)
             (string= "--debug" (cadr (command-line))))
    (set-tio-timeout! tio #f)
    ;;(io-opt/set 'serial-timeout #f)
    (tio-push-parm! tio 'dont-kill)
    (when suspend-execution? (debug-fw! tio))))

(define (instrument! tio exp)
  (when (tio-got-param? tio 'trace?)
    (format #t "# tio:inst:write: ~s~%" exp))
  (format (tio-iconnection tio) "~a~%" exp)
  ;; The instrumentation request needs to return an ‘ok’ on the instrumentation
  ;; port. This is done after all processing of the request.
  (match (trace-read tio-iconnection tio 'inst #:timeout (tio-timeout tio))
    ('ok #t)
    (reply (throw 'expected-ok-from-instrumentation reply))))

;; The "with-fw-test-bundle" macro is a wrapper around with-test-bundle, which
;; does a couple of jobs for convenience:
;;
;;   - A (require (native-firmware-built?)) expression is added at the start.
;;   - …looks for a call to "boot-fw!", after which every expression will be
;;     followed up by (flush-stdin! tio).
;;   - A (kill-fw! tio) expression is added at the very end.
;;
;; The macro is meant to be used in the "system" tests of the test-suite. This
;; should ensure that test firmwares to not block because they can't write
;; stdout anymore, because it will be drained regularly.

(define-syntax with-fw-test-bundle/scan-for-boot
  (lambda (ctx)
    (define (is-build-fw? syn)
      (let ((expr (syntax->datum syn)))
        (and (list? expr)
             (not (null? expr))
             (eq? 'boot-fw! (car expr)))))
    (syntax-case ctx ()
      ((_ io) #'(kill-fw! io))
      ((_ io exp rest ...)
       (if (is-build-fw? #'exp)
           #'(with-fw-test-bundle/post-flush io exp rest ...)
           #'(begin exp
                    (with-fw-test-bundle/scan-for-boot io rest ...)))))))

(define-syntax with-fw-test-bundle/post-flush
  (lambda (ctx)
    (syntax-case ctx ()
      ((_ io) #'(kill-fw! io))
      ((_ io exp rest ...)
       #'(begin exp
                (flush-stdin! io)
                (flush-all-ports)
                (with-fw-test-bundle/post-flush io rest ...))))))

(define-syntax with-fw-test-bundle
  (lambda (ctx)
    (syntax-case ctx ()
      ((_ io bundle exp ...)
       #'(let ((_io io))
           (with-test-bundle bundle
             (require (native-firmware-built?))
             (with-fw-test-bundle/scan-for-boot _io exp ...)))))))
