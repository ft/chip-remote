;; Copyright (c) 2011-2025 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test chip-remote)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs io ports)
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
            kill-fw!
            boot-fw!
            debug-fw!
            $
            instrument!
            fw-expect!))

;; The <test-io> type encapsulates information to interact with a native-build
;; firmware build. These channels are available:
;;
;;   - Firmware stdio
;;   - Firmware chip-remote protocol channel
;;   - Firmware instrumentation protocol channel
;;
;; The firmware start up (boot-fw!) has a number of phases:
;;
;;   - Early boot-up: Here the test runner runs the native-build executable,
;;     and connects to its stdio. Then it waits for messages suitable for
;;     fw-expect! until tio-booted? is satisfied.
;;
;;   - When this point is reached, the firmware has its TCP servers up and
;;     running, so the boot process will connect to them and register the
;;     resulting ports with the tio object.
;;
;; After this point the firmware is fully up and running, ready to be tested.
;; It is still advisable to add a plausibility test like this:
;;
;;   (with-fw-test-bundle tio (chip-remote firmware foobar)
;;     (plan …)
;;     (boot-fw! tio)
;;
;;     (define-test "Running proto-engange works"
;;       (pass-if-no-exception (proto-engage! ($ tio))))
;;
;;     …)
;;
;; Now everything is up and running.
(define-record-type <test-io>
  (make-test-io* fw-port terminal instrumentation connection iconnection pid
                 timeout parameters)
  test-io?
  ;; Port to interact with the native-build's stdio.
  (fw-port tio-fw-port set-tio-fw-port!)
  ;; TCP Port for the chip-remote protocol. Read from fw-port at boot-time.
  (terminal tio-terminal set-tio-terminal!)
  ;; TCP Port for the instrumentation channel. Read from fw-port at boot-time.
  (instrumentation tio-instrumentation set-tio-instrumentation!)
  ;; Port to interact with the firmware via the chip-remote protocol.
  (connection tio-connection set-tio-connection!)
  ;; Port to interact with the firmware via the instrumentation side-channel.
  (iconnection tio-iconnection set-tio-iconnection!)
  ;; PID of the firmware. Read from fw-port at boot-time.
  (pid tio-pid set-tio-pid!)
  ;; IO time out parameter for the test-io instance.
  (timeout tio-timeout set-tio-timeout!)
  ;; Miscellaneous parameters for the test-io instance.
  (parameters tio-parameters set-tio-parameters!))

(define (tio-booted? tio)
  "Predicate for all essential boot-up message

Then this returns true, all essential information from tio's fw-port have been
read, and the test-runner can enter its next phase."
  (and (tio-fw-port tio)
       (tio-terminal tio)
       (tio-instrumentation tio)
       (tio-pid tio)))

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

(define (handle-error tio type rv)
  (format #t "# ~a from firmware. Giving up!~%" type)
  (kill-fw! tio)
  (quit rv))

(define (trace-read sel tio tag . args)
  (let ((exp (apply xread (cons (sel tio) args))))
    (when (tio-got-param? tio 'trace?)
      (format #t "# tio:~a:read: ~s~%" tag exp))
    exp))

(define (flush-stdin! tio)
  (when (has-data? (tio-fw-port tio))
    (let loop ((input (read-line (tio-fw-port tio) 'trim)))
      (unless (eof-object? input)
        (when (tio-got-param? tio 'trace?)
          (format #t "# tio:stdio:read: ~s~%" input))
        (when (has-data? (tio-fw-port tio))
          (loop (read-line (tio-fw-port tio) 'trim)))))))

(define (kill-fw! tio)
  (let ((pid (tio-pid tio)))
    (if (tio-got-param? tio 'dont-kill)
        (format #t "# Not terminating firmware (PID: ~a), as requested.~%" pid)
        (if pid
            (begin
              (format #t "# Terminating firmware PID: ~a~%" pid)
              (kill pid SIGINT))
            (format #t "# kill-fw!: Firmware PID unknown!~%")))))

(define ($ tio)
  "Shorthand for tio-connection, which is very commonly used."
  (tio-connection tio))

(define (native-fw)
  (string-append (getcwd) "/native-fw/zephyr/zephyr.exe"))

(define (native-firmware-built?)
  (file-exists? (native-fw)))

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
;;   (fw-expect! tio 'spi-text #:expect '((spi-tx #x23) (spi-rx #x42)))
;;
;; This will wait for two messages tagged "spi-text", and register tests for
;; their s-expressions being equal to the two additional parameters passed. If
;; one of the parameters satisfies procedure?, it is assumed to work as a pre-
;; dicate. The read s-expression is fed into it and the result is used in a
;; test in terms of pass-if-true. If LST is empty, it will wait for one message
;; tagged "spi-text" and return its s-expression, without registering a test.
;; This can be used to perform arbitrary actions on such messages, not just
;; perform tests in terms of (test tap).
(define* (fw-expect! tio tag #:key unhandled other (expect '()))
  (define attroff "\x1b[0m")
  (define strprefix (string-append attroff (symbol->string tag) ": "))

  (define (tag-split s)
    (call/ec (lambda (return)
               (let ((colon (string-index s #\:)))
                 (unless colon (return #f))
                 (let ((tag (substring s (string-length attroff) colon))
                       (rest (string-trim (substring s (1+ colon)))))
                   (list tag (string->sexp rest)))))))

  (define (read-tag)
    (flush-all-ports)
    (let loop ()
      (let ((data (read-line/timeout (tio-fw-port tio) (tio-timeout tio))))
        (when (and data (tio-got-param? tio 'trace?))
          (format #t "# tio:stdio:read: ~s~%" data))
        (cond ((not data) (throw 'fw-expect-timeout tio tag))
              ((eof-object? data) (throw 'fw-expect-eof tio tag))
              ((string-prefix? strprefix data)
               (string->sexp (substring data (string-length strprefix))))
              (else
               (let ((parts (and unhandled
                                 (string-prefix? attroff data)
                                 (tag-split data))))
                 (cond (parts (apply unhandled parts))
                       (other (other data))))
               (loop))))))

  (if (null? expect)
      (read-tag)
      (let loop ((rest expect))
        (unless (null? rest)
          (let ((this (car rest)))
            (if (procedure? this)
                (define-test (format #f "firmware expect (~a): ~a" tag this)
                  (pass-if-true (this (read-tag))))
                (define-test (format #f "firmware expect (~a): ~a" tag this)
                  (pass-if-equal? this (read-tag)))))
          (loop (cdr rest))))))

(define* (fw-expect/internal tio tag #:key unhandled other (expect '()))
  (catch #t
    (lambda () (fw-expect! tio tag
                           #:unhandled unhandled
                           #:other other
                           #:expect expect))
    (lambda (k . a)
      (cond ((eq? k 'fw-expect-timeout) (handle-error tio k 1))
            ((eq? k 'fw-expect-eof)     (handle-error tio k 1))
            (else (apply throw k a))))))

(define* (boot-fw! tio #:key (suspend-execution? #t))
  (define shell #f)

  (define (find-shell line)
    (unless shell
      (let ((lst (string-split line #\space)))
        (when (and (not (null? lst))
                   (string= (car lst) *cr-shell*))
          (set! shell (car (reverse lst)))))))

  (define (boot-fw/phase-one)
    (let loop ()
      (let ((data (fw-expect/internal tio 'cr-init #:other find-shell)))
        (match data
          (('activated!) (format #t "# Firmware boot-up tag recognised.\n"))
          (('board board) (format #t "# Firmware platform is ~a.~%" board))
          (('firmware-pid pid)
           (let ((realpid (tio-pid tio)))
             (if (= realpid pid)
                 (format #t "# Firmware reports consistent PID (~a).~%" pid)
                 (format #t "# Firmware reports inconsistent PID (~a vs ~a).~%"
                         pid realpid))))
          (('cr-server-port port)
           (format #t "# Firmware chip-remote port: ~a~%" port)
           (set-tio-terminal! tio port))
          (('ni-server-port port)
           (format #t "# Firmware instrumentation port: ~a~%" port)
           (set-tio-instrumentation! tio port))
          (_ (format #t "# Unhandled boot-up message: ~s~%" data)))
        (unless (tio-booted? tio)
          (loop)))))

  ;; This should work, but it doesn't sometimes. Oh no!
  ;;
  ;; This somehow has to do with the value for #:input, which seems to
  ;; something like:
  ;;
  ;;   #:input #<input: custom-port 7efe9e0a23f0>
  ;;
  ;; …which turns into…
  ;;
  ;;   #<input: string 7efe9e0a23f0>
  ;;
  ;; When I run this in tap-harness, which also uses "spawn" to run this. When
  ;; I run it just in guile, I get #<input: file /dev/pts/16> and then things
  ;; are fine.
  ;;
  ;; From within tap-harness, that gets me:
  ;;
  ;;   In procedure spawn: Wrong type argument in position 3 (expecting
  ;;     open file port): #<input: string 7efe9e0a23f0>
  ;;
  ;; …in guile 3.0.10. In 3.0.9 this just segfaults. Meh. I don't have the
  ;; energy to debug this today. There is still a bunch of experiments down
  ;; there.
  ;;
  ;; (let ((fw (native-fw)))
  ;;   (format #t "# Booting native firmware: ~a~%" fw)
  ;;   (flush-all-ports)
  ;;   (let* ((input&output (pipe))
  ;;          (pid (begin
  ;;                 (format #t "# DEBUG: ~a ~a ~a ~a~%"
  ;;                         (current-input-port)
  ;;                         (current-output-port)
  ;;                         (current-error-port)
  ;;                         (port-mode (current-input-port)))
  ;;                 (spawn fw (list fw "-no-color")
  ;;                        #:search-path? #f
  ;;                        #:input (%make-void-port "r")
  ;;                        #:error (cdr input&output)
  ;;                        #:output (cdr input&output)))))
  ;;     (close-port (cdr input&output))
  ;;     (format #t "# Firmware PID: ~a~%" pid)
  ;;     (flush-all-ports)
  ;;     (set-tio-fw-port! tio (car input&output))
  ;;     (set-tio-pid! tio pid)))

  ;; This is a hacky alternative, using an internal function of the popen
  ;; module. Not cool.
  (let* ((p (open-pipe* OPEN_READ (native-fw) "-no-color"))
         (pp (%port-property p 'popen-pipe-info))
         (pid ((@ (ice-9 popen) pipe-info-pid) pp)))
    (format #t "# Firmware started with PID ~a~%" pid)
    (set-tio-fw-port! tio p)
    (set-tio-pid! tio pid))

  (boot-fw/phase-one)

  (when shell
    (format #t "# Firmware shell at: ~a~%" shell))

  (format #t "# Connecting to native-sim build of firmware...~%")
  (connect-test-io! tio)
  (regp:change-param (cr-low-level (tio-connection tio))
                     'trace? (tio-got-param? tio 'trace?))

  (format #t "# Connection established.~%")
  (when (and (> (length (command-line)) 1)
             (string= "--debug" (cadr (command-line))))
    (set-tio-timeout! tio #f)
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
