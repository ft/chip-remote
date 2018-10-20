;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote commander)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module ((chip-remote decode) #:prefix cr:)
  #:use-module (chip-remote item)
  #:use-module (chip-remote device)
  #:use-module (chip-remote device access)
  #:use-module (chip-remote device transfer)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote io)
  #:use-module (chip-remote modify)
  #:use-module (chip-remote protocol)
  #:export (make-commander))

(define-record-type <cmdr-state>
  (make-cmdr-state dev con port default data decode)
  cmdr-state?
  (dev get-device)
  (con get-connection)
  (port get-port)
  (default get-default)
  (data get-data set-data!)
  (decode show))

(define (must-be-connected state)
  (let* ((conn (get-connection state))
         (io-port (cr-connection-port conn)))
    (unless (and io-port
                 (port? io-port)
                 (not (port-closed? io-port)))
      (throw 'connection-not-opened conn)))
  #t)

(define (cmdr-command cmd state)
  (case cmd
    ((close!)
     (must-be-connected state)
     (format #t "Closing io...~%")
     (let ((c (get-connection state)))
       (bye c)
       (io-close c)))
    ((data)
     (get-data state))
    ((decode)
     ((show state) (get-device state) (get-data state)))
    ((device)
     (get-device state))
    ((focus!)
     (must-be-connected state)
     (format #t "Focusing port on remote controller~%")
     (focus (get-connection state) (get-port state)))
    ((open!)
     (format #t "Opening io...~%")
     (let ((c (get-connection state)))
       (io-open c)
       (hi* c)))
    ((reset!)
     (set-data! state (get-default state)))
    ((trace!)
     (assq 'trace (io-opt/set 'trace (not (io-opt/get 'trace)))))
    ((transmit!)
     (must-be-connected state)
     (let* ((dev (get-device state))
            (acc (device-access dev))
            (transfer (da-transfer acc))
            (transform (transfer-transform transfer))
            (write-data (da-write acc)))
       (for-each (lambda (datum)
                   (transmit (get-connection state) datum))
                 (map write-data
                      (transform (get-data state))))))

    ;; Unknown commands error out.
    (else (throw 'unknown-simple-command cmd))))

(define (transmit-data c dev data)
  (let ((addr (assq-ref data 'address))
        (part (assq-ref data 'part))
        (value (assq-ref data 'value))
        (write-data (da-write (device-access dev))))
    (transmit c (write-data (first addr) (second addr) value))))

(define (touched-registers dev lst)
  (let loop ((rest lst) (acc '()))
    (if (null? rest)
        acc
        (let ((this (car rest)))
          (loop (cdr rest) (if (member this acc) acc (cons this acc)))))))

(define (cmdr-w/rest cmd args state)
  (case cmd
    ((decode)
     (let ((extr (device-extract (get-device state) (get-data state) args)))
       ((show state) (assq-ref extr 'part) (assq-ref extr 'item))))
    ((change!)
     (must-be-connected state)
     (cmdr-w/rest 'set! args state)
     (let ((dev (get-device state)))
       (for-each
        (lambda (addr)
          (cmdr-w/rest 'transmit! (list addr) state))
        (touched-registers dev
                           (map (compose (lambda (addr)
                                           (take addr 2))
                                         (lambda (addr)
                                           (find-canonical-address dev addr))
                                         car)
                                args)))))
    ((load!)
     (set-data! state (car args)))
    ((set!)
     (set-data! state (apply chain-modify
                             (cons (get-device state)
                                   (cons (get-data state) args)))))
    ((transmit!)
     (must-be-connected state)
     (let ((c (get-connection state))
           (extr (device-extract (get-device state) (get-data state) args)))
       (transmit-data c (get-device state) extr)))

    ;; Unknown commands error out here as well.
    (else (throw 'unknown-complex-command cmd args))))

(define* (make-commander #:key device connection (port 0) data decode)
  (unless (device? device)
    (throw 'cr-missing-data 'device device))
  (unless (cr-connection? connection)
    (throw 'cr-missing-data 'connection connection))
  (let* ((default (or data (device-default device)))
         (state (make-cmdr-state device connection port
                                 default default
                                 (or decode cr:decode))))
    (case-lambda
      (()
       ((show state) (get-device state) (get-data state)))
      ((cmd)
       (cmdr-command cmd state))
      ((cmd . rest)
       (cmdr-w/rest cmd rest state)))))
