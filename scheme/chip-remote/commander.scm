;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote commander)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-9)
  #:use-module (chip-remote decode)
  #:use-module (chip-remote device)
  #:use-module (chip-remote io)
  #:use-module (chip-remote modify)
  #:use-module (chip-remote protocol)
  #:export (make-commander))

(define-record-type <cmdr-state>
  (make-cmdr-state dev con port data decode)
  cmdr-state?
  (dev get-device)
  (con get-connection)
  (port get-port)
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
    ((decode)
     ((show state) (get-device state) (get-data state)))
    ((focus!)
     (must-be-connected state)
     (format #t "Focusing port on remote controller~%")
     (focus (get-connection state) (get-port state)))
    ((open!)
     (format #t "Opening io...~%")
     (let ((c (get-connection state)))
       (io-open c)
       (hi* c)))
    ((trace!)
     (assq 'trace (io-opt/set 'trace (not (io-opt/get 'trace)))))
    ((transmit!)
     (must-be-connected state)
     (format #t "Transmitting all~%"))

    ;; Unknown commands error out.
    (else (throw 'unknown-simple-command cmd))))

(define (cmdr-w/rest cmd args state)
  (case cmd
    ((decode)
     (format #t "Decode with address~%"))
    ((change!)
     (must-be-connected state)
     (format #t "Changing stuff~%"))
    ((set!)
     (set-data! state (apply chain-modify
                             (cons (get-device state)
                                   (cons (get-data state) args)))))
    ((transmit!)
     (must-be-connected state)
     (format #t "Transmitting stuff~%"))

    ;; Unknown commands error out here as well.
    (else (throw 'unknown-complex-command cmd args))))

(define* (make-commander #:key device connection (port 0) data)
  (unless (device? device)
    (throw 'cr-missing-data 'device device))
  (unless (cr-connection? connection)
    (throw 'cr-missing-data 'connection connection))
  (let ((state (make-cmdr-state device connection port
                                (or data (device-default device))
                                decode)))
    (case-lambda
      (()
       ((show state) (get-device state) (get-data state)))
      ((cmd)
       (cmdr-command cmd state))
      ((cmd . rest)
       (cmdr-w/rest cmd rest state)))))
