;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote frontend)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (chip-remote device)
  #:use-module (chip-remote device access)
  #:use-module (chip-remote modify)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote utilities)
  #:export (cr:reset
            cr:load
            cr:set
            cr:push!
            cr:pull!
            cr:read-registers!
            cr:change!
            cr:setup-port!))

;;; Local utilities

(define (drop-dupes lst)
  (fold (lambda (e acc)
          (if (member e acc)
              acc
              (cons e acc)))
        '()
        lst))

(define (read-register-values c d lst)
  (let ((read-reg (da-read (device-access d))))
    (map-in-order (lambda (a) (cons a (match a ((p r) (read-reg c p r)))))
                  (sort lst addr<))))

(define (replace-register-values d lst)
  (fold (lambda (entry previous)
          (match entry ((a . v) (replace-register-value d previous a v))))
        (current-device-state d)
        lst))

(define (pr-addr p rs)
  (if (null? rs)
      rs
      (cons (list p (car rs))
            (pr-addr p (cdr rs)))))

(define (all-device-addresses d)
  (concatenate (match (device-address-map d)
                 (((pas . rass) ...) (map pr-addr pas rass)))))

(define (device-register-value-for-each f d v)
  (for-each (lambda (p pv)
              (let ((pa (car p))
                    (regs (register-map-table (cdr p))))
                (for-each (lambda (r rv)
                            (let ((ra (car r)))
                              (f pa ra rv)))
                          regs
                          pv)))
            (page-map-table (device-page-map d))
            v))

(define (run-modify-script! c d state script)
  (let* ((mini (minimise-modify-script script))
         (lst (values-for-minimised-script d mini state))
         (reg-write (da-write (device-access d))))
    (for-each (lambda (av)
                (match av
                  ((p r v) (reg-write c p r v))))
              (merge-minimised-script mini lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API below                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cr:reset d)
  "Load the device's default value."
  (push-device-state d 'reset (device-default d)))

(define (cr:load d v)
  "Load V as the current device value for D.

Returns a new device that reflects this change."
  (unless (device-value-suitable? d v)
    (throw 'value-not-suitable-for-device (device-name d) v))
  (push-device-state d 'reset v))

(define (cr:set d . kv)
  "Set elements to values specified by KV in device D.

This function returns an updated device."
  (let* ((state (current-device-state d))
         (next (apply chain-modify (cons* d state kv))))
    (push-device-state d 'set next)))

(define (cr:push! c d)
  "Transfer the complete register table into device D via connection C.

This returns a new device, with an updated device value, reflecting the
register updated register table value."
  (let* ((state (current-device-state d))
         (write-reg (da-write (device-access d)))
         (tx (lambda (p r v) (write-reg c p r v))))
    (device-register-value-for-each tx d state)
    (push-device-state d 'push state)))

(define (cr:pull! c d)
  "Read the complete register table for device D via connection C.

This returns a new device, with an updated device value, reflecting the
register updated register table value."
  (let* ((addresses (all-device-addresses d))
         (av (read-register-values c d addresses))
         (next (replace-register-values d av)))
    (push-device-state d 'pull next)))

(define (cr:read-registers! c d . regref)
  "Read register addressed by the REGREF list for device D via connection C.

This returns a new device, with an updated device value, reflecting the
register value that was just read. Note that if REGREF refers to a combination
value, that the system may update more than one register value."
  (let* ((faddr (lambda (x) (find-canonical-address d x)))
         (p+r (lambda (a) (take a 2)))
         (addresses (drop-dupes (map (compose p+r faddr) regref)))
         (av (read-register-values c d addresses)))
    (replace-register-values d av)))

(define (cr:change! c d . kv)
  "Similar to cr:set, but as a side-effect transfers the required changes to a
connected device via connection C.

This function returns an updated device."
  (let* ((state (current-device-state d))
         (script (apply chain-modify-script (cons d kv)))
         (next (apply-modify-script d state script)))
    (run-modify-script! c d state script)
    (push-device-state d 'change next)))

(define (cr:setup-port! c n d)
  "Setup port N via connection C to accommodate device D.

It does not setup a port's mode, since that may well not be configurable for a
given port. To configure a port for a SPI device, make sure the port at index N
is a SPI port."
  (let* ((d:access (device-access d))
         (d:bus (da-bus d:access))
         (setup (access-bus->proc d:bus)))
    (setup c n)))
