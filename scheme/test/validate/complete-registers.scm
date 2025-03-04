;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test validate complete-registers)
  #:use-module (test tap)
  #:use-module (chip-remote device)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:export (complete-registers/count
            complete-registers/check))

;; Chip-remote allows the annotation of device descriptions, to reflect the
;; width of the device's registers. All layers (device, register-map and re-
;; gisters themselves - note that page-maps don't have meta information) may
;; carry such an annotation. The closer the width annotation is to the re-
;; gister, the more weigth it carries (ie the value from a register-map beats
;; out the value from a device definition).
;;
;; Therefore, this module adds one test per register from a device that is
;; touched by at least one width annotation.

(define (title n rm-addr reg ann act)
  (let ((addr (car reg))
        (desc (register-description (cdr reg))))
    (format #f "Register #~a width annotation (~a) matches actual width ~a (address: ~a/~a, description: ~a)"
            n ann act rm-addr addr desc)))

(define (test-register n rm-addr reg annotation)
  (when annotation
    (let ((actual (register-width (cdr reg))))
      (define-test (title n rm-addr reg annotation actual)
        (pass-if-= annotation actual)))))

(define (complete-registers/check dev cfg)
  (let loop-rm ((rms (page-map-table (device-page-map dev)))
                (width-dev (or (page-map-width (device-page-map dev))
                               (device-register-width dev)))
                (n 0))
    (unless (null? rms)
      (let loop-reg ((rm-addr (caar rms))
                     (regs (register-map-table (cdar rms)))
                     (width-rm (register-map-width (cdar rms)))
                     (n n))
        (if (null? regs)
            (loop-rm (cdr rms) width-dev n)
            (begin (test-register n rm-addr (car regs)
                                  (or (register-width* (cdar regs))
                                      width-rm
                                      width-dev))
                   (loop-reg rm-addr (cdr regs) width-rm (+ 1 n))))))))

(define (maybe-count data meta registers)
  (and (meta data)
       (length (registers data))))

(define (one-up reg) '(one-thing))

(define (count-register reg)
  (or (maybe-count reg register-width* one-up)
      0))

(define (count-register-map rm)
  (or (maybe-count rm register-map-width register-map-registers)
      (apply + (map count-register (register-map-registers rm)))))

(define (complete-registers/count dev cfg)
  (or (maybe-count dev device-register-width device-registers)
      (apply + (map count-register-map
                    (page-map-register-maps (device-page-map dev))))))
