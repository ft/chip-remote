;; Copyright (c) 2018 chip-remote workers, All rights reserved.
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

(define (get-width data meta)
  (assq-ref (meta data) #:register-width))

(define (title n rm-addr reg ann act)
  (let* ((meta (register-meta (cdr reg)))
         (addr (car reg))
         (desc (assq-ref meta #:description)))
    (format #f "Register #~a width annotation (~a) matches actual width ~a (address: ~a/~a, description: ~a)"
            n ann act rm-addr addr desc)))

(define (test-register n rm-addr reg annotation)
  (when annotation
    (let ((actual (register-width (cdr reg))))
      (define-test (title n rm-addr reg annotation actual)
        (pass-if-= annotation actual)))))

(define (complete-registers/check dev cfg)
  (let loop-rm ((rms (page-map-table (device-page-map dev)))
                (width-dev (get-width dev device-meta))
                (n 0))
    (unless (null? rms)
      (let loop-reg ((rm-addr (caar rms))
                     (regs (register-map-table (cdar rms)))
                     (width-rm (get-width (cdar rms) register-map-meta))
                     (n n))
        (if (null? regs)
            (loop-rm (cdr rms) width-dev n)
            (begin (test-register n rm-addr (car regs)
                                  (or (get-width (cdar regs) register-meta)
                                      width-rm
                                      width-dev))
                   (loop-reg rm-addr (cdr regs) width-rm (+ 1 n))))))))

(define (maybe-count data meta registers)
  (and (get-width data meta)
       (length (registers data))))

(define (one-up reg) '(one-thing))

(define (count-register reg)
  (or (maybe-count reg register-meta one-up)
      0))

(define (count-register-map rm)
  (or (maybe-count rm register-map-meta register-map-registers)
      (apply + (map count-register (register-map-registers rm)))))

(define (complete-registers/count dev cfg)
  (or (maybe-count dev device-meta device-registers)
      (apply + (map count-register-map
                    (page-map-register-maps (device-page-map dev))))))
