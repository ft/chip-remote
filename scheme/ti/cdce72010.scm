;;;;
;; Copyright 2011 Frank Terbeck <ft@bewatermyfriend.org>, All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
;; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; AUTHOR OR CONTRIBUTORS OF THE PROJECT BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

; This is the level cdce72010 manipulation module. It is intended to make
; changing settings of the device as straight forward as possible. If you
; prefer the symbols from this module to have a `cdce/' prefix, use:
;
;      (use-modules
;       ((ti cdce72010)
;        #:renamer (symbol-prefix-proc 'cdce/)))

(define-module (ti cdce72010)
  :export (disable-output-divider
           enable-output-divider
           power-down-device
           power-down-pll
           power-up-device
           power-up-pll
           read-registers
           set-feedback-divider
           set-m-divider
           set-n-divider
           set-output-divider
           toggle-trace))

(use-modules (bitops)
             (ti cdce-primitives)
             (ti cdce72010-messages)
             (ti cdce72010-prg)
             (ti cdce72010-validate))

(define (read-registers)
  (let ((a '()))
    (let loop ((i 0))
      (set! a (append a (list (cdce/read-register i))))
      (if (< i 12)
          (loop (+ i 1))))
    a))

(define (toggle-trace)
  (display "-!- ")
  (cond
   (cdce/options:trace
    (display "Dis")
    (set! cdce/options:trace #f))
   (else
    (display "En")
    (set! cdce/options:trace #t)))
  (display "abling serial communication trace.")
  (newline))

(define (set-feedback-divider val)
  (let ((fbdivreg 11))
    (cdce/write-register fbdivreg (set-bits-fbdiv
                                   (cdce/read-register fbdivreg)
                                   val))))

(define (set-output-divider div val)
  ;; There are ten outputs but only eight dividers. Outputs 0,1 and 8,9 each
  ;; share one divider. Unlike the outputs, the dividers are numbered starting
  ;; at `1'. Thus the outputs 0 and 1 share the divider number 1.
  ;;
  ;; Unfortunately, you can't just set a divider to `4' and expect the divider
  ;; to be set to 4 (or 5 - which would be way more sensible). No, we need to
  ;; look up the right bit settings in a table.
  ;;
  ;; There is something good, still: All dividers are configured in the same
  ;; position in registers 1..8, one divider per register.

  (if (not (divider? div))
      (error-divider div)
      (cdce/write-register div
                           (set-bits-odiv
                            (cdce/read-register div)
                            val))))

(define (change-output-divider with-what div)
  (if (not (divider? div))
      (error-divider div)
      (cdce/write-register div (with-what (cdce/read-register div)))))

(define (disable-output-divider div)
  (change-output-divider clear-odiv-enable-bit div))

(define (enable-output-divider div)
  (change-output-divider set-odiv-enable-bit div))

(define (change-mn-divider with-what value)
  (if (not (mn-divider-value? value))
      (error-mn-divider-value value)
      (let ((mn-div-reg 10))
        (cdce/write-register mn-div-reg
                             (with-what (cdce/read-register mn-div-reg)
                                        value)))))

(define (set-n-divider value)
  (change-mn-divider set-bits-ndiv value))

(define (set-m-divider value)
  (change-mn-divider set-bits-mdiv value))

(define (change-power-down-pll with-what)
  (let ((pll-pd-reg 11))
    (cdce/write-register pll-pd-reg
                         (with-what (cdce/read-register pll-pd-reg)))))

(define (power-down-pll)
  (change-power-down-pll set-pll-power-down-bit))

(define (power-up-pll)
  (change-power-down-pll clear-pll-power-down-bit))

(define (change-power-down-device with-what)
  (let ((device-pd-reg 10))
    (cdce/write-register device-pd-reg
                         (with-what (cdce/read-register device-pd-reg)))))

(define (power-down-device)
  (change-power-down-device clear-device-power-down-bit))

(define (power-up-device)
  (change-power-down-device set-device-power-down-bit))
