;; Copyright 2011-2013 Frank Terbeck <ft@bewatermyfriend.org>, All
;; rights reserved.
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

;; This is the top level cdce72010 manipulation module. It is intended to
;; make changing settings of the device as straight forward as possible. If
;; you prefer the symbols from this module to have a `cdce/' prefix, use:
;;
;;      (use-modules
;;       ((chip-remote devices ti cdce72010)
;;        #:renamer (symbol-prefix-proc 'cdce/)))
;;
;; Note, that the code in this module assumes that the serial connection to
;; the microcontroller is stable. If you need this for unattended control in
;; unstable environments, you may want to use the backend code directly.
;;
;; This code concentrates on sanitising procedure parameters to avoid
;; problems during interactive use. For example, trying to change divider 9
;; will cause an error, because divider 9 does not exist on the device.

(define-module (chip-remote devices ti cdce72010)
  :export (decode-device
           decode-register
           disable-output-divider
           enable-output-divider
           export-registers
           power-down-device
           power-down-pll
           power-up-device
           power-up-pll
           read-registers
           set-feedback-divider
           set-m-divider
           set-n-divider
           set-output-divider
           set-output-mode
           set-reference-divider))

(use-modules (bitops)
             (chip-remote devices ti cdce72010 decode)
             (chip-remote devices ti cdce72010 messages)
             (chip-remote devices ti cdce72010 prg)
             (chip-remote devices ti cdce72010 validate))

(define (read-registers)
  (let ((a '()))
    (let loop ((i 0))
      (set! a (append a (list (cdce/read-register i))))
      (if (< i 12)
          (loop (+ i 1))))
    a))

(define (export-registers-c-array registers)
  (display "/* for `uint32_t' */\n#include <stdint.h>\n\n")
  (display "uint32_t\nregs[] = {\n")
  (let nr ((r registers))
    (cond
     ((null? r) #t)
     (else
      (format #t "    0x~8,'0x" (car r))
      (if (null? (cdr r))
          (newline)
          (display ",\n"))
      (nr (cdr r)))))
  (display "};\n"))

(define (export-registers-scheme-script registers)
  (display "(use-modules ((chip-remote devices ti cdce72010)\n")
  (display "              #:renamer (symbol-prefix-proc 'cdce/)))\n\n")
  (display "(cdce/open \"/dev/ttyUSB0\") ;; or whatever device file...\n")
  (display "(cdce/hi)\n\n")
  (let nr ((i 0)
           (r registers))
    (cond
     ((null? r) #t)
     (else
      (format #t "(cdce/write-register ~2d #x~8,'0x)\n" i (car r))
      (nr (1+ i) (cdr r)))))
  (display "\n(cdce/bye)\n")
  (display "(cdce/close)\n"))

(define (export-registers-scheme-list registers type)
  (display "(define registers\n  '(")
  (let ((fmt (cond
              ((equal? type 'hex)    "#x~8,'0x")
              ((equal? type 'binary) "#b~32,'0b")
              (else "Unknown scheme-list export type ~d"))))
    (let nr ((r registers))
      (cond
       ((null? r)
        (display "))\n")
        #t)
       (else
        (format #t fmt (car r))
        (if (not (null? (cdr r)))
            (display "\n    "))
        (nr (cdr r)))))))

(define (export-registers registers . mode)
  (let ((argc (length mode)))
    (cond
     ((and (not (= argc 1))
           (not (= argc 0)))
      (format #t "usage: (export-registers <register-list> [mode])\n")
      #f)
     (else
      (let ((m (if (= argc 1)
                   (car mode)
                   'c-array)))
        (cond
         ((equal? m 'c-array)
          (export-registers-c-array registers))
         ((equal? m 'scheme-script)
          (export-registers-scheme-script registers))
         ((equal? m 'scheme-list-hex)
          (export-registers-scheme-list registers 'hex))
         ((equal? m 'scheme-list-binary)
          (export-registers-scheme-list registers 'binary))
         (else
          (format #t "Unknown export mode: `~a'\n" (symbol->string m))
          #f)))))))

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

(define (set-output-mode output mode)
  (if (not (output-index? output))
      (error-output-index)
      (cdce/write-register output
                           (set-bits-output-mode
                            (cdce/read-register output)
                            mode))))

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

(define (set-reference-divider type state)
  (cond
   ((not (r-divider? type))
    (error-invalid-r-divider)
    #f)
   ((not (boolean? state))
    (error-not-boolean "r divider state")
    #f)
   (else
    (let ((rdiv-reg 11))
      (cdce/write-register rdiv-reg
                           (set-bits-rdiv
                            (cdce/read-register rdiv-reg)
                            type state))))))

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
  (change-power-down-device set-device-power-down-bit))

(define (power-up-device)
  (change-power-down-device clear-device-power-down-bit))

(define (decode-register idx)
  (cond
   ((not (register-index? idx))
    (error-invalid-reg-index idx))
   (else
    (decode-register-by-value (cdce/read-register idx)))))

(define (decode-device)
  (let nextreg ((reg (read-registers)))
    (cond ((null? reg) (display "Done.\n"))
          (else
           (decode-register-by-value (car reg))
           (nextreg (cdr reg))))))
