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

(define-module (chip-remote devices ti cdce72010 decode)
  :export (decode-register-by-value
           decode-specific-register-by-value
           fuzz-register
           pll-lock?
           signal-exists?))

(use-modules (ice-9 format)
             (bitops)
             (chip-remote devices ti cdce72010 messages)
             (chip-remote devices ti cdce72010 tables)
             (chip-remote devices ti cdce72010 validate))

(define (pll-lock? regval)
  (logbit? 10 regval))

(define (signal-exists? which regval)
  (cond
   ((not (symbol? which))
    (display "Signal-type needs to be a symbol.\n"))
   ((equal? which 'primary-reference)
    (logbit? 29 regval))
   ((equal? which 'secondary-reference)
    (logbit? 30 regval))
   ((equal? which 'aux-in-clk)
    (logbit? 8 regval))
   ((equal? which 'vcxo-clk)
    (logbit? 9 regval))
   (else
    (format #t "Unknown signal-type `~a'.\n" (symbol->string which)))))

(define (fuzz-register idx)
  (cond
   ((not (register-index? idx))
    (error-invalid-reg-index idx))
   (else
    (logior idx (ash (random (ash 1 28)) 4)))))

(define (decode-register-by-value regval)
  (decode-specific-register-by-value (bit-extract regval 0 4) regval))

(define (get-register-description idx)
  (let next ((rc register-content-table)
             (n idx))
    (if (= n 0)
        (car rc)
        (next (cdr rc) (1- n)))))

(define (get-decoder type)
  (let next ((d decoder-table))
    (cond ((null? d) dummy-decoder)
          ((equal? type (caar d)) (cdar d))
          (else (next (cdr d))))))

(define (decode-specific-register-by-value idx regval)
  (cond
   ((not (register-index? idx))
    (error-invalid-reg-index idx))
   (else
    (display "\n   ------------------------------------------------------\n")
    (format #t "  | Decoding bits \"~32,'0b\"...  |\n" regval)
    (format #t "  | ...as register ~2d:                hex: ~8,'0x      |\n"
            idx regval)
    (display "   ------------------------------------------------------\n\n")
    (let nextfield ((rest (get-register-description idx))
                    (start 0))
      (cond
       ((null? rest) (newline))
       (else
        (let* ((width (caar rest))
               (bits (bit-extract-width regval start width))
               (type (cdar rest))
               (decoder (get-decoder type)))
          (decoder bits width type))
        (nextfield (cdr rest) (+ start (caar rest)))))))))

(define (dummy-decoder x width type)
  (format #t
          "Decoder for `~a' not implemented, yet. (bit(s): ~b, width: ~d).\n"
          (symbol->string type) x width))

(define (decode/address bits width type)
  (format #t "Address bits: \"~4,'0b\", looks like register `~d'.\n\n"
          bits bits))

(define (decode/inbufsel bits width type)
  (format #t "Primary and secondary input buffer type: `~a' (bits: ~2,'0b)\n"
          (cond ((= bits #b01) "LVPECL")
                ((= bits #b11) "LVDS")
                ((= bits #b00) "LVCMOS")
                (else "INVALID"))
          bits))

(define (decode/delay-pfd bits width type)
  (format #t "PFD Pulse Width Delay: ~a (bits: ~2,'0b)\n"
          (cond ((= bits #b00) "1.5ns")
                ((= bits #b01) "3.0ns")
                ((= bits #b10) "4.5ns")
                (else "6.0ns"))
          bits))

(define (decode/mn-divider bits width type)
  (cond
   ((equal? type 'm-divider) (display "M"))
   (else (display "N")))
  (format #t "-Divider value: ~5d (binary: ~14,'0b plus 1)\n" (1+ bits) bits))

(define (decode/cp-current bits width type)
  (let nextval ((val charge-pump-current-table))
    (cond ((null? val)
           (display "decode/cp-current, BUG: This shouldn't happen.")
           (display "What register value did this happen with?\n"))
          (else
           (let ((b (caar val))
                 (amps (cadar val))
                 (remark (cddar val)))
             (cond
              ((= bits b)
               (format #t "CP-Current: ~f mA" amps)
               (if (not (null? remark))
                   (format #t " ~a" (car remark)))
               (format #t " (bits: ~4,'0b)\n" bits))
              (else
               (nextval (cdr val)))))))))

(define (decode/reserved-bits bits width type)
  (format #t
          (string-join
           (list "~d reserved bit"
                 (if (> width 1) "s" "")
                 ": ~"
                 (number->string width 10)
                 ",'0b\n") "")
          width bits))

(define (is-output-mode? type bits)
  (let ((val (cadar type)))
    (cond
     ((not (list? val))
      (= bits val))
     (else
      (let nextval ((v val))
        (cond
         ((null? v) #f)
         ((= bits (car v)) #t)
         (else (nextval (cdr v)))))))))

(define (decode/output-mode bits width type)
  (display "Output Mode: ")
  (let nexttype ((type output-modes))
    (cond ((null? type) (display "Invalid setting"))
          ((is-output-mode? type bits)
           (display (string-upcase (symbol->string (caar type)))))
          (else
           (nexttype (cdr type)))))
  (format #t " (bits: ~7,'0b)\n" bits))

(define (decode/coarse bits width type)
  (format #t
          "Coarse-Phase-Adjust bits are undocumented, but: ~7,'0b\n"
          bits))

(define (decode/divider bits width type)
  (format
   #t
   "~a: ~a (bits: ~7,'0b)\n"
   (cond
    ((equal? type 'output-divider) "Output divider")
    ((equal? type 'fb-divider) "Feedback divider")
    (else (format #f "Invalid divider type: `~a'"
                  (symbol->string type))))
   (let ((val (get-divider-value-by-bits bits)))
     (if (equal? val 'invalid)
         "Invalid setting"
         (number->string val 10)))
   bits))

(define (simple-string-print str bits width)
  (let ((fmt (string-join (list "~a (bit"
                                (if (> width 1) "s" "")
                                ": ~"
                                (number->string width 10)
                                ",'0b)\n")
                          "")))
    (format #t fmt str bits)))

(define (decode/simple-string bits width type)
  ;; If the bits to decode don't need special attention, but basically
  ;; just require a string to be printed depending on the bit-field's
  ;; value, then this one can be used. The strings to display are looked
  ;; up in `decode-string-table'.
  (let nextstr ((dst decode-string-table))
    (cond
     ((null? dst)
      (simple-string-print (format #f "Missing string for `~a'.\n"
                                   (symbol->string type))
                           bits
                           width))
     ((equal? type (caar dst))
      (let nextval ((v (cdar dst)))
        (cond
         ((null? v)
          (simple-string-print (format #f "Invalid value for `~a'."
                                       (symbol->string type))
                               bits
                               width))
         ((= bits (caar v))
          (simple-string-print (cdar v) bits width))
         (else
          (nextval (cdr v))))))
     (else
      (nextstr (cdr dst))))))

(define decoder-table
  `((address . ,decode/address)
    (coarse-phase-adjust . ,decode/coarse)
    (cp-current . ,decode/cp-current)
    (cp-direction . ,decode/simple-string)
    (cp-mode . ,decode/simple-string)
    (cp-op-amp . ,decode/simple-string)
    (cp-preset-output-voltage . ,decode/simple-string)
    (cp-sink . ,decode/simple-string)
    (cp-src . ,decode/simple-string)
    (delay-pfd . ,decode/delay-pfd)
    (divider-enable . ,decode/simple-string)
    (fb-divider . ,decode/divider)
    (i-ref-pull-down . ,decode/simple-string)
    (in-buf-sel . ,decode/inbufsel)
    (m-divider . ,decode/mn-divider)
    (n-divider . ,decode/mn-divider)
    (output-divider . ,decode/divider)
    (output-mode . ,decode/output-mode)
    (pri-sec-sel . ,decode/simple-string)
    (ref-sel-ctrl . ,decode/simple-string)
    (reserved . ,decode/reserved-bits)
    (vcxo-sel . ,decode/simple-string)))

(define decode-string-table
  '((cp-direction
     . ((0 . "CP-Direction: positive")
        (1 . "CP-Direction: negative")))
    (cp-mode
     . ((0 . "CP-Mode: 3V")
        (1 . "CP-Mode: 5V")))
    (cp-op-amp
     . ((0 . "CP-OpAmp: on (Test-GTME)")
        (1 . "CP-OpAmp: off (Test-GTME)")))
    (cp-preset-output-voltage
     . ((0 . "CP-Preset-Voltage: off")
        (1 . "CP-Preset-Voltage: on")))
    (cp-sink
     . ((0 . "CP-Current-Sink: off (Test-GTME)")
        (1 . "CP-Current-Sink: on (Test-GTME)")))
    (cp-src
     . ((0 . "CP-Current-Source: off (Test-GTME)")
        (1 . "CP-Current-Source: on (Test-GTME)")))
    (divider-enable
     . ((0 . "Output divider disabled.")
        (1 . "Output divider enabled.")))
    (i-ref-pull-down
     . ((0 . "I_REF_CP pull-down res: disabled (Test-GTME)")
        (1 . "I_REF_CP pull-down res: enabled (Test-GTME)")))
    (pri-sec-sel
     . ((0 . "No input buffer is selected/active.")
        (1 . "PRI_BUF is selected, SEC_BUF is powered down.")
        (2 . "SEC_BUF is selected, PRI_BUF is powered down.")
        (3 . "Auto Reference Select (PRI then SEC).")))
    (ref-sel-ctrl
     . ((0 . "REG0:6+7 ignored. REF_SEL pin selects reference input.")
        (1 . "REF_SEL pin disabled. REG0:6+7 are used to select ref-in.")))
    (vcxo-sel
     . ((0 . "Divider-sync: PRI/SEC-ref according to REG0:6+7.")
        (1 . "Divider-sync: VCXO/AUX selected.")))))
