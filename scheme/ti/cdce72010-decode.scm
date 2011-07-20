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

(define-module (ti cdce72010-decode)
  :export (decode-register-by-value
           decode-specific-register-by-value
           fuzz-register
           pll-lock?
           signal-exists?))

(use-modules (bitops)
             (ti cdce72010-messages)
             (ti cdce72010-tables)
             (ti cdce72010-validate))

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
    (display "\n  ,------------------------------------------------------.\n")
    (format #t "  | Decoding bits \"~32,'0b\"...   |\n" regval)
    (format #t "  | ...as register ~2d:                hex: ~8,'0x       |\n"
            idx regval)
    (display "  `------------------------------------------------------´\n\n")
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

(define (decode/mn-divider bits width type)
  (cond
   ((equal? type 'm-divider) (display "M"))
   (else (display "N")))
  (format #t "-Divider value: ~5d (binary: ~14,'0b plus 1)\n" (1+ bits) bits))

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
      (simple-string-print "Missing string for `~a'.\n"
                           (symbol->string type)
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
    (in-buf-sel . ,decode/inbufsel)
    (m-divider . ,decode/mn-divider)
    (n-divider . ,decode/mn-divider)
    (ref-sel-ctrl . ,decode/simple-string)))

(define decode-string-table
  '((ref-sel-ctrl
     . ((0 . "REF_SEL pin disabled. REG0:6+7 are used to select ref-in.")
        (1 . "REG0:6+7 ignored. REF_SEL pin selects reference input.")))))
