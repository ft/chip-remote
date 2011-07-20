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
  (format #t "Register bits: \"~4,'0b\", looks like register `~d'.\n\n"
          bits bits))

(define (decode/mn-divider bits width type)
  (cond
   ((equal? type 'm-divider) (display "M"))
   (else (display "N")))
  (format #t "-Divider value: ~5d (binary: ~14,'0b plus 1)\n" (1+ bits) bits))

(define decoder-table
  `((address . ,decode/address)
    (m-divider . ,decode/mn-divider)
    (n-divider . ,decode/mn-divider)))
