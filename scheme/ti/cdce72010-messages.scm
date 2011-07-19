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

(define-module (ti cdce72010-messages)
  :export (error-divider
           error-invalid-r-divider
           error-invalid-reg-index
           error-mn-divider
           error-not-boolean
           error-output-index))

(define (error-divider x)
  (display "`divider' needs to be an integer 1..8.\n")
  (display (format #f "Was: ~d\n" x)))

(define (error-mn-divider-value value)
  (display (format #f "The M and N dividers need to be set from 1 to ~d.\n"
                   (#b100000000000000)))
  (display (format #f "You tried to set: ~d\n" value)))

(define (error-output-index)
  (display "Output index out of range (0..9)\n"))

(define (error-invalid-r-divider)
  (display "r-divider type needs to be either 'primary or 'secondary.\n"))

(define (error-invalid-reg-index idx)
  (format #t "Register index (~d) out of range. [0..12]\n" idx))

(define (error-not-boolean what)
  (format #t "~a needs to be a boolean value.\n" what))
