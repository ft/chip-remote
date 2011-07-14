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
  :export (read-registers
           toggle-trace))

(use-modules (ti cdce-primitives))

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
