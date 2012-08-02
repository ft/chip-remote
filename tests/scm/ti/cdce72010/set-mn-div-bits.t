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

(use-modules (ice-9 format)
             (bitops)
             (chip-remote devices ti cdce72010 prg))

;; Since the `set-div-bits' test already checks against non-trivial bit
;; patterns, we'll assume that the involved bit operations work properly.

(define values '(#b000000000000001
                 #b100000000000000
                 #b011111111111111
                 #b010101010101010
                 #b001010101010101
                 #b011001100110011
                 #b000110011001100))

(define value-width 14)

(define (run-check fcn shifts)
  (let nextval ((v values))
    (cond
     ((null? v) #t)
     (else
      (let* ((exp (car v))
             (result (fcn #xffffffff exp))
             ;; Read the bits from the result and add 1, because the bits
             ;; store the configured divider value minus 1.
             (got (1+ (bit-extract-width result shifts value-width))))
        (cond
         ((not (= exp got))
          (display
           (format #f
                   "expected: ~s\n     got: ~s\nresult was ~s"
                   (number->string exp 2)
                   (number->string got 2)
                   (number->string result 16)))
          (quit 1))
         (else
          (nextval (cdr v)))))))))

(run-check set-bits-mdiv 4)
(run-check set-bits-ndiv 18)

(quit 0)
