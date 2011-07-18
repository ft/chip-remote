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

(define-module (ti cdce72010-prg)
  :export (clear-device-power-down-bit
           clear-odiv-enable-bit
           clear-pll-power-down-bit
           set-bits-fbdiv
           set-bits-mdiv
           set-bits-ndiv
           set-bits-odiv
           set-bits-output-mode
           set-bits-rdiv
           set-device-power-down-bit
           set-odiv-enable-bit
           set-pll-power-down-bit))

(use-modules (bitops)
             (ti cdce72010-tables))

(define (set-bits-odiv regval divval)
  (set-bits regval (get-bits-for-divider divval) 7 17))

(define (set-bits-fbdiv regval divval)
  (set-bits regval (get-bits-for-divider divval) 7 9))

;; The M and N dividers are way simpler to set than the fb/output ones. You
;; just need to substract 1 off of the `divval' value and put the resulting
;; bits into the right position.
(define (set-bits-mdiv regval divval)
  (set-bits regval (1- divval) 14 4))

(define (set-bits-ndiv regval divval)
  (set-bits regval (1- divval) 14 18))

(define (set-bits-output-mode regval mode)
  (set-bits regval (get-bits-for-output-mode mode) 7 25))

(define (set-odiv-enable-bit regval)
  (logior regval (ash 1 24)))

(define (clear-odiv-enable-bit regval)
  (clear-bits regval 1 24))

(define (set-pll-power-down-bit regval)
  (logior regval (ash 1 23)))

(define (clear-pll-power-down-bit regval)
  (clear-bits regval 1 23))

(define (set-device-power-down-bit regval)
  (logior regval (ash 1 11)))

(define (clear-device-power-down-bit regval)
  (clear-bits regval 1 11))

(define (set-bits-rdiv regval type state)
  (let ((bitnum (cond
                 ((equal? type 'primary) 4)
                 ((equal? type 'secondary) 5))))
    (cond (state
           (logior regval (ash 1 bitnum)))
          (else
           (clear-bits regval 1 bitnum)))))
