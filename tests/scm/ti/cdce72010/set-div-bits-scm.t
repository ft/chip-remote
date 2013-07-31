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

(use-modules (ice-9 format)
             (test tap)
             (bitops)
             (chip-remote devices ti cdce72010 prg))
(primitive-load "tests/test-tap-cfg.scm")

(load "divider-samples.scm")

;; The width of the divider bit field (needed for extraction).
(define bit-width 7)

;; We'll test against different register values, to make sure the operation
;; didn't just succeed because the old register value was "just-ones" or
;; "just-zeros".
(define target-register-values
  '(#x00000000
    #xffffffff
    #x33333333
    #xcccccccc
    #xaaaaaaaa))

;; The CDCE72010 uses the complex divider settings for output- and feedback-
;; dividers. They use the same bits but in different places.
(define functions `((,set-bits-fbdiv . 9)
                    (,set-bits-odiv . 17)))

(define (test-gen fnc sh val div exp)
  (let* ((got (fnc val div))
         (bits (bit-extract-width got sh bit-width)))
    (define-test (format #f
                         "div(~d@~d), regval: ~s, exp: ~s, got: ~s"
                         div
                         sh
                         (number->string got 16)
                         (number->string exp 2)
                         (number->string bits 2))
      (pass-if-= bits exp))))

(define (loop-over-values fnc shifts)
  ;; loop over register value list
  (let nextregval ((values target-register-values))
    (cond
     ((null? values) #t)
     (else
      (let ((value (car values)))
        ;; loop over sample divider values
        (let nextdiv ((divlist divider-samples))
          (cond
           ((null? divlist) #t)
           (else
            (let ((div (caar divlist))
                  (expected (cadar divlist)))
              (if (not (test-gen fnc shifts value div expected))
                  (quit 1))
              (nextdiv (cdr divlist)))))))
      (nextregval (cdr values))))))

(with-fs-test-bundle
 (plan (* (length functions)
          (length divider-samples)
          (length target-register-values)))
 (for-each (lambda (x)
             (loop-over-values (car x)
                               (cdr x)))
           functions))
