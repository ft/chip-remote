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
             (ti cdce72010-prg))

;; Set this to `#t' to get output for succeeded tests, too.
(define test-verbose #f)

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

;; Here is what's going on: for each function in `setters', this is
;; applying each divider from `divider-samples' to all values from
;; `target-register-values'. From that result the target bits are
;; extracted again and compared to the expected register value from
;; `divider-samples'. Each such comparison should succeed.

(define (test-output div val got bits exp)
  (display (format
            "set divider in ~d:\n\n  from ~s, ~s\n    to ~s, ~s\n\n"
            div
            (number->string val 2)
            (number->string val 16)
            (number->string got 2)
            (number->string got 16)))
  (display (format "     got ~s,\nexpected ~s\n"
                   (number->string bits 2)
                   (number->string exp 2))))

(define (test-setter fnc sh val div exp)
  (let* ((got (fnc val div))
         (bits (bit-extract got sh (+ bit-width sh))))
    (cond
     ((not (= bits exp))
      (test-output div val got bits exp)
      #f)
     (else
      (if test-verbose
          (test-output div val got bits exp))
      #t))))

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
              (if (not (test-setter fnc
                                    shifts
                                    value
                                    div
                                    expected))
                  (quit 1))
              (nextdiv (cdr divlist)))))))
      (nextregval (cdr values))))))

;; The CDCE72010 uses the complex divider settings for output- and feedback-
;; dividers. They use the same bits but in different places.
(loop-over-values set-bits-fbdiv 9)
(loop-over-values set-bits-odiv 17)

(quit 0)
