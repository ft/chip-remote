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
             (chip-remote devices ti cdce72010 tables))
(primitive-load "tests/test-tap-cfg.scm")

(load "divider-samples.scm")

(with-fs-test-bundle
 (plan (length divider-samples))
 (let next ((c divider-samples))
   (cond ((null? c)
          (quit 0))
         (else
          (let ((got (get-bits-for-divider (caar c)))
                (exp (cadar c))
                (div (caar c)))
            (define-test (format #f
                                 "div(~d), exp: ~s, got: ~s."
                                 div
                                 (number->string exp 2)
                                 (number->string got 2))
              (pass-if-= exp got))
            (next (cdr c)))))))
