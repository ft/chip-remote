;; Copyright 2014 Frank Terbeck <ft@bewatermyfriend.org>, All
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

(use-modules (test tap)
             (srfi srfi-1))
(primitive-load "tests/test-tap-cfg.scm")

(define zip2 (@@ (chip-remote protocol) zip2))
(define pcs (@@ (chip-remote protocol) protocol-char-set))

(define charset-tests
  '(("single word" "FOO123BAR"
     "FOO123BAR")
    ("single word with dash" "FOO-BAR"
     "FOO-BAR")
    ("three simple words" "FOO BAR BAZ"
     "FOO" "BAR" "BAZ")
    ("three simple words, two with dash" "FOO-BAR BAZ-QUUZ DING"
     "FOO-BAR" "BAZ-QUUZ" "DING")))

(with-fs-test-bundle
 (plan (+ 6 (length charset-tests)))

 (define-test "zip2: normal zip"
   (pass-if-equal? (zip2 '(a c e) '(b d f))
                   '((a . b) (c . d) (e . f))))

 (define-test "zip2: first short"
   (pass-if-equal? (zip2 '(a c) '(b d f))
                   '((a . b) (c . d))))

 (define-test "zip2: second short"
   (pass-if-equal? (zip2 '(a c e) '(b d))
                   '((a . b) (c . d))))

 (define-test "zip2: first empty"
   (pass-if-true (null? (zip2 '() '(b d f)))))

 (define-test "zip2: second empty"
   (pass-if-true (null? (zip2 '(a c e) '()))))

 (define-test "zip2: both empty"
   (pass-if-true (null? (zip2 '() '()))))

 (map (lambda (x)
        (define-test (format #f "charset: ~a" (car x))
          (pass-if-equal? (string-tokenize (cadr x) pcs)
                          (cddr x))))
      charset-tests))
