;; Copyright 2012-2013 Frank Terbeck <ft@bewatermyfriend.org>, All
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

(use-modules (test tap))
(primitive-load "tests/test-tap-cfg.scm")

(define er (@@ (chip-remote protocol) expect-read))

(define working-expects '(("VERSION 2 7 ca"
                           ("VERSION" int int int)
                           ("VERSION" 2 7 202))))

(define failing-expects '(("VERSION 2 7z ca"
                           ("VERSION" int int int)
                           ("7z" . int))))

(with-fs-test-bundle
 (plan (+ (length working-expects)
          (length failing-expects)))

 (map (lambda (x)
        (let* ((input (car x))
               (spec (cadr x))
               (expected (caddr x))
               (result (er input spec)))
          (define-test (format #f "expect-read succeeds: ~s" input)
            (pass-if-equal? result expected))))
      working-expects)

 (map (lambda (x)
        (let* ((input (car x))
               (spec (cadr x))
               (failure (caddr x)))
          (define-test (format #f "expect-read fails: ~s" input)
            (pass-if-exception 'protocol-unexpected-data
                               (er input spec)))))
      failing-expects))
