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

(with-fs-test-bundle
 (define syntax '("VERSION" int int int))
 (define input "VERSION 2 7 ca")
 (define expected '("VERSION" 2 7 202))

 (plan 2)

 (define-test "expect-read with \"VERSION\" 2 7 ca) => '(\"VERSION\" 2 7 202)"
   (pass-if-equal? (er input syntax)
                   expected))

 (define-test "expect-read with cdr post-proc => '(2 7 202)"
   (pass-if-equal? (er input syntax cdr)
                   (cdr expected))))
