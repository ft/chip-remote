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
             (test tap)
             (chip-remote devices ti cdce72010 prg))
(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (plan 4)
 (define-test "set-bits-rdiv primary: set"
   (pass-if-true (logbit? 4 (set-bits-rdiv #x00000000 'primary #t))))

 (define-test "set-bits-rdiv secondary: set"
   (pass-if-true (logbit? 5 (set-bits-rdiv #x00000000 'secondary #t))))

 (define-test "set-bits-rdiv primary: unset"
   (pass-if-false (logbit? 4 (set-bits-rdiv #xffffffff 'primary #f))))

 (define-test "set-bits-rdiv secondary: unset"
   (pass-if-false (logbit? 5 (set-bits-rdiv #xffffffff 'secondary #f)))))
