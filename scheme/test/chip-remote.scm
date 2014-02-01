;; Copyright 2014 Frank Terbeck <ft@bewatermyfriend.org>, All rights reserved.
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

(define-module (test chip-remote)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote io)
  #:use-module (chip-remote protocol)
  #:export (init-connection
            close-connection
            test-with-tag))

(define* (init-connection #:key (device (getenv "CR_BOARD_DEVICE")))
  (let ((c (make-cr-connection device)))
    (test-open c)
    (test-hi c)
    c))

(define (close-connection c)
  (test-bye c)
  (test-close c))

(define (test-with-tag tag value)
  (unless value
    (format #t "# FAIL with-tag: ~a~%" tag)
    (quit 1)))

(define (test-open c)
  (unless (io-open c)
    (format #t "# FAIL Establishing serial connection failed.~%")
    (quit 1)))

(define (test-close c)
  (unless (io-close c)
    (format #t "# FAIL Closing serial connection failed.~%")
    (quit 1)))

(define (test-hi c)
  (unless (hi c)
    (format #t "# FAIL Protocol HI failed.~%")
    (quit 1)))

(define (test-bye c)
  (unless (bye c)
    (format #t "# FAIL Protocol BYE failed.~%")
    (quit 1)))
