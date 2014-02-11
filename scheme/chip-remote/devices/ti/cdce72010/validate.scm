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

(define-module (chip-remote devices ti cdce72010 validate)
  :export (divider?
           mn-divider-value?
           output-index?
           r-divider?
           register-index?))

(define (divider? idx)
  (and (> idx 0)
       (< idx 9)))

(define (mn-divider-value? value)
  (and (> value 0)
       ;; max. value is 14 bits set to `1' plus 1.
       (<= value #b100000000000000)))

(define (output-index? idx)
  (and (>= idx 0)
       (<= idx 9)))

(define (r-divider? type)
  (and (symbol? type)
       (or (equal? type 'primary)
           (equal? type 'secondary))))

(define (register-index? idx)
  (and (>= idx 0)
       (<= idx 12)))
