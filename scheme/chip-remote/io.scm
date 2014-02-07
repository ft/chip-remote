;;;;
;; Copyright 2013 Frank Terbeck <ft@bewatermyfriend.org>, All rights reserved.
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

;; Low-level IO library for `chip-remote'

(define-module (chip-remote io)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:export (cr-connection?
            cr-connection-uri
            cr-path
            io-close
            io-open
            io-opt/get
            io-opt/set
            io-read
            io-write
            make-cr-connection))

(define-record-type <cr-connection>
  (make-bare-cr-connection)
  cr-connection?
  (uri cr-connection-uri set-cr-connection-uri!)
  (port cr-connection-port set-cr-connection-port!))

(define (uri-defaults-to-file string-uri)
  (let ((uri (string->uri string-uri)))
    (if uri
        uri
        (string->uri (string-concatenate (list "file://" string-uri))))))

(define (make-cr-connection string-uri)
  (let* ((uri (uri-defaults-to-file string-uri))
         (scheme (uri-scheme uri)))
    (unless (eq? scheme 'file)
      (throw 'unsupported-uri-scheme uri))
    (let ((new (make-bare-cr-connection)))
      (set-cr-connection-uri! new uri)
      new)))

(define (cr-path connection)
  (uri-path (cr-connection-uri connection)))

(define io-options '((serial-timeout . 2)
                     (trace . #f)))

(define (io-opt/get key)
  (let ((value (assq key io-options)))
    (if value
        (cdr value)
        (throw 'unknown-option key))))

(define (io-opt/set key value)
  (let ((current-value (assq key io-options)))
    (if current-value
        (assq-set! io-options key value)
        (throw 'unknown-option `(,key ,value)))))

(define (io-read connection)
  (let ((result (select (list (cr-connection-port connection))
                        '()
                        '()
                        (io-opt/get 'serial-timeout))))
    (if (null? (car result))
        (throw 'read-timeout connection)
        (let ((string (read-line (cr-connection-port connection) 'trim)))
          (unless (string? string)
            (throw 'io-read-error))
          (if (io-opt/get 'trace)
              (begin
                (display (string-concatenate (list " <<< " string)))
                (newline)))
          string))))

(define (io-write connection string)
  (if (io-opt/get 'trace)
      (begin
        (display (string-concatenate (list " >>> " string)))
        (newline)))
  (let ((p (cr-connection-port connection)))
    (display string p)
    (newline p)))

(define (io-open connection)
  (set-cr-connection-port! connection
                           (open-file (cr-path connection) "r+l")))

(define (io-close connection)
  (close-port (cr-connection-port connection)))
