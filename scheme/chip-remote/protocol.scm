;;;;
;; Copyright 2012 Frank Terbeck <ft@bewatermyfriend.org>, All rights reserved.
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

;;---------------------------------------------------------------
;;
;; This module implements the plain text protocol used between a host computer
;; (which runs the chip-remote interpreter) and an external interface-driver
;; board.
;;
;; The protocol is versioned. The version implemented by the board may be
;; queried by using the `VERSION' command. The reply consists of these parts:
;;
;;    VERSION  <major-compat> <minor-compat> <additions>
;;
;; VERSION is a fixed string.
;;
;; <major-compat> is a version number (in hex, like every other numeric value
;; in the protocol as well. A mismatch here denotes significant differences
;; between host and board which make the combination unusable (most likely).
;; Increases in this number would be caused by complete protocol redesign.
;;
;; <minor-compat> is another version number. It is increased when existing
;; features are changed in ways that break backwards compatibility.
;;
;; <additions> is yet another version number, that is increased when a new
;; features is added to the protocol.
;;
;;---------------------------------------------------------------
;;
;; The protocol supports a implementation-defined communication channel: Any
;; messages that start in an ASCII colon (`:', code: 0x3a) are explicitly
;; reserved for implementation specific use.
;;
;;---------------------------------------------------------------

(define-module (chip-remote protocol)
  :use-module (chip-remote-primitives)
  :use-module (srfi srfi-1) ;; Implements `fold' and `zip'.
  :export (write-raw
           read-raw
           bye
           hi
           protocol-version))

;; Words in the protocol may contain letters and digits from ASCII.
(define protocol-char-set
  (char-set-diff+intersection char-set:ascii
                              char-set:letter+digit))

;; Turn a hexadecimal string into an integer. Returns `#f' in case of an error.
(define (hexstring->int str)
  (string->number str 16))

;; Takes a list of exactly two elements:
;;
;;   - If the second item is a string, the first string must be the same string.
;;   - If the second item is 'int, the first must by a valid hexstring.
;;
;; Returns a list of two items: `#t' or `#f' depending on whether the
;; verification succeeded and the (possibly converted) input data; if the first
;; value is `#f', the second is always the unchanged input data.
(define (verify pair)
  (let ((got (car pair))
        (want (cadr pair)))
    (cond ((string? want)
           (list (string=? want got)
                 got))
          ((eq? want 'int)
           (let* ((val (hexstring->int got))
                  (success (integer? val)))
             (list success
                   (if success val got))))
          (else
           (list #f got)))))

;; Checks if an input string meets the supplied conditions.
;;
;; Say the input is "VERSION 2 7 c" and the conditions are these: The string's
;; first word has to be "VERSION". The following parts need to be exactly three
;; chunks of hexadecimally encoded integers.
;;
;; You'd call it like this:
;;
;;  (expect-read "VERSION 2 7 c" '("VERSION" int int int))
;;    => ("VERSION" 2 7 12)
;;
;; You may optionally supply a function that postprocesses the returned list.
;; Say, in the previous example, you're really only interested in the three
;; integers and not in the fixed string "VERSION". You'd do this:
;;
;;  (expect-read "VERSION 2 7 c" '("VERSION" int int int))
;;    => (2 7 12)
;;
;; If a condition is not met, `#f' is returned.
(define* (expect-read string what #:optional (postproc (lambda (x) x)))
  (let ((dtp (zip (string-tokenize string protocol-char-set) what)))
    (cond
     ((not (eq? (length what)
                (length dtp)))
      #f)
     (else
      ;; The data in dtp looks like this:
      ;;
      ;; '(("VERSION" "VERSION")
      ;;   ("2" int)
      ;;   ("7" int)
      ;;   ("c" int))
      ;;
      ;; The `fold' turns it into the following: '(#t "VERSION" 2 7 12)
      ;; Or in case only dec. ints were allowed: '(#f "VERSION" 2 7 "c")
      ;;
      ;; If the first element is `#t' return the rest, otherwise return `#f'.
      (let ((results (fold
                      (lambda (new acc)
                        (let ((old (car acc))
                              (lst (cdr acc))
                              (v (verify new)))
                          (append (list (and old (car v)))
                                  lst
                                  (cdr v))))
                      '(#t)
                      dtp)))
        (if (car results)
            (postproc (cdr results))
            #f))))))

;; Write a raw string to the device.
(define (write-raw message)
  (cr/write-raw message))

;; Read a raw string from the device.
(define (read-raw)
  (cr/read-raw))

;; Read from the device, save the reply and run code in case the read was
;; successful. Return `#f' otherwise.
(define-syntax with-read-raw-string
  (lambda (x)
    (syntax-case x ()
      ((_ (r) code ...)
       #'(let ((r (read-raw)))
           (if (not (string? r))
               #f
               (begin code ...)))))))

;; Initiate communication channel to the device.
(define (hi)
  (write-raw "HI")
  (with-read-raw-string (reply)
    (string=? reply "Hi there, stranger.")))

;; Close down communication channel to the device.
(define (bye)
  (write-raw "BYE")
  (with-read-raw-string (reply)
    (string=? reply "Have a nice day.")))

;; Query protocol version from the board.
(define (protocol-version)
  (write-raw "VERSION")
  (with-read-raw-string (reply)
    (expect-read reply '("VERSION" int int int) cdr)))
