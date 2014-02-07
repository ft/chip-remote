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

(define-module (chip-remote protocol)
  :use-module (chip-remote io)
  :use-module (srfi srfi-1)
  :export (bye
           hi
           features
           ports
           protocol-version))

(define (zip2 la lb)
  (let next ((a la)
             (b lb)
             (acc '()))
    (cond ((any null? (list a b))
           (reverse acc))
          (else
           (next (cdr a) (cdr b)
                 (cons (cons (car a) (car b))
                       acc))))))

;; Words in the protocol may contain letters and digits from ASCII.
(define protocol-char-set
  (char-set-diff+intersection char-set:ascii
                              char-set:letter+digit))

;; Turn a hexadecimal string into an integer. Returns `#f' in case of an error.
(define (hexstring->int str)
  (string->number str 16))

;; Takes a pair:
;;
;;   - If the second item is a string, the first string must be the same string.
;;   - If the second item is 'int, the first must by a valid hexstring.
;;
;; Returns a pairs: `#t' or `#f' depending on whether the verification
;; succeeded and the (possibly converted) input data; if the first value is
;; `#f', the second is always the unchanged input data.
(define (verify-and-convert pair)
  (let ((got (car pair))
        (want (cdr pair)))
    (cond ((string? want)
           (cons (string=? want got)
                 got))
          ((eq? want 'int)
           (let* ((val (hexstring->int got))
                  (success (integer? val)))
             (cons success
                   (if success val got))))
          ((eq? want 'string)
           (cons #t got))
          (else
           (cons #f got)))))

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
  (let ((dtp (zip2 (string-tokenize string protocol-char-set) what)))
    (cond
     ((not (eq? (length what)
                (length dtp)))
      #f)
     (else
      ;; The data in dtp looks like this:
      ;;
      ;; '(("VERSION" . "VERSION")
      ;;   ("2" . int)
      ;;   ("7" . int)
      ;;   ("c" . int))
      ;;
      ;; The `fold' turns it into the following: '(#t "VERSION" 2 7 12)
      ;; Or in case only dec. ints were allowed: '(#f "VERSION" 2 7 "c")
      ;;
      ;; If the first element is `#t' return the rest, otherwise return `#f'.
      (let ((results (fold
                      (lambda (new acc)
                        (let ((old (car acc))
                              (lst (cdr acc))
                              (v (verify-and-convert new)))
                          (append (list (and old (car v)))
                                  lst
                                  (cons (cdr v) '()))))
                      '(#t)
                      dtp)))
        (if (car results)
            (postproc (cdr results))
            #f))))))

;; Read from the device, save the reply and run code in case the read was
;; successful. Return `#f' otherwise.
(define-syntax with-read-raw-string
  (lambda (x)
    (syntax-case x ()
      ((_ (c r) code ...)
       #'(let ((r (io-read c)))
           code ...)))))

;; Initiate communication channel to the device.
(define (hi conn)
  (io-write conn "HI")
  (with-read-raw-string (conn reply)
    (string=? reply "Hi there, stranger.")))

;; Close down communication channel to the device.
(define (bye conn)
  (io-write conn "BYE")
  (with-read-raw-string (conn reply)
    (string=? reply "Have a nice day.")))

;; Query protocol version from the board.
(define (protocol-version conn)
  (io-write conn "VERSION")
  (with-read-raw-string (conn reply)
    (expect-read reply '("VERSION" int int int) cdr)))

;; A set of commands return more than one reply. The host triggers the 2nd to
;; the N-th reply by saying "MORE". The board will reply with DONE when there
;; is nothing more to say. This function does exactly that and returns a list
;; of replies for further processing.
(define (list-more-done conn item)
  (io-write conn item)
  (let next ((f '())
             (reply (io-read conn)))
    (cond ((string=? reply "DONE") f)
          (else (io-write conn "MORE")
                (next (cons reply f)
                      (io-read conn))))))

;; Check if the second argument (`list') to the function is a list, if not
;; return it unchanged. If it is, map `proc' over and return the result.
(define (list-and-map proc list)
  (cond ((not (list? list)) list)
        (else (map proc list))))

;; Turns a string into a lower-cased symbol: "FOO" => foo
(define (reply->symbol s)
  (string->symbol (string-downcase s)))

;; Queries the board's feature list and returns a list of according symbols.
(define (features conn)
  (list-and-map reply->symbol
                (list-more-done conn "FEATURES")))

(define (string+int->pair s)
  (let ((l (expect-read s '(string int))))
    (cond ((not (list? l)) l)
          (else (cons (reply->symbol (car l))
                      (cadr l))))))

;; Queries the board for its ports and returns a list of alists.
(define (ports conn)
  (list-and-map string+int->pair
                (list-more-done conn "PORTS")))
