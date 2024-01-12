;; Copyright (c) 2024 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; TODO: I think META messages are not properly decoded.

(define-module (protocol ufw-regp)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs bytevectors gnu)
  #:use-module ((scheme base) #:select (bytevector-append))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote utilities)
  #:use-module (protocol length-prefix)
  #:use-module (protocol slip)
  #:export (regp:decode*                ; Low Level decode
            regp:encode                 ; Low Level encode
            regp:encode-header          ; Low Level Header encode
            regp:decode                 ; High Level decode
            regp:read-request           ; High Level encode
            regp:write-request          ; High Level encode
            regp:read-request!          ; Perform request on connection
            regp:write-request!         ; Perform request on connection
            regp:frame-errors           ; Frame type query
            regp:frame-valid?           ; Frame type query
            regp:acknowledge?           ; Frame type query
            regp:valid-ack?             ; Frame type query
            regp:tcp-connection         ; Connection generator
            regp:serial-connection      ; Connection generator
            regp:connection?            ; Connection predicate
            regp:set-port!              ; Connection update
            regp:framing                ; Connection query
            regp:port                   ; Connection query
            regp:word-size-16?          ; Connection query
            regp:with-header-crc?       ; Connection query
            regp:with-payload-crc?      ; Connection query
            regp:send                   ; Connection primitive
            regp:recv))                 ; Connection primitive

;;; Checksumming

(define *iserial-crc-polynomial* #x8005)
(define *iserial-crc-initial* #x0000)

(define (reflect-bits data width)
  (let ((data (logand data (1- (ash 1 width)))))
    (let loop ((pos (1- width)) (acc 0))
      (if (< pos 0)
          acc
          (loop (1- pos)
                (if (zero? (logand (ash 1 pos) data))
                    acc
                    (logior acc (ash 1 (- width pos 1)))))))))

(define (bytevector-fold fnc init data n)
  (let ((end (1- n)))
    (let loop ((n 0) (acc init))
      (if (> n end)
          acc
          (loop (1+ n) (fnc (array-ref data n) acc))))))

(define (next-crc-value poly value)
  (let ((new (ash value 1)))
    (if (zero? (logand #x8000 value))
        new
        (logxor poly new))))

(define (crc-16-step poly acc datum)
  (let loop ((n 0) (new (logxor acc (ash (reflect-bits datum 8) 8))))
    (if (>= n 8)
        (logand new #xffff)
        (loop (1+ n) (next-crc-value poly new)))))

(define (crc-16 poly init data n)
  (reflect-bits (bytevector-fold (lambda (x a) (crc-16-step poly a x))
                                 init data n)
                16))

(define (checksum-more previous data n)
  (crc-16 *iserial-crc-polynomial* (reflect-bits previous 16) data n))

(define (checksum data n)
  (checksum-more *iserial-crc-initial* data n))

(define (checksum-all data)
  (checksum data (bytevector-length data)))

(define (make-checksum-pair t c)
  `((transmitted . ,t)
    (calculated  . ,c)))

(define (test-checksum checksum)
  (match checksum
    ((('transmitted . a)
      ('calculated  . b)) (= a b))
    (#f #t)
    (_ #f)))

;;; Register Protocol Encodings

(define implementation-version 0)
(define frame-min-size 12)
(define opt:word-size-16     (ash 1 0))
(define opt:with-header-crc  (ash 1 1))
(define opt:with-payload-crc (ash 1 2))

(define type-table
  '((read-request   . #x0)
    (read-response  . #x1)
    (write-request  . #x2)
    (write-response . #x3)
    (meta-message   . #xf)))

(define option-table
  `((word-size-16     . ,opt:word-size-16)
    (with-header-crc  . ,opt:with-header-crc)
    (with-payload-crc . ,opt:with-payload-crc)
    (reserved         . #x1000)))

(define meta-meta-table
  '((error-header-encoding . 1)
    (error-header-crc      . 2)))

(define response-table
  '((acknowledge           .  0)
    (error-word-size       .  1)
    (error-payload-crc     .  2)
    (error-payload-size    .  3)
    (error-rx-overflow     .  4)
    (error-tx-overflow     .  5)
    (error-remote-busy     .  6)
    (error-unmapped-memory .  7)
    (error-access-denied   .  8)
    (error-value-range     .  9)
    (error-value-invalid   . 10)
    (error-input-output    . 11)))

;;; Register Protocol Utilities

(define (match-options bits)
  (fold (lambda (mask result)
          (if (bitset? bits (cdr mask))
              (cons (car mask) result)
              result))
        '()
        (reverse option-table)))

(define (resolve-meta bits frame-type)
  (case frame-type
    ((read-response write-response)
     (assv-ref-reverse response-table bits))
    ((meta)
     (assv-ref-reverse meta-meta-table bits))
    (else (if (zero? bits)
              'valid
              `(unknown , bits)))))

(define* (candidate-frame? frame #:optional (more 0))
  (>= (bytevector-length frame)
      (+ more frame-min-size)))

(define (with-payload? frame header-size)
  (> (bytevector-length frame)
     header-size))

(define (bitset? a b)
  (not (zero? (logand a b))))

(define (opt-word-size-16? opt)
  (bitset? opt opt:word-size-16))

(define (opt-with-header-crc? opt)
  (bitset? opt opt:with-header-crc))

(define (opt-with-payload-crc? opt)
  (bitset? opt opt:with-payload-crc))

(define (make-options w16? hdcrc? plcrc?)
  (logior (if w16?   opt:word-size-16     0)
          (if hdcrc? opt:with-header-crc  0)
          (if plcrc? opt:with-payload-crc 0)))

(define (even-octet-size? bv)
  (even? (bytevector-length bv)))

;;; Register Protocol Low Level Parser

(define (regp:decode* frame)
  "Low-level parse a binary frame into structured data.

This returns an association list describing the frame. It does not do any
verification as to whether or not the data makes sense, beyond checking frame
sizes as much as it has to.

The function returns #f if there is a low-level issue with the frame that makes
it impossible to parse."
  (if (not (candidate-frame? frame))
      'frame-too-short-basic
      (let* ((motv (bytevector-u16-ref frame 0 'big))
             (meta (bit-extract-width motv 12 4))
             (opts (bit-extract-width motv  8 4))
             (type (bit-extract-width motv  4 4))
             (vers (bit-extract-width motv  0 4))
             (seqno     (bytevector-u16-ref frame 2 'big))
             (address   (bytevector-u32-ref frame 4 'big))
             (blocksize (bytevector-u32-ref frame 8 'big))
             (hdcrcsize (if (opt-with-header-crc? opts) 2 0))
             (plcrcsize (if (opt-with-payload-crc? opts) 2 0))
             (more-size (+ hdcrcsize plcrcsize))
             (header-size (+ 12 more-size)))
        (if (not (candidate-frame? frame more-size))
            'frame-too-short-checksums
            (let* ((hdcrc (if (zero? hdcrcsize)
                              #f
                              (bytevector-u16-ref frame 12 'big)))
                   (plcrc (if (zero? plcrcsize)
                              #f
                              (bytevector-u16-ref frame (+ 12 hdcrcsize)
                                                  'big)))
                   (plcrcraw (and plcrc
                                  (bytevector-slice frame (+ 12 hdcrcsize))))
                   (payload (if (with-payload? frame header-size)
                                (bytevector-slice frame header-size)
                                #f)))
              `((version         . ,vers)
                (type            . ,type)
                (options         . ,opts)
                (meta            . ,meta)
                (sequence-number . ,seqno)
                (address         . ,address)
                (block-size      . ,blocksize)
                (header-crc      . ,(and hdcrc
                                         (make-checksum-pair
                                          hdcrc
                                          (let ((start (checksum frame 12)))
                                            (if plcrcraw
                                                (checksum-more start
                                                               plcrcraw 2)
                                                start)))))
                (payload-crc     . ,(and plcrc
                                         (make-checksum-pair
                                          plcrc
                                          (checksum-all payload))))
                (payload         . ,payload)))))))

;;; Register Protocol High Level Parser

(define (interpret-frame key value old new)
  (cons key (case key
              ((type) (let ((value* (assv-ref-reverse type-table value)))
                        (if value* value*
                            `(unknown ,value))))
              ((options) (match-options value))
              ((meta) (resolve-meta value (assq-ref new 'type)))
              (else value))))

(define (regp:interpret-frame frame)
  "Replace parts of frame by human-readable information."
  (if (symbol? frame)
      frame
      (reverse (fold (lambda (item new-frame)
                       (cons (interpret-frame (car item)
                                              (cdr item)
                                              frame
                                              new-frame)
                             new-frame))
                     '()
                     frame))))

(define (plausible-block-size? size type frame)
  (let* ((options (assq-ref frame 'options))
         (w16? (memq 'word-size-16 options))
         (block-size (assq-ref frame 'block-size)))
    (if w16?
        (and (even? size)
             (= (/ size 2) block-size))
        (= size block-size))))

(define (reserved-options? opts)
  (not (not (memq 'reserved opts))))

(define (frame-may-w16? frame)
  (or (memq (assq-ref frame 'type)
            '(read-request write-request))
      (and (memq (assq-ref frame 'type)
                 '(read-response write-response))
           (eq? (assq-ref frame 'meta) 'acknowledging))))

(define (check-options frame)
  (let* ((options (assq-ref frame 'options))
         (e-reserved (if (reserved-options? options)
                         '(unknown-option) '()))
         (e-w16 (if (frame-may-w16? frame)
                    '() '(invalid-use-of-word-width-16))))
    (append e-reserved e-w16)))

(define (regp:verify-frame frame)
  "Perform tests to verify a frame is valid."
  (if (symbol? frame)
      frame
      (let loop ((rest frame) (clean '()) (errors '()))
        (cond ((and (null? rest) (null? errors))
               (reverse clean))
              ((null? rest) (cons `(errors . ,(reverse
                                               (append (check-options clean)
                                                       errors)))
                                  (reverse clean)))
              (else
               (let* ((original (car rest))
                      (key (car original))
                      (value (cdr original))
                      (rest* (cdr rest)))
                 (case key
                   ((version)
                    (loop rest* (cons original clean)
                          (if (= value implementation-version)
                              errors
                              (cons 'invalid-version errors))))
                   ((type meta)
                    (loop rest*
                          (cons (if (list? value)
                                    (cons key (last value))
                                    original)
                                clean)
                          (if (list? value)
                              (cons (if (eq? key 'type)
                                        'unknown-frame-type
                                        'invalid-meta-value)
                                    errors)
                              errors)))
                   ((header-crc payload-crc)
                    (let ((valid? (test-checksum value)))
                      (loop rest* (cons original clean)
                            (if valid?
                                errors
                                (cons (symbol-append key '-invalid) errors)))))
                   ((payload)
                    (let ((size (if value (bytevector-length value) 0))
                          (type (assq-ref clean 'type)))
                      (if (eq? type 'read-request)
                          (loop rest* (cons original clean)
                                (if (zero? size)
                                    errors
                                    (cons 'read-request-with-payload errors)))
                          (loop rest* (cons original clean)
                                (if (plausible-block-size? size type clean)
                                    errors
                                    (cons 'implausible-block-size errors))))))
                   (else (loop rest* (cons original clean) errors)))))))))

(define (regp:decode frame)
  "Completely decode and verify a binary frame to structured form."
  ((compose regp:verify-frame regp:interpret-frame regp:decode*) frame))

;;; Register Protocol Low Level Encoder

(define* (regp:encode-header #:key
                             (version 0)
                             (type 0)
                             (options 0)
                             (meta 0)
                             (sequence-number 0)
                             (address 0)
                             (block-size 0)
                             header-crc?
                             payload-crc?
                             payload)
  (let ((header (make-bytevector
                 (+ 12 (if header-crc? 2 0) (if payload-crc? 2 0))))
        (motv (logior (set-bits 0 version 4  0)
                      (set-bits 0 type    4  4)
                      (set-bits 0 options 4  8)
                      (set-bits 0 meta    4 12)))
        (hdcrc-offset 12)
        (plcrc-offset (if header-crc? 14 12)))
    (bytevector-u16-set! header 0 motv            'big)
    (bytevector-u16-set! header 2 sequence-number 'big)
    (bytevector-u32-set! header 4 address         'big)
    (bytevector-u32-set! header 8 block-size      'big)
    (when (and payload payload-crc?)
      (let ((payload-crc (checksum-all payload)))
        (bytevector-u16-set! header plcrc-offset payload-crc 'big)))
    (when header-crc?
      (let* ((start (checksum header 12))
             (plcrcraw (and payload-crc?
                            (bytevector-slice header plcrc-offset)))
             (header-crc (if plcrcraw
                             (checksum-more start plcrcraw 2)
                             start)))
        (bytevector-u16-set! header hdcrc-offset header-crc 'big)))
    header))

;;; Register Protocol High Level Encoders

(define* (regp:read-request address block-size
                            #:key
                            (sequence-number 0)
                            word-size-16?
                            header-crc?)
  (regp:encode-header #:type (assq-ref type-table 'read-request)
                      #:options (make-options word-size-16? header-crc? #f)
                      #:sequence-number sequence-number
                      #:address address
                      #:block-size block-size
                      #:header-crc? header-crc?))

(define* (regp:write-request address payload
                             #:key
                             (sequence-number 0)
                             word-size-16?
                             header-crc?
                             payload-crc?)
  (let ((size (bytevector-length payload)))
    (when (and word-size-16? (not (even-octet-size? payload)))
      (throw 'payload-size-not-even size payload))
    (bytevector-append
     (regp:encode-header #:type (assq-ref type-table 'write-request)
                         #:options (make-options word-size-16?
                                                 header-crc?
                                                 payload-crc?)
                         #:sequence-number sequence-number
                         #:address address
                         #:block-size (if word-size-16? (/ size 2) size)
                         #:header-crc? header-crc?
                         #:payload-crc? payload-crc?
                         #:payload payload)
     payload)))

;;; Register Protocol Connection Type

(define-record-type <regp:connection>
  (make-regp-connection* port framing-algorithm framing-state
                         sequence-number word-size-16?
                         with-header-crc? with-payload-crc?)
  regp:connection?
  (port regp:port regp:set-port!)
  (framing-algorithm regp:framing)
  (framing-state regp:framing-state)
  (sequence-number regp:seqno regp:set-seqno!)
  (word-size-16? regp:word-size-16?)
  (with-header-crc? regp:with-header-crc?)
  (with-payload-crc? regp:with-payload-crc?))

(define framing-algorithms
  '(varint-length-prefix classic-slip))

(define (make-regp-connection port framing-algorithm framing-state
                              word-size-16? with-checksums?)
  (unless (member framing-algorithm framing-algorithms)
    (throw 'invalid-framing-algorithm framing-algorithm framing-algorithms))
  (make-regp-connection* port framing-algorithm framing-state 0
                         word-size-16?
                         with-checksums? with-checksums?))

(define* (regp:tcp-connection port #:key word-size-16?)
  (make-regp-connection port 'varint-length-prefix #f word-size-16? #f))

(define* (regp:serial-connection port #:key word-size-16?)
  (make-regp-connection port 'classic-slip
                        ;; tx rx
                        (cons (make-slip-state) (make-slip-state))
                        word-size-16? #t))

;;; Register Protocol Connection Based Operations

(define (regp:recv con)
  (unless (regp:connection? con)
    (throw 'invalid-parameter con))
  (regp:decode (case (regp:framing con)
                 ((varint-length-prefix)
                  (recv-bytevector (regp:port con)))
                 ((classic-slip)
                  (slip-recv (cdr (regp:framing-state con)) (regp:port con)))
                 (else 'invalid-framing-algorithm
                       (regp:framing con)
                       framing-algorithms))))

(define (regp:send con data)
  (unless (regp:connection? con)
    (throw 'invalid-parameter con))
  (case (regp:framing con)
    ((varint-length-prefix)
     (send-bytevector (regp:port con) data))
    ((classic-slip)
     (slip-send (car (regp:framing-state con)) (regp:port con) data))
    (else (throw 'invalid-framing-algorithm
                 (regp:framing con)
                 framing-algorithms))))

(define (regp:request! con data)
  (regp:set-seqno! con (logand (1+ (regp:seqno con)) #xffff))
  (regp:send con data)
  (regp:recv con))

(define (regp:read-request! con address blocksize)
  (unless (regp:connection? con)
    (throw 'invalid-parameter con))
  (regp:request!
   con
   (regp:read-request address blocksize
                      #:sequence-number (regp:seqno con)
                      #:word-size-16?   (regp:word-size-16? con)
                      #:header-crc?     (regp:with-header-crc? con))))

(define (regp:write-request! con address payload)
  (unless (regp:connection? con)
    (throw 'invalid-parameter con))
  (regp:request!
   con
   (regp:write-request address payload
                       #:sequence-number (regp:seqno con)
                       #:word-size-16?   (regp:word-size-16? con)
                       #:header-crc?     (regp:with-header-crc? con)
                       #:payload-crc?    (regp:with-payload-crc? con))))

;;; Register Protocol Miscellaneous Public API

(define (regp:frame-errors frame)
  (assq-ref frame 'errors))

(define (regp:frame-valid? frame)
  (not (regp:frame-errors frame)))

(define (regp:acknowledge? frame)
  (eq? (assq-ref frame 'meta) 'acknowledge))

(define (regp:valid-ack? frame)
  (and (regp:frame-valid? frame)
       (regp:acknowledge? frame)))
