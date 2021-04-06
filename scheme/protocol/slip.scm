;; Copyright (c) 2019-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; This module implements encoders and decoders for the SLIP framing protocol
;; as described in RFC1055, plus common extensions to encode start-of-frame
;; positions.

(define-module (protocol slip)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-slip-encoding make-slip-state slip-decode! slip-encode))

;; RFC-1055 suggests the use of EOF to mark starts of frames as well. So that's
;; what we're doing by default, if with-sof? is set.
(define *slip-default-encoding*
  '((escape         #xdb . #xdd)
    (start-of-frame #xc0 . #xdc)
    (end-of-frame   #xc0 . #xdc)))

(define-immutable-record-type <slip-encoding>
  (make-slip-encoding* esc esc-esc sof esc-sof eof esc-eof with-sof?)
  slip-encoding?
  (esc esc) (esc-esc esc-esc)
  (sof sof) (esc-sof esc-sof)
  (eof eof) (esc-eof esc-eof)
  (with-sof? with-sof?))

(define* (make-slip-encoding #:key
                             (escape (assq-ref *slip-default-encoding*
                                               'escape))
                             (end-of-frame (assq-ref *slip-default-encoding*
                                                     'end-of-frame))
                             (start-of-frame (assq-ref *slip-default-encoding*
                                                       'start-of-frame))
                             (with-sof? #f))
  (make-slip-encoding* (car escape)
                       (cdr escape)
                       (car start-of-frame)
                       (cdr start-of-frame)
                       (car end-of-frame)
                       (cdr end-of-frame)
                       with-sof?))

(define-record-type <slip-state>
  (make-slip-state* state encoding input output current)
  slip-state?
  (state slip-state set-slip-state!)
  (encoding slip-encoding)
  (input slip-input set-slip-input!)
  (output slip-output set-slip-output!)
  (current slip-current set-slip-current!))

(define* (make-slip-state #:key
                          (encoding (make-slip-encoding))
                          (input '())
                          (output '())
                          (current '()))
  (make-slip-state* (if (with-sof? encoding) 'find-sof 'normal)
                    encoding input output current))

(define (slip-input! state data)
  (set-slip-input! state (append (slip-input state)
                                 (if (list? data)
                                     data
                                     (list data)))))

(define (slip-get! state)
  (let ((rv (slip-output state)))
    (set-slip-output! state '())
    rv))

(define-immutable-record-type <procbuf>
  (make-process-buffer* buffer index length)
  procbuf?
  (buffer procbuf-get)
  (index procbuf-index set-procbuf-index)
  (length procbuf-length))

(define (make-process-buffer buffer)
  (make-process-buffer* buffer 0 (bytevector-length buffer)))

(define (procbuf-done? buffer)
  (>= (procbuf-index buffer) (procbuf-length buffer)))

(define (procbuf-advance buffer)
  (set-procbuf-index buffer (1+ (procbuf-index buffer))))

(define (slip-transition state event with-sof?)
  ;; Possible events: end escape lost-end bogus normal ignore
  (case state
    ((normal) (case event
                ((end) (if with-sof? 'find-sof 'normal))
                ((escape) 'escape)
                (else 'normal)))
    ((escape) 'normal)
    ((find-sof) (case event
                  ((normal lost-end) 'normal)
                  (else 'find-sof)))))

(define (decode-escaped enc octet)
  (cond ((= octet (esc-eof enc)) (eof enc))
        ((= octet (esc-esc enc)) (esc enc))
        ((and (with-sof? enc) (= octet (esc-sof enc))) (sof enc))
        (else #f)))

(define (process-octet state enc octet)
  (case state
    ((normal) (cond ((= octet (eof enc)) 'end)
                    ((= octet (esc enc)) 'escape)
                    ((and (with-sof? enc) (= octet (sof enc))) 'lost-end)
                    (else octet)))
    ((escape) (or (decode-escaped enc octet) 'bogus-escape))
    ((find-sof) (if (= octet (sof enc)) 'normal 'ignore))))

(define (process-buffer! state buffer)
  (define (proc s o)
    (process-octet s (slip-encoding state) o))
  (let loop ((s (slip-state state))
             (current (slip-current state))
             (pb (make-process-buffer buffer))
             (acc '()))
    (if (procbuf-done? pb)
        (begin (set-slip-current! state current)
               (set-slip-state! state s)
               (reverse acc))
        (let ((next (proc s (bytevector-u8-ref buffer (procbuf-index pb)))))
          (loop (slip-transition s next (with-sof? (slip-encoding state)))
                (cond ((eq? next 'end) '())
                      ((symbol? next) current)
                      (else (cons next current)))
                (procbuf-advance pb)
                (if (eq? next 'end)
                    (cons (u8-list->bytevector (reverse current)) acc)
                    acc))))))

(define (process-buffers! state input)
  (let loop ((rest input) (acc '()))
    (if (null? rest)
        (begin (set-slip-input! state '())
               acc)
        (loop (cdr input) (append acc (process-buffer! state (car input)))))))

(define (slip-process! state)
  (set-slip-output! state (append (slip-output state)
                                  (process-buffers! state (slip-input state)))))

(define (slip-decode! state data)
  (slip-input! state data)
  (slip-process! state)
  (slip-get! state))

(define (encode-octet state octet)
  (let ((enc (slip-encoding state)))
    (cond ((= octet (eof enc)) (list (esc enc) (esc-eof enc)))
          ((= octet (esc enc)) (list (esc enc) (esc-esc enc)))
          ((and (with-sof? enc) (= octet (sof enc)))
           (list (esc enc) (esc-sof enc)))
          (else (list octet)))))

(define (enclose! state data)
  (let* ((enc (slip-encoding state))
         (with-eof (append! data (list (eof enc)))))
    (if (with-sof? enc)
        (cons (sof enc) with-eof)
        with-eof)))

(define (slip-encode-bv state data)
  (let ((len (bytevector-length data)))
    (let loop ((n 0) (acc '()))
      (if (<= len n)
          acc
          (loop (1+ n)
                (append! acc (encode-octet state (bytevector-u8-ref data n))))))))

(define* (slip-encode state data #:key (full-frame? #t))
  (let loop ((rest (if (list? data) data (list data))) (acc '()))
    (if (null? rest)
        (u8-list->bytevector (if full-frame? (enclose! state acc) acc))
        (loop (cdr rest) (append! acc (slip-encode-bv state (car rest)))))))
