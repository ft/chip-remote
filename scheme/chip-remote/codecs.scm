;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote codecs)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote named-value)
  #:export (decode-boolean
            decode-boolean/active-low
            encode-boolean
            encode-boolean/active-low
            decode-state
            decode-state/active-low
            encode-state
            encode-state/active-low
            decode-ones-complement
            encode-ones-complement
            decode-twos-complement
            encode-twos-complement
            decode-offset-binary
            encode-offset-binary
            decode-sign-magnitude
            encode-sign-magnitude
            decode-with-table
            make-table-decoder
            encode-with-table
            make-table-encoder))

;; Boolean codecs

(define (true-ish x)
  (or (eq? x #t)
      (and (number? x)
           (= x 1))))

(define (false-ish x)
  (or (eq? x #f)
      (and (number? x)
           (= x 0))))

(define (invert-bit x)
  (logxor x 1))

(define (boolean-encoder datum t f tf exception)
  (cond ((or (eq? datum t) (true-ish datum)) (tf 1))
        ((or (eq? datum f) (false-ish datum)) (tf 0))
        (else (throw exception datum t f tf))))

(define (boolean-decoder datum t f tf exception)
  (let ((x (tf datum)))
    (cond ((= x 1) t)
          ((= x 0) f)
          (else (throw exception datum t f tf)))))

(define (encode-state* x tf)
  (boolean-encoder x 'enabled 'disabled tf 'invalid-state))

(define (encode-state x)
  (encode-state* x identity))

(define (encode-state/active-low x)
  (encode-state* x invert-bit))

(define (decode-state* x tf)
  (boolean-decoder x 'enabled 'disabled tf 'invalid-state))

(define (decode-state x)
  (decode-state* x identity))

(define (decode-state/active-low x)
  (decode-state* x invert-bit))

(define (encode-boolean* x tf)
  (boolean-encoder x 'true 'false tf 'invalid-boolean))

(define (encode-boolean x)
  (encode-boolean* x identity))

(define (encode-boolean/active-low x tf)
  (encode-boolean* x invert-bit))

(define (decode-boolean* x tf)
  (boolean-decoder x 'true 'false tf 'invalid-boolean))

(define (decode-boolean x)
  (decode-boolean* x identity))

(define (decode-boolean/active-low x)
  (decode-boolean* x invert-bit))

;; Integer codecs. For a nice summary of common signed integer encodings (un-
;; signed ones are easy, because that's just the identity function) take a look
;; at this application note from Intersil:
;;
;; https://www.intersil.com/content/dam/Intersil/documents/an96/an9657.pdf
;;
;; All encodings need to know the width of an item to work.

(define (encode-twos-complement width value)
  (if (>= value 0)
      value
      (+ 1 (logxor (one-bits width) (* -1 value)))))

(define (decode-twos-complement width value)
  (let* ((top-bit (ash 1 (- width 1)))
         (rest (- top-bit 1)))
    (if (< value top-bit)
        value
        (* -1 (+ 1 (logxor rest (logand rest value)))))))

(define (encode-ones-complement width value)
  (if (>= value 0)
      value
      (logxor (one-bits width) (* -1 value))))

(define (decode-ones-complement width value)
  (let* ((top-bit (ash 1 (- width 1)))
         (rest (- top-bit 1)))
    (if (< value top-bit)
        value
        (* -1 (logxor rest (logand rest value))))))

(define (encode-sign-magnitude width value)
  (if (negative? value)
      value
      (let ((top-bit (ash 1 (- width 1))))
        (logior top-bit value))))

(define (decode-sign-magnitude width value)
  (let* ((w (- width 1))
         (v (bit-extract-width value 0 w)))
    (if (zero? (bit-extract-width value w 1))
        (* -1 v)
        v)))

(define (encode-offset-binary width value)
  (let ((half (ash 1 (- width 1))))
    (+ value half)))

(define (decode-offset-binary width value)
  (let ((half (ash 1 (- width 1))))
    (- value half)))

;; Table lookup based codecs

(define (make-table-decoder table)
  (lambda (x) (decode-with-table table x)))

(define (decode-with-table table value)
  (let loop ((rest (if (named-value? table) (value-data table) table)))
    (if (null? rest)
        'undefined
        (let ((k (caar rest))
              (v (cdar rest)))
          (if (= value v)
              k
              (loop (cdr rest)))))))

(define (make-table-encoder table)
  (lambda (x) (encode-with-table table x)))

(define (encode-with-table table key)
  (let ((value (assoc key table)))
    (if value
        (cdr value)
        'undefined)))
