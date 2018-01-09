;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote modify)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (chip-remote device)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote item)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote interpreter)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote validate)
  #:export (modify modify* chain-modify chain-modify*))

(define (not-integer? x)
  (not (integer? x)))

(define (modify-register reg init addr value)
  ;;(format #t "debug: ~a ~a ~a~%" init addr value)
  (let ((item (match addr
                (((? integer? i)) (register-address reg i))
                ((? integer? i) (register-address reg i))
                (((? not-integer? name)) (register-ref reg name))
                ((n (? integer? i)) (register-address reg n i))
                ((? not-integer? name) (register-ref reg name))
                (_ (throw 'unknown-addressing-scheme addr)))))
    (unless (item? item)
      (throw 'addressing-returned-non-item item addr))
    ((item-set item) init (item-encode item value))))

(define (register-matches? reg addr)
  (match addr
    ((ra . rest) (eqv? ra (car reg)))
    (name (register-ref (cdr reg) name))
    (_  #f)))

(define (regmap-matches? regmap addr)
  (match addr
    ((rma . rest) (eqv? rma (car regmap)))
    (name (register-map-ref (cdr regmap) name))
    (_  #f)))

(define (modify-register-map rm init addr value)
  (map (lambda (r v)
         (if (register-matches? r addr)
             (modify-register (cdr r) v
                              (if (list? addr) (cdr addr) addr)
                              value)
             v))
       (register-map-table rm)
       init))

(define (modify-page-map pm init addr value)
  (map (lambda (rm v)
         (if (regmap-matches? rm addr)
             (modify-register-map (cdr rm) v
                                  (if (list? addr) (cdr addr) addr)
                                  value)
             v))
       (page-map-table pm)
       init))

(define (modify-device dev init addr value)
  (modify-page-map (device-page-map dev) init addr value))

(define (modify target init address value)
  ((cond ((register? target) modify-register)
         ((register-map? target) modify-register-map)
         ((page-map? target) modify-page-map)
         ((device? target) modify-device)
         (else (throw 'invalid-target target init address value)))
   target init address value))

(define (default-by-target target)
  ((cond ((register? target) register-default)
         ((register-map? target) register-map-default)
         ((page-map? target) page-map-default)
         ((device? target) device-default)
         (else (throw 'invalid-target target)))
   target))

(define (modify* target address value)
  (modify target (default-by-target target) address value))

(define (chain-modify target init . lst)
  (if (null? lst)
      init
      (apply chain-modify
             (cons target
                   (cons (modify target init (caar lst) (cadar lst))
                         (cdr lst))))))

(define (chain-modify* target . lst)
  (apply chain-modify (cons target (cons (default-by-target target) lst))))
