;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote combination)
  #:use-module (ice-9 control)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote modify)
  #:use-module (chip-remote device)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote process-plist)
  #:use-module (chip-remote item)
  #:export (make-combination
            combination?
            cmb-parts
            cmb-order
            cmb-combine
            cmb-partition
            cmb-semantics
            cmb-validator
            cmb-encode
            cmb-decode
            make-part
            part?
            part-address
            part-transform-from
            part-transform-into))

(define (either pred lst)
  (call/ec (lambda (return)
             (let loop ((rest lst))
               (if (null? (cdr rest))
                   (pred (car rest))
                   (if (pred (car rest))
                       (return #t)
                       (loop (cdr rest))))))))

(define (all pred lst)
  (call/ec (lambda (return)
             (let loop ((rest lst))
               (if (null? (cdr rest))
                   (pred (car rest))
                   (if (not (pred (car rest)))
                       (return #f)
                       (loop (cdr rest))))))))

(define (list-of-list-of-integers? v)
  (and (list? v)
       (all list-of-integers? v)))

(define (list-of-integers? v)
  (and (list? v)
       (all integer? v)))

(define (value-fits-target? target value)
  (cond ((or (device? target)
             (page-map? target))
         (list-of-list-of-integers? value))
        ((register-map? target)
         (list-of-integers? value))
        ((register? target) (integer? value))
        (else (throw 'unknown-target target))))

(define (find-register-value target addr value)
  (unless (value-fits-target? target value)
    (throw 'invalid-value-for-target target value))
  (cond ((device? target)
         (find-register-value-for (device-page-map target) addr value))
        ((page-map? target)
         (let loop ((rms (page-map-table target))
                    (vals value))
           (cond ((null? rms) #f)
                 ((null? vals) #f)
                 ((regmap-matches? (car rms) addr)
                  (find-register-value-for (cdar rms)
                                           (if (list? addr) (cdr addr) addr)
                                           (car vals)))
                 (else (loop (cdr rms) (cdr vals))))))
        ((register-map? target)
         (let loop ((regs (register-map-table target))
                    (vals value))
           (cond ((null? regs) #f)
                 ((null? vals) #f)
                 ((register-matches? (car regs) addr)
                  (car vals))
                 (else (loop (cdr regs) (cdr vals))))))
        ;; This falls down here if target is a register.
        (else value)))

(define-immutable-record-type <part>
  (make-part* address name from into)
  part?
  (address part-address)
  (name part-name)
  (from part-transform-from)
  (into part-transform-into))

(define (convert-to-string x)
  (cond ((integer? x) (number->string x 16))
        ((symbol? x) (symbol->string x))
        ((string? x) x)
        ((eq? x #f) "*")
        (else (throw 'invalid-type x))))

(define (build-name-from address)
  (string->symbol (string-join (map convert-to-string address) "-")))

(define (guess-name-from address)
  (if (symbol? address)
      address
      (let ((sym (call/ec (lambda (return)
                            (let loop ((rest address))
                              (cond ((null? rest) #f)
                                    ((symbol? (car rest)) (return (car rest)))
                                    (else (loop (cdr rest)))))))))
        (if (symbol? sym)
            sym
            (build-name-from address)))))

(define* (make-part address #:key (name #f) (from identity) (into identity))
  (make-part* address (or name (guess-name-from address)) from into))

(define-immutable-record-type <combination>
  (make-combination* parts order
                     combine partition
                     semantics validator
                     encode decode)
  combination?
  (parts cmb-parts)
  (order cmb-order)
  (combine cmb-combine)
  (partition cmb-partition)
  (semantics cmb-semantics)
  (validator cmb-validator)
  (encode cmb-encode)
  (decode cmb-decode))

(define* (make-combination parts
                           #:key
                           (order #f)
                           (combine #f)
                           (partition #f)
                           (semantics #f)
                           (validator #f)
                           (encode #f)
                           (decode #f))
  (make-combination* parts (or order (map part-name parts))
                     combine partition
                     semantics validator
                     encode decode))

(define-syntax generate-combination
  (lambda (x)
    (syntax-case x ()
      ((kw exp0 expn ...)
       (is-kw? #'exp0)
       (with-syntax ((((parts ...)
                       (order ...)
                       combine partition
                       (semantics ...) (validator ...)
                       encode decode
                       (meta ...))
                      (process-plist #'(exp0 expn ...)
                                     parts-group
                                     order-group
                                     (scalar-group 'combine)
                                     (scalar-group 'partition)
                                     semantics-group
                                     validator-group
                                     (scalar-group 'encode)
                                     (scalar-group 'decode)
                                     (group 'meta))))
         #'(make-combination (parts ...) (order ...)
                             combine partition
                             (semantics ...) (validator ...)
                             encode decode))))))

(define (combine target cmb value)
  0)
