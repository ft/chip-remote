;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote validate)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote item)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:export (predicates
            make-validator
            generate-validator
            define-validator
            combine-validator
            identify-validator
            validator?
            validator-name
            validator-type
            validator-expression
            validator-predicate
            validate-item-value))

(define-syntax-rule (predicates (op bound) ...)
  (lambda (x) (and (op x bound) ...)))

(define-immutable-record-type <validator>
  (make-validator name combination type expression predicate)
  validator?
  (name validator-name identify-validator)
  (combination validator-combination combine-validator)
  (type validator-type)
  (expression validator-expression)
  (predicate validator-predicate))

(define-syntax generate-validator
  (lambda (x)
    (define (elem-of-mode? expr)
      (let ((sym (syntax->datum expr)))
        (or (equal? sym 'element-of)
            (equal? sym '∈))))
    (define (not-elem-of-mode? expr)
      (let ((sym (syntax->datum expr)))
        (or (equal? sym 'not-element-of)
            (equal? sym '∉))))
    (syntax-case x (range interpreter scheme)
      ((_ range expr ...)
       #'(make-validator #f #f 'range '(expr ...)
                         (predicates expr ...)))
      ((_ elem-of expr ...)
       (elem-of-mode? #'elem-of)
       #'(make-validator #f #f 'element-of '(expr ...)
                         (lambda (x)
                           (not (not (member x '(expr ...)))))))
      ((_ not-elem-of expr ...)
       (not-elem-of-mode? #'not-elem-of)
       #'(make-validator #f #f 'not-element-of '(expr ...)
                         (lambda (x)
                           (not (member x '(expr ...))))))
      ((_ interpreter expr)
       #'(make-validator #f #f 'interpreter #f
                         (make-evaluation expr)))
      ((_ scheme expr)
       #'(make-validator #f #f 'scheme #f expr)))))

(define-syntax-rule (define-validator binding expr expr* ...)
  (define binding
    (identify-validator (generate-validator expr expr* ...)
                        'binding)))

(define (unsigned-integer-max width) (- (2e width) 1))
(define (unsigned-integer-min width) 0)
(define (offset-binary-min width) (* -1 (2e width)))
(define offset-binary-max unsigned-integer-max)
(define (ones-complement-min width) (* -1 (unsigned-integer-max width)))
(define ones-complement-max unsigned-integer-max)
(define twos-complement-min offset-binary-min)
(define twos-complement-max unsigned-integer-max)
(define signed-magnitude-min ones-complement-min)
(define signed-magnitude-max unsigned-integer-max)

(define (int-validate width min* max*)
  (lambda (x)
    (and (integer? x)
         (>= x (min* width))
         (<= x (max* width)))))

(define (semantics->validator width semantics)
  (let ((type (semantics-type semantics)))
    (case type
      ((boolean boolean/active-low)
       (lambda (x)
         (or (boolean-true? x)
             (boolean-false? x))))
      ((table-lookup)
       (lambda (x)
         (not (not (member x (map car (semantics-data semantics)))))))
      ((unsigned-integer)
       (int-validate width unsigned-integer-min unsigned-integer-max))
      ((ones-complement)
       (int-validate width ones-complement-min ones-complement-max))
      ((twos-complement)
       (int-validate width twos-complement-min twos-complement-max))
      ((offset-binary)
       (int-validate width offset-binary-min offset-binary-max))
      ((signed-magnitude)
       (int-validate width signed-magnitude-min signed-magnitude-max))
      (else (lambda (x) #t)))))

(define (validator-or-true validator)
  (if (validator? validator)
      (validator-predicate validator)
      (lambda (x) #t)))

(define (combination-or-semantic validator)
  (if (validator? validator)
      (validator-combination validator)
      'semantic))

(define (validate-item-value item value)
  (let* ((width (item-width item))
         (semantics (item-semantics item))
         (semantic-predicate (semantics->validator width semantics))
         (validator (item-validator item))
         (predicate (validator-or-true validator))
         (combination (combination-or-semantic validator)))
    (case combination
      ((semantic) (semantic-predicate value))
      ((and) (and (semantic-predicate value) (predicate value)))
      ((or) (or (semantic-predicate value) (predicate value)))
      (else (predicate value)))))
