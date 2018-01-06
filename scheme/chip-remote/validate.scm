;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote validate)
  #:use-module (srfi srfi-9 gnu)
  #:export (predicates
            make-validator
            generate-validator
            define-validator
            identify-validator
            validator?
            validator-name
            validator-type
            validator-expression
            validator-predicate))

(define-syntax-rule (predicates (op bound) ...)
  (lambda (x) (and (op x bound) ...)))

(define-immutable-record-type <validator>
  (make-validator name type expression predicate)
  validator?
  (name validator-name identify-validator)
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
       #'(make-validator #f 'range '(expr ...)
                         (predicates expr ...)))
      ((_ elem-of expr ...)
       (elem-of-mode? #'elem-of)
       #'(make-validator #f 'element-of '(expr ...)
                         (lambda (x)
                           (not (not (member x '(expr ...)))))))
      ((_ not-elem-of expr ...)
       (not-elem-of-mode? #'not-elem-of)
       #'(make-validator #f 'not-element-of '(expr ...)
                         (lambda (x)
                           (not (member x '(expr ...))))))
      ((_ interpreter expr)
       #'(make-validator #f 'interpreter #f
                         (make-evaluation expr)))
      ((_ scheme expr)
       #'(make-validator #f 'scheme #f expr)))))

(define-syntax-rule (define-validator binding expr expr* ...)
  (define binding
    (identify-validator (generate-validator expr expr* ...)
                        'binding)))
