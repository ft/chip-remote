;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote validate)
  #:use-module (srfi srfi-9)
  #:export (predicates
            make-validator
            generate-validator
            define-validator
            validator?
            validator-type
            validator-expression
            validator-predicate))

(define-syntax-rule (predicates (op bound) ...)
  (lambda (x) (and (op x bound) ...)))

(define-record-type <validator>
  (make-validator type expression predicate)
  validator?
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
       #'(make-validator 'range '(expr ...)
                         (predicates expr ...)))
      ((_ elem-of expr ...)
       (elem-of-mode? #'elem-of)
       #'(make-validator 'element-of '(expr ...)
                         (lambda (x)
                           (not (not (member x '(expr ...)))))))
      ((_ not-elem-of expr ...)
       (not-elem-of-mode? #'not-elem-of)
       #'(make-validator 'not-element-of '(expr ...)
                         (lambda (x)
                           (not (member x '(expr ...))))))
      ((_ interpreter expr)
       #'(make-validator 'interpreter #f
                         (make-evaluation expr)))
      ((_ scheme expr)
       #'(make-validator 'scheme #f expr)))))

(define-syntax-rule (define-validator binding expr ...)
  (define binding (generate-validator expr ...)))
