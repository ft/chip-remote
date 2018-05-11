;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote register)
  #:use-module (ice-9 control)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (chip-remote item)
  #:use-module (chip-remote process-plist)
  #:use-module (chip-remote utilities)
  #:export (generate-register
            make-register
            register?
            register-meta
            register-items
            register-default
            register-width
            register-item-names
            register-contains?
            register-address
            register-ref
            register-ref/address
            register-set
            register-fold
            sorted-items
            define-register))

(define-record-type <register>
  (make-register meta items)
  register?
  (meta register-meta)
  (items register-items))

(define-syntax expand-content
  (lambda (x)
    (syntax-case x (=>)
      ((_ => expr) #'expr)
      ((_ exps ...) #'(generate-item exps ...)))))

(define group:contents
  (group 'contents
         #:type 'list
         #:predicate (lambda (x)
                       (memq x '(#:contents #:contents*)))
         #:transformer (lambda (e)
                         (syntax-case e ()
                           ((#:contents (exps ...) ...)
                            #'((expand-content exps ...) ...))
                           ((#:contents* e0 e* ...)
                            #'(e0 e* ...))))))

(define-syntax generate-register
  (lambda (x)
    (syntax-case x ()
      ((kw exp0 expn ...)
       (is-kw? #'exp0)
       (with-syntax (((((contents ...) ...) ((key value) ...))
                      (process-plist #'(exp0 expn ...)
                                     group:contents
                                     (group 'meta))))
         #'(make-register (list (cons key value) ...)
                          (list contents ... ...)))))))

(define-syntax-rule (define-register binding e0 e* ...)
  (define binding (generate-register e0 e* ...)))

(define (register-default reg)
  (let ((default (assq #:default (register-meta reg))))
    (if default
        (cdr default)
        (fold (lambda (x acc)
                ((item-set x) acc (item-default x)))
              0
              (register-items reg)))))

(define (register-width reg)
  (let* ((items (register-items reg))
         (n (length items)))
    (cond ((= n 0) 0)
          ((= n 1) (item-width (car items)))
          (else
           (let loop ((rest items) (biggest-offset 0) (width-of-that 0))
             (if (null? rest)
                 (+ biggest-offset width-of-that)
                 (let* ((this (car rest))
                        (o (item-offset this))
                        (changed? (> o biggest-offset)))
                   (loop (cdr rest)
                         (if changed? o biggest-offset)
                         (if changed? (item-width this) width-of-that)))))))))

(define (register-item-names reg)
  (map item-name (register-items reg)))

(define (register-contains? reg item)
  (!! (memq item (register-item-names reg))))

(define (register-ref reg name)
  (call/ec (lambda (return)
             (register-fold (lambda (item iacc)
                              (if (eq? (item-name item)
                                       name)
                                  (return item)
                                  #f))
                            #f reg))))

(define (register-set reg regval item itemval)
  (let ((item (register-ref reg item)))
    (unless item
      (throw 'unknown-register-item item reg))
    ((item-set item) regval itemval)))

(define (register-fold fnc init reg)
  (fold fnc init (register-items reg)))

(define (sorted-items reg)
  (sort (register-items reg)
        (lambda (a b)
          (< (item-offset a)
             (item-offset b)))))

(define register-address
  (case-lambda
    ((reg n) (list-ref (sorted-items reg) n))
    ((reg name n)
     (call/ec (lambda (return)
                (let loop ((rest (sorted-items reg)) (cnt 0))
                  (cond ((null? rest) #f)
                        ((eq? name (item-name (car rest)))
                         (if (= cnt n)
                             (return (car rest))
                             (loop (cdr rest) (+ cnt 1))))
                        (else (loop (cdr rest) cnt)))))))))

(define (register-ref/address reg thing)
  (cond ((symbol? thing) (register-ref reg thing))
        ((integer? thing) (register-address reg thing))
        (else (throw 'invalid-item-address thing))))
