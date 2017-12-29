;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote register)
  #:use-module (ice-9 control)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote item)
  #:use-module (chip-remote process-plist)
  #:export (generate-register
            make-register
            register?
            register-meta
            register-items
            register-defaults
            register-width
            register-item-names
            register-contains?
            register-ref
            register-set
            register-fold
            define-register))

(define-record-type <register>
  (make-register meta items)
  register?
  (meta register-meta)
  (items register-items))

(define group:contents
  (group 'contents
         #:type 'list
         #:predicate (lambda (x)
                       (memq x '(#:contents #:contents*)))
         #:transformer (lambda (e)
                         (syntax-case e ()
                           ((#:contents (exps ...) ...)
                            #'((generate-item exps ...) ...))
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

(define (record-register-printer register port)
  (format port "<register:~%    #:meta~%")
  (pretty-print (register-meta register) port #:per-line-prefix "    ")
  (format port "    #:items~%")
  (pretty-print (register-items register) port #:per-line-prefix "    ")
  (format port ">"))

(set-record-type-printer! <register> record-register-printer)

(define (register-defaults reg)
  (fold (lambda (x acc)
          ((item-set x) acc (item-default x)))
        0
        (register-items reg)))

(define (register-width reg)
  (let loop ((rest (register-items reg)) (biggest-offset 0) (width-of-that 0))
    (if (null? rest)
        (+ biggest-offset width-of-that)
        (let* ((this (car rest))
               (o (item-offset this))
               (changed? (> o biggest-offset)))
          (loop (cdr rest)
                (if changed? o biggest-offset)
                (if changed? (item-width this) width-of-that))))))

(define (register-item-names reg)
  (map item-name (register-items reg)))

(define (register-contains? reg item)
  (not (not (memq item (register-item-names reg)))))

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

;; TODO: register-verify?:
;;
;;   - No overlaps in between items.
;;   - No names used multiple times.
;;   - No missing regions in register.
