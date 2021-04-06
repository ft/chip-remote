;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (documentation module)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 session)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (documentation more)
  #:use-module (documentation module constants)
  #:use-module (documentation module generic)
  #:export (module->documentation))

(define (string->s-exp str)
  (with-input-from-string str
    (lambda ()
      (read (current-input-port)))))

(define (module->documentation mod)
  (sort (module->documentation* (or (and (string? mod) (string->s-exp mod)) mod))
        (lambda (a b)
          (let ((a-name (symbol->string (car a)))
                (b-name (symbol->string (car b))))
            (string< a-name b-name)))))

(define (module->documentation* mod)
  (let ((inf (module-obarray (resolve-interface mod))))
    (map (lambda (x) (process-interface mod x))
         (hash-map->list cons inf))))

(define (namespace-matches? ns mod)
  (let ((ns-n (length ns))
        (mod-n (length mod)))
    (and (>= mod-n ns-n)
         (equal? ns (take mod ns-n)))))

(define (expand-unknown name)
  (list name 'unknown-datum))

(define expanders
  `((,procedure? ,expand-procedure)
    (,macro? ,expand-macro)
    (,integer? ,expand-constants-integer (xmms2 constants))
    (,integer? ,expand-integer)))

(define (expand-for-value mod name value)
  (let loop ((rest expanders))
    (if (null? rest)
        (expand-unknown name)
        (let-values (((predicate? expander)
                      (match (car rest)
                        ((predicate? expander namespace)
                         (values (lambda (mod name value)
                                   (and (predicate? value)
                                        (namespace-matches? namespace mod)))
                                 expander))
                        ((predicate? expander)
                         (values (lambda (mod name value)
                                   (predicate? value))
                                 expander)))))
          (if (predicate? mod name value)
              (expander mod name value)
              (loop (cdr rest)))))))

(define (process-interface mod item)
  (let* ((name (car item))
         (value (cdr item)))
    (if (and (variable? value) (not (variable-bound? value)))
        (list name 'unbound-parameter)
        (expand-for-value mod name (if (variable? value)
                                       (variable-ref value)
                                       value)))))
