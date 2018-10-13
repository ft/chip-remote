;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (documentation more)
  #:use-module (ice-9 documentation)
  #:export (inlinable?
            add-macro-docstring
            define-variable
            maybe-topdir
            variable-documentation))

(define (inlinable? mod name)
  (catch #t
    (lambda ()
      (eval-string (symbol->string name) (resolve-module mod)))
    (lambda (k . a)
      #f)))

(define (add-macro-docstring name docstring)
  (let ((var (module-variable (current-module)
                              name)))
    (set-procedure-property! (macro-transformer (variable-ref var))
                             'documentation
                             docstring)))

(define-syntax define-variable
  (lambda (x)
    (syntax-case x ()
      ((kw name value docstring)
       #'(begin (define name value)
                (set-object-property! (module-variable (current-module)
                                                       'name)
                                      'documentation
                                      docstring))))))

(define (variable-documentation mod name)
  (catch #t
    (lambda ()
      (or (object-property (module-variable (resolve-module mod) name)
                           'documentation)
          'undocumented))
    (lambda (k . a)
      'undocumented)))

(define (maybe-topdir fallback)
  (or (getenv "TOPDIR") fallback))
