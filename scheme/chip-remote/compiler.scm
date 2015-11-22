;; Copyright (c) 2015 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote compiler)
  #:use-module (srfi srfi-11)
  #:export (analyse-register-map
            determine-file-name
            file-in-load-path
            read-file))

;; This is a compiler front-end for the register map DSL of chip-remote. From
;; a single register map definition, this allows to define compilers for
;; multiple purposes:
;;
;;  - Generate first level access
;;  - Generate second level access
;;  - Generate third level access
;;  - Generate decoder code
;;  - Implement compiler backends that target completely different
;;    purposes, like C code to access a chip using a given micro-
;;    controller plattform.
;;
;; This removes the need for any external script files that produce inter-
;; mediate code, that the developer has to manually adjust to serve the
;; intended purpose (which is a major maintanance headache).
;;
;; The idea is to make the DSL powerful enough to tackle all common situations
;; with many configurable chips that are available and to allow the compiler to
;; be user-extensible to allow for the user to create special-purpose tools
;; when a chip uses a particularly weird approach.
;;
;; Everything that can be deduced from and thus have code generated from a
;; register map should be done automatically. Developers should only have to
;; implement a minimal amount of functionality on their own.

;; The front-end is a set of utilities, defined in this ‘eval-when’, so they
;; are available in functions as well as macros of back-ends:

(eval-when (expand load eval)

  ;; Utilities

  (define (syntax-keyword? sym)
    (keyword? (syntax->datum sym)))

  (define (alist-change is-equal? alist key value)
    "Change an existing value in an association list.

IS-EQUAL? is a binary function used to compare keys of the association list
ALIST against KEY. If KEY is found in ALIST in terms of IS-EQUAL?, a new
association list is returned where the value of KEY is replaced with VALUE.

If KEY is not found in ALIST, #f is returned."
    (let loop ((rest alist) (acc '()))
      (if (null? rest)
          #f
          (let* ((this (car rest))
                 (this-key (car this)))
            (if (is-equal? key this-key)
                (append acc (list (cons key value)) (cdr rest))
                (loop (cdr rest) (append acc (list this))))))))

  (define (alist-add alist key value)
    "Add key KEY with value VALUE to the association list ALIST.

This function does *not* check whether KEY is part of ALIST, it
conconditionally adds the key/value pair."
    (cons (cons key value)
          alist))

  (define (alist-change-or-add is-equal? alist key value)
    "Returns a new association list based on ALIST, where the value of KEY is
VALUE.

See `alist-change' for details about this function's arguments.

The difference between this and `alist-change' is, that in case KEY is not part
of ALIST, *this* function *adds* the key/value pair indicated by KEY and VALUE."
    (or (alist-change is-equal? alist key value)
        (alist-add alist key value)))

  (define (read-file keyword filename)
    "Opens FILENAME, reads all expressions from it and returns them as
syntax-objects in the context of KEYWORD."
    (let ((fileport (open-input-file filename)))
      (let loop ((expr (read fileport)))
        (if (eof-object? expr)
            (begin (close-input-port fileport)
                   '())
            (cons (datum->syntax keyword expr)
                  (loop (read fileport)))))))

  (define (change-section state section is-equal? key value)
    (let ((sec (assq-ref state section)))
      (alist-change-or-add eq? state section
                           (alist-change-or-add is-equal? sec key value))))

  (define (change-meta state key value)
    (change-section state 'meta eq? key value))

  (define (clean-up-section state section cleaner)
    (let ((sec (assq-ref state section)))
      (alist-change-or-add eq? state section (cleaner sec))))

  (define (clean-up state)
    (let ((cleaners `((registers . ,(lambda (x)
                                      (sort x (lambda (a b)
                                                (< (car a) (car b))))))
                      (meta . ,(lambda (x)
                                 (sort x (lambda (a b)
                                           (let ((sa (symbol->string (car a)))
                                                 (sb (symbol->string (car b))))
                                             (string< sa sb)))))))))
      (let loop ((rest cleaners) (clean-state state))
        (if (null? rest)
            clean-state
            (loop (cdr rest) (clean-up-section clean-state
                                               (caar rest)
                                               (cdar rest)))))))

  (define (file-in-load-path lp lst ext)
    (let ((fname (string-concatenate
                  (list (string-join (map symbol->string lst)
                                     file-name-separator-string
                                     'infix)
                        ext))))
      (let loop ((rest lp))
        (if (null? rest)
            (throw 'no-such-file-in-load-path
                   (cons 'load-path lp)
                   (cons 'file-list lst)
                   (cons 'file-name fname))
            (let ((full-name (string-concatenate
                              (list (car rest)
                                    file-name-separator-string
                                    fname))))
              (if (file-exists? full-name)
                  full-name
                  (loop (cdr rest))))))))

  (define (determine-file-name file)
    (cond ((string? file)
           (unless (file-exists? file)
             (throw 'no-such-file file))
           file)
          ((list? file)
           (file-in-load-path %load-path file ".map"))
          (else (throw 'unknown-type-for-file file))))

  ;; Keyword-specific utilities

  (define (add-register state idx data)
    (change-section state 'registers = idx data))

  (define (initial-dsl-state)
    (list (list 'meta
                (cons 'name #f)
                (cons 'manufacturer #f)
                (cons 'type #f)
                (cons 'datasheet-code #f)
                (cons 'datasheet-uri #f)
                (cons 'register-width 'dynamic)
                (cons 'default-value #f)
                (cons 'default-post-processor #f))
          (list 'registers)
          (list 'banks)
          (list 'encoders)
          (list 'decoders)
          (list 'dependencies)
          (list 'combinations)))

  (define (register/contents lst)
    (let loop ((rest lst) (acc '()))
      (syntax-case rest ()
        (() (values rest (reverse acc)))
        ((sym e* ...) (syntax-keyword? #'sym)
         (values rest (reverse acc)))
        ((e0 e* ...)
         (loop #'(e* ...)
               (cons (let next-part ((rest #'e0) (acc '()))
                       (syntax-case rest ()
                         (() (reverse acc))
                         ((#:offset value e* ...)
                          (next-part #'(e* ...)
                                     (alist-change-or-add eq? acc
                                                          'offset
                                                          #'value)))
                         ((#:width value e* ...)
                          (next-part #'(e* ...)
                                     (alist-change-or-add eq? acc
                                                          'width
                                                          #'value)))
                         ((name e* ...) (null? acc)
                          (next-part #'(e* ...)
                                     (alist-change-or-add eq? acc
                                                          'name
                                                          #'name)))))
                     acc))))))

  (define (encoder-constraints lst)
    (syntax-case lst (=>)
      ((name => value)
       #'(cons name (lambda (x) (memq x value))))
      ((name . value)
       #'(cons name value))))

  (define (add-coder section state name value)
    (change-section state section eq? name value))

  ;; DSL callbacks

  (define (dsl/combine-into kw state name exprs)
    state)

  (define (dsl/datasheet-code kw state value)
    (change-meta state 'datasheet-code value))

  (define (dsl/datasheet-uri kw state value)
    (change-meta state 'datasheet-uri value))

  (define (dsl/decode-using kw state type name exprs)
    state)

  (define (dsl/decoder kw state name exprs)
    (let loop ((rest exprs) (acc '()))
      (syntax-case rest ()
        (() (add-coder 'decoders state (syntax->datum name) (reverse acc)))
        ((#:type value e* ...)
         (loop #'(e* ...) (alist-change-or-add eq? acc
                                               'type
                                               #'value)))
        ((#:callback value e* ...)
         (loop #'(e* ...) (alist-change-or-add eq? acc
                                               'callback
                                               #'value))))))

  (define (dsl/default-post-processor kw state value)
    (change-meta state 'default-post-processor value))

  (define (dsl/default-value kw state value)
    (change-meta state 'default-value value))

  (define (dsl/dependencies-for kw state type name exprs)
    state)

  (define (dsl/encoder kw state name exprs)
    (let loop ((rest exprs) (acc '()))
      (syntax-case rest ()
        (() (add-coder 'encoders state (syntax->datum name) (reverse acc)))
        ((#:type value e* ...)
         (loop #'(e* ...) (alist-change-or-add eq? acc
                                               'type
                                               #'value)))
        ((#:callback value e* ...)
         (loop #'(e* ...) (alist-change-or-add eq? acc
                                               'callback
                                               #'value)))
        ((#:signature (v0 v* ...) e* ...)
         (loop #'(e* ...) (alist-change-or-add eq? acc
                                               'signature
                                               #'(v0 v* ...))))
        ((#:constraints value e* ...)
         (loop #'(e* ...) (alist-change-or-add
                           eq? acc 'constraints
                           (encoder-constraints #'value)))))))

  (define (dsl/encode-using kw state type name exprs)
    state)

  (define (dsl/manufacturer kw state value)
    (change-meta state 'manufacturer value))

  (define (dsl/name kw state value)
    (change-meta state 'name value))

  (define (dsl/register kw state idx exprs)
    "Define a register and its contents.

Syntax:

  (register <idx>
    #:keyword <arg> ...
    ...)

Top-level keywords:

  #:default       The default register value.
  #:contents      Content definition of the register.
  #:post-process  A function that post-processes a register value.

Contents:

  (<name> #:keyword <arg> ...
          ...)

Contents keywords:

  #:offset  The Offset of the entry within the register.
  #:width   The width in bits within the register."
    (let loop ((rest exprs)
               (default #f)
               (pp #f)
               (contents '()))
      (if (null? rest)
          (add-register state (syntax->datum idx)
                        (list (cons 'default default)
                              (cons 'post-process pp)
                              (cons 'contents contents)))
          (syntax-case rest ()
            ((#:default value e* ...)
             (loop #'(e* ...) #'value pp contents))
            ((#:post-process value e* ...)
             (loop #'(e* ...) default #'value contents))
            ((#:contents e* ...)
             (let-values (((rest new-contents)
                           (register/contents #'(e* ...))))
               (loop rest default pp (append new-contents contents))))))))

  (define (dsl/register-width kw state value)
    (change-meta state 'register-width value))

  (define (dsl/with-modules-from kw state prefix exprs)
    state)

  ;; Main entry points

  (define (analyse-form keyword state expression)
    (syntax-case expression (combine-into
                             datasheet-code
                             datasheet-uri
                             decode-using
                             decoder
                             default-post-processor
                             default-value
                             dependencies-for
                             encoder
                             encode-using
                             manufacturer
                             name
                             register
                             register-width
                             with-modules-from)
      ((with-modules-from (prefix ...) e0 e* ...)
       (dsl/with-modules-from keyword
                              state
                              #'(prefix ...)
                              #'(e0 e* ...)))
      ((register idx e0 e* ...)
       (dsl/register keyword
                     state
                     #'idx
                     #'(e0 e* ...)))
      ((datasheet-code value)
       (dsl/datasheet-code keyword state #'value))
      ((datasheet-uri value)
       (dsl/datasheet-uri keyword state #'value))
      ((default-value value)
       (dsl/default-value keyword state #'value))
      ((default-post-processor value)
       (dsl/default-post-processor keyword state #'value))
      ((manufacturer value)
       (dsl/manufacturer keyword state #'value))
      ((name value)
       (dsl/name keyword state #'value))
      ((register-width value)
       (dsl/register-width keyword state #'value))
      ((decode-using (type name) e0 e* ...)
       (dsl/decode-using keyword
                         state
                         #'type
                         #'name
                         #'(e0 e* ...)))
      ((decoder (dname) e0 e* ...)
       (dsl/decoder keyword state #'dname #'(e0 e* ...)))
      ((encoder (ename) e0 e* ...)
       (dsl/encoder keyword state #'ename #'(e0 e* ...)))
      ((encode-using (type name) e0 e* ...)
       (dsl/encode-using keyword
                         state
                         #'type
                         #'name
                         #'(e0 e* ...)))
      ((dependencies-for (type name) e0 e* ...)
       (dsl/dependencies-for keyword
                             state
                             #'type
                             #'name
                             #'(e0 e* ...)))
      ((combine-into name e0 e* ...)
       (dsl/combine-into keyword
                         state
                         #'name
                         #'(e0 e* ...)))))

  (define (analyse-register-map keyword syntax-expressions)
    "This function processes all syntax expressions from a register map,
store and return all information from those expressions in a concise way, so
that compiler backends have easy access to that information to ease their
jobs."
    (let loop ((rest syntax-expressions)
               (state (initial-dsl-state)))
      (if (null? rest)
          (clean-up state)
          (loop (cdr rest) (analyse-form keyword state (car rest)))))))
