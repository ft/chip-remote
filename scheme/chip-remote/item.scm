;; Copyright (c) 2017-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; This module implements an abstraction for pieces of data that, when combined
;; make up a register (see (chip-remote register) about that combination). The
;; abstraction is bundled into the <item> data-type, which make-item creates.
;;
;; An item is a series of one or more bits, that is located at a specific
;; offset in a larger word of one or more items (which is a register). Offsets
;; start at zero, which addresses the rightmost bit in a word. This is also the
;; least significant bit in the word.
;;
;;   | n | ... | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
;;   | n | ... | X | X | X | X | d | d | d |
;;
;; In the above example, the "XXXX" item is located at offset 3 and has a width
;; of 4.
;;
;; The language for users of the module is implemented by the generate-item
;; macro, that explodes into a make-item call. generate-item has three basic
;; call structures:
;;
;;   (generate-item NAME OFFSET WIDTH REST ...)
;;   (generate-item NAME #:width WIDTH #:offset OFFSET REST ...)
;;   (generate-item #:width WIDTH #:name NAME #:offset OFFSET REST ...)
;;
;; "REST ..." are always a property list. In the first two cases, NAME is
;; automatically quoted. The third, fully keyworded variant, evaluates all its
;; arguments.

(define-module (chip-remote item)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote process-plist)
  #:use-module (chip-remote interpreter)
  #:use-module (chip-remote item access)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote validate)
  #:export (generate-item           ;; Item creation
            make-item
            derive-item-from
            item?                   ;; Item type API
            item-name
            new-item-name
            item-offset
            new-item-offset
            item-width
            item-semantics
            item-validator
            item-access
            new-item-access
            item-meta
            new-item-meta
            item-default            ;; Item utilities
            item-decode
            item-encode
            item-get
            item-set
            item->list
            item-named?
            move-item))

(define-immutable-record-type <item>
  (make-item name offset width semantics validator access meta)
  item?
  (name item-name new-item-name)
  (offset item-offset new-item-offset)
  (width item-width)
  (semantics item-semantics)
  (validator item-validator)
  (access item-access new-item-access)
  (meta item-meta new-item-meta))

(define (move-item item n)
  (set-fields item ((item-offset) n)))

(define (item-get item register-value)
  (bit-extract-width register-value (item-offset item) (item-width item)))

(define (item-set item register-value item-value)
  (set-bits register-value item-value (item-width item) (item-offset item)))

(define access-group
  (group 'access
         #:type 'single
         #:context 'scalar
         #:predicate (lambda (x) (memq x '(#:access #:access*)))
         #:transformer (lambda (e)
                         (syntax-case e ()
                           ((#:access access)
                            (syntax-case #'access (none forbidden
                                                   rw   read-write
                                                   ro   read-only
                                                   wo   write-only)
                              (none       #'(none))
                              (forbidden  #'(none))
                              (rw         #'(rw))
                              (read-write #'(rw))
                              (ro         #'(ro))
                              (read-only  #'(ro))
                              (wo         #'(wo))
                              (write-only  #'(wo))))
                           ((#:access* expr)  #'expr)))))

(define semantics-group
  (group 'semantics
         #:type 'list
         #:context 'scalar
         #:predicate (lambda (x) (memq x '(#:semantics #:semantics*)))
         #:transformer (lambda (e)
                         (syntax-case e ()
                           ((#:semantics e* ...) #'(generate-semantics e* ...))
                           ((#:semantics* expr) #'expr)))))

(define validator-group
  (group 'validator
         #:type 'list
         #:context 'scalar
         #:predicate
         (lambda (x)
           (memq x '(#:validate #:validate* #:and-validate #:or-validate)))
         #:transformer
         (lambda (e)
           (syntax-case e ()
             ((#:and-validate exps ...)
              #'(combine-validator (generate-validator exps ...)
                                   'and))
             ((#:or-validate exps ...)
              #'(combine-validator (generate-validator exps ...)
                                   'or))
             ((#:validate exps ...) #'(generate-validator exps ...))
             ((#:validate* expr) #'expr)))))

(define-syntax generate-item*
  (lambda (x)
    (define (with-default exp)
      (syntax-case exp ()
        (() #'#f)
        (_ exp)))
    (syntax-case x ()
      ((kw name offset width access sem valid? ((key value) ...))
       (with-syntax ((access (with-default #'access))
                     (semantics (with-default #'sem))
                     (validator (with-default #'valid?)))
         #'(let ((v:name name)
                 (v:width width)
                 (v:offset offset)
                 (v:meta (list (cons key value) ...))
                 (v:semantics semantics)
                 (v:validator validator)
                 (v:access (or access (rw))))
             (make-item v:name v:offset v:width
                        (deduce-semantics v:width v:semantics)
                        v:validator
                        v:access
                        v:meta)))))))

(define-syntax generate-item
  (lambda (x)
    (syntax-case x ()
      ((kw name offset width m ...)
       (and (not-kw? #'name)
            (not-kw? #'offset)
            (not-kw? #'width))
       (with-syntax (((access semantics validator (meta ...))
                      (process-plist #'(m ...)
                                     access-group
                                     semantics-group
                                     validator-group
                                     (group 'meta))))
         #'(generate-item* 'name offset width
                           access semantics validator
                           (meta ...))))
      ((kw name exp0 expn ...)
       (and (not-kw? #'name)
            (is-kw? #'exp0))
       (with-syntax (((width offset access semantics validator (meta ...))
                      (process-plist #'(exp0 expn ...)
                                     (scalar-group 'width)
                                     (scalar-group 'offset)
                                     access-group
                                     semantics-group
                                     validator-group
                                     (group 'meta))))
         #'(generate-item* 'name offset width
                           access semantics validator
                           (meta ...))))
      ((kw exp0 expn ...)
       (is-kw? #'exp0)
       (with-syntax (((width offset name access semantics validator (meta ...))
                      (process-plist #'(exp0 expn ...)
                                     (scalar-group 'width)
                                     (scalar-group 'offset)
                                     (scalar-group 'name)
                                     access-group
                                     semantics-group
                                     validator-group
                                     (group 'meta))))
         #'(generate-item* name offset width
                           access semantics validator
                           (meta ...)))))))

(define (item-default item)
  (let ((default (assq #:default (item-meta item)))
        (default-raw (assq #:default-raw (item-meta item))))
    (cond (default (item-encode item (cdr default)))
          (default-raw (cdr default-raw))
          (else 0))))

(define (item-decode item value)
  (s:decode (item-semantics item) (item-width item) value))

(define (item-encode item value)
  (s:encode (item-semantics item) (item-width item) value))

(define (item->list item)
  (list (item-name item)
        (item-offset item)
        (item-width item)))

(define* (derive-item-from item #:key offset access)
  (set-fields item
              ((item-offset) (or offset (item-offset item)))
              ((item-access) (or access (item-access item)))))

(define (item-named? item name)
  (eq? (item-name item) name))
