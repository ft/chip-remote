;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; scratchpad VALIDATORS: (Datatype)
;;
;; Check if a value is valid for an item.
;;
;;  - unbounded, with respect to semantics (ie. has to be a value from a table,
;;    in table lookup semantics)
;;  - range
;;  - list
;;  - interpreter
;;  - scheme

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
  #:use-module (srfi srfi-9)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote process-plist)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote validate)
  #:export (generate-item
            make-item
            item?
            item-name
            item-offset
            item-width
            item-meta
            item-get
            item-set
            item-default
            item-semantics
            item-validator
            item-decode
            item-encode))

(define-record-type <item>
  (make-item* name offset width semantics validator meta get set)
  item?
  (name item-name)
  (offset item-offset)
  (width item-width)
  (semantics item-semantics)
  (validator item-validator)
  (meta item-meta)
  (get item-get)
  (set item-set))

(define* (make-item name offset width meta get set
                    #:key
                    (validator #f)
                    (semantics '()))
  (make-item* name offset width
              (deduce-semantics width meta semantics)
              validator meta get set))

(define semantics-group
  (group 'semantics
         #:type 'list
         #:context 'scalar
         #:predicate (lambda (x) (eq? x #:semantics))
         #:transformer (lambda (expr)
                         (syntax-case expr ()
                           ((#:semantics e* ...) #'(e* ...))
                           ((e* ...) #'(e* ...))))))

(define validator-group
  (group 'validator
         #:type 'list
         #:predicate
         (lambda (x) (memq x '(#:validate #:validate*)))
         #:transformer
         (lambda (e)
           (syntax-case e ()
             ((#:validate exps ...) #'(generate-validator exps ...))
             ((#:validate* expr) #'expr)))))

(define-syntax generate-item*
  (lambda (x)
    (syntax-case x ()
      ((kw name offset width (semantics ...) () ((key value) ...))
       #'(kw name offset width (semantics ...) (#f) ((key value) ...)))
      ((kw name offset width (semantics ...) (validator _ ...) ((key value) ...))
       #'(let ((v:name name)
               (v:width width)
               (v:offset offset)
               (v:semantics (list semantics ...))
               (v:validator validator)
               (v:meta (list (cons key value) ...)))
           (make-item v:name v:offset v:width v:meta
                      (lambda (r) (bit-extract-width r v:offset v:width))
                      (lambda (r v) (set-bits r v v:width v:offset))
                      #:validator v:validator
                      #:semantics v:semantics))))))

(define-syntax generate-item
  (lambda (x)
    (syntax-case x ()
      ((kw name offset width m ...)
       (and (not-kw? #'name)
            (not-kw? #'offset)
            (not-kw? #'width))
       (with-syntax ((((semantics ...) (validator ...) (meta ...))
                      (process-plist #'(m ...)
                                     semantics-group
                                     validator-group
                                     (group 'meta))))
         #'(generate-item* 'name offset width
                           (semantics ...) (validator ...)
                           (meta ...))))
      ((kw name exp0 expn ...)
       (and (not-kw? #'name)
            (is-kw? #'exp0))
       (with-syntax (((width offset (semantics ...) (validator ...) (meta ...))
                      (process-plist #'(exp0 expn ...)
                                     (scalar-group 'width)
                                     (scalar-group 'offset)
                                     semantics-group
                                     validator-group
                                     (group 'meta))))
         #'(generate-item* 'name offset width
                           (semantics ...) (validator ...) (meta ...))))
      ((kw exp0 expn ...)
       (is-kw? #'exp0)
       (with-syntax (((width offset name (semantics ...) (validator ...) (meta ...))
                      (process-plist #'(exp0 expn ...)
                                     (scalar-group 'width)
                                     (scalar-group 'offset)
                                     (scalar-group 'name)
                                     semantics-group
                                     validator-group
                                     (group 'meta))))
         #'(generate-item* name offset width
                           (semantics ...) (validator ...) (meta ...)))))))

(define (item-default item)
  (let ((default (assq #:default (item-meta item))))
    (if default (cdr default) 0)))

(define (item-codec what item value)
  (let* ((semantics (item-semantics item))
         (worker (what semantics))
         (arity (car (procedure-minimum-arity worker))))
    (cond ((= arity 1) (worker value))
          ((= arity 2) (worker (item-width item) value))
          (else (throw 'invalid-arity
                       what item value
                       semantics worker arity)))))

(define (item-decode item value)
  (item-codec semantics-decode item value))

(define (item-encode item value)
  (item-codec semantics-encode item value))
