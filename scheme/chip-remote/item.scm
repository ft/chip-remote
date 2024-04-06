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

(define-module (chip-remote item)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote item access)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:use-module (data-structures records)
  #:use-module (data-structures records utilities)
  #:export (‣                       ;; Item creation
            item
            make-item
            item?                   ;; Item type API
            item-name
            item-offset
            item-width
            item-semantics
            item-access
            item-default
            item-default-raw        ;; Item utilities
            item-decode
            item-encode
            item-get
            item-set
            item->list
            item-named?
            item-range
            validate-item-value))

(define-record-type* <item>
  item make-item item? this-item
  (name      item-name (sanitize (need 'name symbol?)))
  (offset    item-offset    (default 0)
             (sanitize (need 'offset non-negative-integer?)))
  (width     item-width     (default 1)
             (sanitize (need 'width (lambda (x) (>= x 1)))))
  (semantics item-semantics (thunked)
             (default (if (> (item-width this-item) 1)
                          unsigned-integer
                          boolean))
             (sanitize (need 'semantics semantics?)))
  (default   item-default (thunked) (default (semantics-default
                                              (item-semantics this-item)
                                              (item-width this-item))))
  ;; TODO: This needs to become record* type too.
  (access    item-access    (default 'access)))

;; Unicode triangular bullet point as the short-hand for (item ...).
(define-syntax ‣
  (syntax-rules ()
    ((_ n o w e* ...) (item (name 'n) (offset o) (width w) e* ...))))

(set-record-type-printer! <item>
  (lambda (it port)
    (simple-format port "#<item name: ~a offset: ~a width: ~a semantics: ~a default: ~a>"
                   (item-name it)
                   (item-offset it)
                   (item-width it)
                   (semantics-name (item-semantics it))
                   (item-default it))))

(define (item-default-raw item)
  (item-encode item (item-default item)))

(define (item-decode item value)
  (semantics-decode (item-semantics item) (item-width item) value))

(define (item-encode item value)
  (semantics-encode (item-semantics item) (item-width item) value))

(define (item-get item register-value)
  (bit-extract-width register-value (item-offset item) (item-width item)))

(define (item-set item register-value item-value)
  (set-bits register-value item-value (item-width item) (item-offset item)))

(define (item-range item)
  (semantics-range (item-semantics item)
                   (item-width item)))

(define (item->list item)
  (list (item-name item)
        (item-offset item)
        (item-width item)))

(define (item-named? item name)
  (eq? (item-name item) name))

(define (validate-item-value item value)
  (semantics-in-range? (item-semantics item)
                       (item-width item)
                       value))
