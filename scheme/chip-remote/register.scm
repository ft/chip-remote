;; Copyright (c) 2017-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote register)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (chip-remote item)
  #:use-module (chip-remote utilities)
  #:use-module (data-structures records)
  #:use-module (data-structures records utilities)
  #:export (†                              ;; Register creation
            register
            make-register
            define-register
            register?                      ;; Register type API
            register-name
            register-items
            register-description
            register-width*
            register-items:sorted          ;; Register utilities
            register-default
            register-width
            register-item-names
            register-named-items-count
            register-contains?
            register-canonical
            register-canonical-index
            register-address
            register-ref
            register-fold
            register-diff
            rename-register
            prefix-register
            derive-register-from
            replace-register-items
            register->alist))

(define-record-type* <register>
  register make-register register? this-register
  (name         register-name (default #f))
  (description  register-description (default #f))
  (width        register-width* (default #f))
  (items        register-items))

;; Short hand for defining registers on the fly. This does not support
;; "inherit"!
(define-syntax †
  (lambda (s)
    (syntax-case s (name)
      ((_ (name n) e* ...) #'(register (name n) (items (list e* ...))))
      ((_ e* ...)          #'(register (items (list e* ...)))))))

(new-record-definer define-register register)

(define (register-default reg)
  (fold (lambda (x acc) (item-set x acc (item-default-raw x)))
        0
        (register-items reg)))

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

(define* (register-find-item reg name idx #:key (default (const #f)))
  (list-iterate (lambda (x a n k)
                  (if (eq? name x)
                      (if (= a idx)
                          (k n)
                          (1+ a))
                      a))
                0 default
                (map item-name (register-items:sorted reg))))

(define (register-canonical register . item-address)
  (let ((v (apply register-canonical-index (cons register item-address))))
    (and v (list v))))

(define (register-canonical-index register . item-address)
  (match item-address
    (((? number? index))
     (if (or (negative? index)
             (>= index (length (register-items register))))
         #f
         index))
    (((? symbol? name))
     (register-find-item register name 0))
    (((? symbol? name) (? number? index))
     (register-find-item register name index))
    (_ (throw 'invalid-item-address item-address))))

(define (register-address register . item-address)
  (let ((ca (apply register-canonical (cons register item-address))))
    (and ca (apply register-ref (cons register ca)))))

(define (register-item-names reg)
  (map item-name (register-items reg)))

(define (register-named-items-count reg name)
  (register-fold (lambda (item a)
                   (if (item-named? item name)
                       (1+ a)
                       a))
                 0 reg))

(define (register-contains? reg item)
  (!! (memq item (register-item-names reg))))

(define (register-ref reg n)
  (list-ref (register-items:sorted reg) n))

(define (register-fold fnc init reg)
  (fold fnc init (register-items:sorted reg)))

(define (register-items:sorted reg)
  (sort (register-items reg)
        (lambda (a b)
          (< (item-offset a)
             (item-offset b)))))

(define (assq-or-zero lst key)
  (or (assq-ref lst key) 0))

(define (just-seen db name)
  (cons (cons name (+ 1 (assq-or-zero db name)))
        (filter (lambda (x) (not (eq? name (car x)))) db)))

(define (filter/items fnc items)
  (let loop ((rest items) (idx 0) (seen '()))
    (if (null? rest) '()
        (let* ((this (car rest))
               (this-name (item-name this))
               (rest (cdr rest)))
          (if (fnc idx (assq-or-zero seen this-name) this)
              (cons this (loop rest (+ 1 idx) (just-seen seen this-name)))
              (loop rest (+ 1 idx) (just-seen seen this-name)))))))

(define (spec->fnc spec)
  (match spec
    ((? procedure? proc) proc)
    ((? number? n) (lambda (idx cnt item) (= idx n)))
    ((? symbol? x) (lambda (idx cnt item) (eq? x (item-name item))))
    (((? symbol? x) (? number? n)) (lambda (idx cnt item)
                                     (and (= cnt n)
                                          (eq? (item-name item) x))))
    (('offset (? procedure? compare) (? number? n))
     (lambda (idx cnt item)
       (compare (item-offset item) n)))
    (('width (? procedure? compare) (? number? n))
     (lambda (idx cnt item)
       (compare (item-width item) n)))
    (else (throw 'unknown-register-item-spec spec))))

(define (specs->fnc lst)
  (lambda (idx cnt item)
    (let loop ((rest lst))
      (if (null? rest)
          #f
          (or ((spec->fnc (car rest)) idx cnt item)
              (loop (cdr rest)))))))

(define (remove-items specs items)
  (filter/items (lambda (idx cnt item)
                  (not ((specs->fnc specs) idx cnt item))) items))

(define (insert-item item lst)
  (let loop ((rest lst))
    (if (null? rest)
        (cons item '())
        (let ((this (car lst)) (rest (cdr rest)))
          (if (<= (item-offset item) (item-offset this))
              (cons item (cons this rest))
              (cons this (insert-item item rest)))))))

(define (insert-items new target)
  (if (null? new)
      target
      (insert-items (cdr new) (insert-item (car new) target))))

(define* (derive-register-from reg #:key (remove '()) (insert '()))
  (define (change fnc x lst) (fnc (if (list? x) x (list x)) lst))
  (define (ins x lst) (change insert-items x lst))
  (define (rem x lst) (change remove-items x lst))
  (register (inherit reg) (items (ins insert
                                      (rem remove
                                           (register-items reg))))))

(define (register->alist reg)
  (map item->list (register-items:sorted reg)))

(define (rename-register reg fnc)
  (register (inherit reg) (items (map fnc (register-items reg)))))

(define (prefix-register reg prefix)
  (rename-register reg
                   (lambda (entry)
                     (item (inherit entry)
                           (name (symbol-append prefix
                                                (item-name entry)))))))

(define (register-diff r a b)
  (fold (lambda (x acc)
          (let ((va (item-get x a))
                (vb (item-get x b)))
            (if (= va vb)
                acc
                (cons (list x va vb) acc))))
        '()
        (register-items:sorted r)))
