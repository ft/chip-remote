;; Copyright (c) 2017-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote register)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote item)
  #:use-module (chip-remote process-plist)
  #:use-module (chip-remote utilities)
  #:export (generate-register              ;; Register creation
            make-register
            define-register
            register?                      ;; Register type API
            register-meta
            register-items
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

(define-immutable-record-type <register>
  (make-register meta items)
  register?
  (meta register-meta)
  (items register-items replace-register-items))

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
                (item-set x acc (item-default x)))
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
  (replace-register-items reg (ins insert (rem remove (register-items reg)))))

(define (register->alist reg)
  (map item->list (register-items reg)))

(define (prefix-register reg prefix)
  (rename-register reg
                   (lambda (item)
                     (new-item-name item
                                    (symbol-append prefix
                                                   (item-name item))))))

(define (rename-register reg fnc)
  (replace-register-items reg (map fnc (register-items reg))))

(define (register-diff r a b)
  (fold (lambda (x acc)
          (let ((va (item-get x a))
                (vb (item-get x b)))
            (if (= va vb)
                acc
                (cons (list x va vb) acc))))
        '()
        (register-items:sorted r)))
