;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; Description:
;;
;; Register-maps are one description of configuration tables. They consist of
;; an address, and an alist, that describes the contents of the register in
;; question. Possible keys:
;;
;;     default-value: The default value of the register (if omitted, the
;;                    default-value defaults to zero)
;;
;;     contents: A list of items being configured by the register
;;
;; The content entry is a list of lists, each of which looks like this:
;;
;;     (symbol offset width)
;;
;; Where OFFSET is the offset of the entry within the register and WIDTH being
;; the number of bits the entry is comprised of. SYMBOL is the name for the
;; described entry, that has to be unique within the register-map.
;;
;; Example:
;;
;;     (define-register-map foo-bar
;;       (#x0 (default-value #x54)
;;            (contents (m-divider 0 4)
;;                      (n-divider 4 4))
;;       (#x1 (default-value #xd0)
;;            (contents (power-sabe 0 1)
;;                      (reference-divider 4 4))))
;;
;; That defines (using `define-public') the following functions:
;;
;;   - set-m-divider-bits
;;   - get-m-divider-bits
;;   - set-n-divider-bits
;;   - get-n-divider-bits
;;   - set-reference-divider-bits
;;   - get-reference-divider-bits
;;   - set-power-save-bits
;;   - get-power-save-bits
;;
;; as well as a set of variables:
;;
;;   - foo-bar-register-map
;;
;; that contains the entire register-map for later reference. And a few
;; back-references to register addresses:
;;
;;   - register-address-m-divider
;;   - register-address-n-divider
;;   - register-address-reference-divider
;;   - register-address-power-save

(define-module (chip-remote register-map)
  #:use-module (srfi srfi-1)
  #:export (define-register-map
            register-default
            =>
            map-across))

(define (get-variable name)
  (let ((var (module-variable (current-module) name)))
    (or var (throw 'cr-unknown-variable name))
    var))

(define (get-value name)
  (variable-ref (get-variable name)))

(define (assemble-decoder name)
  (let* ((var (get-variable name))
         (val (variable-ref var)))
    (list (list var (cond ((procedure? val) 'function)
                          ((list? val) 'list)
                          (else (throw 'cr-unsupported-decoder-at name)))
                name))))

(define (expand-rest name rest)
  (cond ((null? rest)
         (assemble-decoder 'literal-binary))
        ((or (not (list? rest))
             (not (= (length rest) 2))
             (not (eq? (car rest) '=>)))
         (throw 'cr-dubious-register-map-entry name))
        (else (assemble-decoder (cadr rest)))))

(define (expand-entry item)
  (let* ((name (car item))
         (offset (cadr item))
         (width (caddr item))
         (rest (cdddr item))
         (getter (get-value (symbol-append 'get- name '-bits)))
         (setter (get-value (symbol-append 'set- name '-bits))))
    (cons name
          (cons offset
                (cons width
                      (cons getter
                            (cons setter (expand-rest name rest))))))))

(define (expand-registers lst)
  (map (lambda (reg)
         (cons (car reg)
               (map (lambda (a)
                      (cond ((not (eq? (car a) 'contents)) a)
                            (else (cons 'contents
                                        (map (lambda (i)
                                               (expand-entry i))
                                             (cdr a))))))
                    (cdr reg))))
       lst))

(define (generate-setter c kw)
  (datum->syntax
   kw (let* ((name (car c))
             (offset (cadr c))
             (width (caddr c))
             (max (- (ash 1 width) 1)))
        `(define-public (,(symbol-append 'set- name '-bits) register value)
           (set-bits register (logand value ,max) ,width ,offset)))))

(define (generate-getter c kw)
  (datum->syntax
   kw (let ((name (car c))
            (offset (cadr c))
            (width (caddr c)))
        `(define-public (,(symbol-append 'get- name '-bits) register)
           (bit-extract-width register ,offset ,width)))))

(define (generate-var name address kw)
  (datum->syntax
   kw `(define-public ,(symbol-append 'register-address- name) ,address)))

(define (generate-procedures kw register-map)
  (let next-register ((register register-map))
    (if (null? register)
        '()
        (let* ((this (car register))
               (address (car this))
               (al (cdr this))
               (c (assq 'contents al)))
          (let next-content ((content (cdr c)))
            (if (null? content)
                (next-register (cdr register))
                (cons (generate-setter (car content) kw)
                      (cons (generate-getter (car content) kw)
                            (cons (generate-var (caar content) address kw)
                                  (next-content (cdr content)))))))))))

(define-syntax define-register-map
  (lambda (x)
    (syntax-case x ()
      ((kw name register ...)
       (with-syntax ((variable-name
                      (datum->syntax #'kw
                                     (symbol-append (syntax->datum #'name)
                                                    '-register-map)))
                     ((exp ...)
                      (generate-procedures #'kw
                                           (syntax->datum #'(register ...)))))
         #'(begin exp ...
                  (define-public variable-name
                    (expand-registers (syntax->datum #`(register ...))))))))))

(define (register-default regmap address)
  (let ((reg (assoc address regmap)))
    (if (not reg)
        (throw 'cr-no-such-register address)
        (let ((value (assq 'default-value (cdr reg))))
          (if value
              (cdr value)
              0)))))

(define-syntax =>
  (lambda (x)
    (syntax-case x ()
      ((_ n) #'(cons (quote n) (module-variable (current-module) (quote n)))))))

(define (map-across fnc what regmap)
  (fold (lambda (x acc)
          (let ((address (car x))
                (item (assq what (cdr x))))
            (if item (cons (fnc address (cdr item)) acc))))
        '()
        regmap))
