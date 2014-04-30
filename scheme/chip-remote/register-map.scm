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
  #:export (define-register-map))

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
                    (syntax->datum #'(register ...)))))))))
