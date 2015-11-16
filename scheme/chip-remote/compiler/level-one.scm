;; Copyright (c) 2015 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (chip-remote compiler level-one)
  #:use-module (chip-remote compiler)
  #:use-module (srfi srfi-1)
  #:export (generate-level-one))

(define (generate-entry-functions kw data cb)
  (let loop ((rest (concatenate
                    (map (lambda (x) (assq-ref (cdr x) 'contents))
                         (assq-ref data 'registers))))
             (acc '()))
    (if (null? rest)
        (reverse acc)
        (let* ((this (car rest))
               (name (assq-ref this 'name))
               (offset (assq-ref this 'offset))
               (width (assq-ref this 'width)))
          (loop (cdr rest) (cons (cb kw name offset width) acc))))))

(define (fnc-name kw prefix name)
  (datum->syntax kw (symbol-append prefix '- (syntax->datum name) '-bits)))

(define (generate-setters kw data)
  (generate-entry-functions
   kw data
   (lambda (kw name offset width)
     #`(define-public (#,(fnc-name kw 'set name) register value)
         (let ((w #,width) (o #,offset))
           ((@ (chip-remote bitops) set-bits)
            register
            (logand value (- (ash 1 w) 1)) w o))))))

(define (generate-getters kw data)
  (generate-entry-functions
   kw data
   (lambda (kw name offset width)
     #`(define-public (#,(fnc-name kw 'get name) register)
         ((@ (chip-remote bitops) bit-extract-width)
          register #,offset #,width)))))

(define (content->name content)
  (syntax->datum (assq-ref content 'name)))

(define (addr-name kw name)
  (datum->syntax kw (symbol-append 'regaddr: (syntax->datum name))))

(define (regaddr-definitions kw data)
  (let ((addr (datum->syntax kw (car data)))
        (names (cdr data)))
    (map (lambda (x)
           #`(define-public #,(addr-name kw x) #,addr))
         names)))

(define (generate-regaddr kw data)
  (let loop ((rest (map (lambda (x)
                          (cons (car x)
                                (map content->name
                                     (assq-ref (cdr x) 'contents))))
                        (assq-ref data 'registers)))
             (acc '()))
    (if (null? rest)
        (concatenate (reverse acc))
        (loop (cdr rest)
              (cons (regaddr-definitions kw (car rest)) acc)))))

(define (rewrite-entry entry)
  (list 'cons
        (list 'quote (car entry))
        (if (eq? (car entry) 'name)
            (list 'quote (syntax->datum (cdr entry)))
            (cdr entry))))

(define (rewrite-content content)
  (cons 'list (map rewrite-entry content)))

(define (rewrite-register-data data)
  (if (eq? 'contents (car data))
      (list 'cons ''contents
            (cons 'list (map rewrite-content (cdr data))))
      (list 'cons (list 'quote (car data)) (cdr data))))

(define (rewrite-register reg)
  (list 'cons (car reg) (cons 'list (map rewrite-register-data (cdr reg)))))

(define (rewrite-map kw data)
  (let ((regs (assq-ref data 'registers)))
    (datum->syntax kw (cons 'list (map rewrite-register regs)))))

(define (generate-register-map kw data)
  ;; TODO: Once decoders and encoders are implemented within the DSL, they
  ;; should probably become part of the register map as well. Also, for
  ;; completeness' sake, we should add getters and setters to the map.
  #`(define-public register-map #,(rewrite-map kw data)))

(define-syntax generate-level-one
  (lambda (x)
    (syntax-case x ()
      ((kw file)
       (let* ((fname (determine-file-name (syntax->datum #'file)))
              (data (analyse-register-map #'kw (read-file #'kw fname))))
         (with-syntax (((setters ...) (generate-setters #'kw data))
                       ((getters ...) (generate-getters #'kw data))
                       ((regaddr ...) (generate-regaddr #'kw data))
                       (regmap (generate-register-map #'kw data)))
           #'(begin setters ...
                    getters ...
                    regaddr ...
                    regmap)))))))
