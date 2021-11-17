(define-module (chip-remote page-map)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (chip-remote process-plist)
  #:use-module (chip-remote item)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote utilities)
  #:export (generate-page-map          ;; PageMap creation
            make-page-map
            define-page-map
            page-map?                  ;; PageMap type API
            page-map-table
            page-map-table:sorted      ;; PageMap utilities
            page-map-register-maps
            page-map-default
            page-map-item-names
            page-map-merge
            page-map-fold
            page-map-ref
            page-map-address
            page-map-address:register
            page-map-find-item
            page-map-canonical
            ;; Missing API
            page-map-registers))

;; How to share some registers between the register maps of all or even only
;; some pages of a device? This needs to be encoded in (chip-remote device).
;; But maybe it also requires another type. We'll see. Maybe only mirror items
;; between possibly different registers. That would be the more general
;; solution.

(define-record-type <page-map>
  (make-page-map table)
  page-map?
  (table page-map-table))

(define (page-map-register-maps pm)
  (map cdr (page-map-table pm)))

(define (page-map-table:sorted rm)
  (sort (page-map-table rm)
        (lambda (a b) (index< (car a) (car b)))))

(define-syntax generate-page-map
  (lambda (x)
    ;; Like *this* you can't *mix* the two cases. This needs to be recursive in
    ;; order to enable this. And we want this, obviously.
    (syntax-case x ()
      ;; Here, if exp0 is a #:keyword, explode into (generate-register-map ...),
      ;; otherwise (expn ...) should be empty and exp0 should be inserted
      ;; verbatim, so the expression will be evaluated at run-time.
      ((kw (addr exp0 expn ...) ...)
       (is-kw? (car #'(exp0 ...)))
       #'(make-page-map (list (cons addr
                                    (generate-register-map exp0 expn ...))
                              ...)))
      ((kw (addr exp) ...)
       #'(make-page-map (list (cons addr exp) ...))))))

(define (page-map-default rm)
  (map (lambda (x) (cons (car x) (register-map-default (cdr x))))
       (page-map-table rm)))

(define (page-map-item-names rm)
  (flatten (map (lambda (x) (register-map-item-names (cdr x)))
                (page-map-table rm))))

(define-syntax-rule (define-page-map binding e0 e* ...)
  (define binding (generate-page-map e0 e* ...)))

(define (page-map-merge lst)
  (make-page-map (apply append (map page-map-table (flatten lst)))))

(define (page-map-fold fnc init pm)
  (fold (lambda (page acc)
          (fnc (car page) (cdr page) acc))
        init
        (page-map-table pm)))

(define (page-map-ref pm . args)
  (match args
    (((? index? address))
     (let ((entry (assv address (page-map-table pm))))
       (if entry
           (cdr entry)
           (throw 'unknown-page-map-address address))))
    (((? index? pa) (? index? ra))
     (register-map-ref (page-map-ref pm pa) ra))
    (((? index? pa) (? index? ra) (? non-negative-integer? idx))
     (register-map-ref (page-map-ref pm pa) ra idx))
    (_ (throw 'invalid-page-map-reference args))))

(define* (page-map-find-item pm name idx #:optional (default (const #f)))
  (list-iterate (lambda (x a n k)
                  (let* ((pa (car x))
                         (rm (cdr x))
                         (m (register-map-find-item rm name (- idx a))))
                    (if (list? m)
                        (k (cons pa m))
                        (or m a))))
                0 default
                (page-map-table:sorted pm)))

(define (page-map-canonical pm . item-address)
  (match item-address
    (((? symbol? name))
     (page-map-find-item pm name 0))
    (((? symbol? name) (? non-negative-integer? n))
     (page-map-find-item pm name n))
    (((? index? pa) . rest)
     (cons pa (apply register-map-canonical (cons (page-map-ref pm pa) rest))))
    (_ (throw 'invalid-item-address item-address))))

(define (page-map-address pm . item-address)
  (let ((ca (apply page-map-canonical (cons pm item-address))))
    (and ca (apply page-map-ref (cons pm ca)))))

(define (page-map-address:register pm . item-address)
  (let ((ca (apply page-map-canonical (cons pm item-address))))
    (and ca (apply page-map-ref (cons pm (drop-right ca 1))))))
