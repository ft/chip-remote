(define-module (chip-remote page-map)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 control)
  #:use-module (ice-9 pretty-print)
  #:use-module (chip-remote process-plist)
  #:use-module (chip-remote item)
  #:use-module (chip-remote pretty-print)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote utilities)
  #:export (generate-page-map
            make-page-map
            page-map?
            page-map-table
            page-map-register-maps
            page-map-default
            page-map-item-names
            page-map-merge
            page-map-fold
            page-map-ref
            page-map-ref->address
            page-map-address
            page-address->index
            define-page-map))

;; How to share some registers between the register maps of all or even only
;; some pages of a device? This needs to be encoded in (chip-remote device).
;; But maybe it also requires another type. We'll see. Maybe only mirror items
;; between possibly different registers. That would be the more general
;; solution.

(define-record-type <page-map>
  (make-page-map table)
  page-map?
  (table page-map-table))

(define (pp-page-map port indent tab)
  (let ((pp (make-printer/assoc port indent)))
    (pp-record port 'page-map
               (lambda ()
                 (pp 'table (page-map-table tab))))))

(set-record-type-printer! <page-map>
  (lambda (rec port)
    (pp-page-map port (pp-indent) rec)))

(define (page-map-register-maps pm)
  (map cdr (page-map-table pm)))

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
  (map (lambda (x) (register-map-default (cdr x)))
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

(define-syntax-rule (pm-iter init return pm fnc)
  (call/ec (lambda (return) (page-map-fold fnc init pm))))

(define (page-map-ref->address pm name)
  (pm-iter #f return pm
           (lambda (pa rm pacc)
             (let ((ra (register-map-ref->address rm name)))
               (if ra
                   (return (cons pa ra))
                   #f)))))

(define (page-map-ref pm name)
  (pm-iter #f return pm
           (lambda (pa rm pacc)
             (let ((item (register-map-ref rm name)))
               (if (item? item)
                   (return item)
                   #f)))))

(define page-map-address
  (case-lambda
    ((pm page-addr)
     (pm-iter #f return pm
              (lambda (pa rm pacc)
                (if (eqv? pa page-addr)
                    (return rm)
                    #f))))
    ((pm page-addr reg-addr)
     (register-map-address (page-map-address pm page-addr) reg-addr))
    ((pm page-addr reg-addr item-addr)
     (register-map-address (page-map-address pm page-addr)
                           reg-addr item-addr))
    ((pm page-addr reg-addr name cnt)
     (register-map-address (page-map-address pm page-addr)
                           reg-addr name cnt))))

(define (page-address->index pm addr)
  (pm-iter 0 return pm
           (lambda (pa rm idx)
             (if (eqv? pa addr)
                 (return idx)
                 (+ idx 1)))))
