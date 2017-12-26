(define-module (chip-remote page-map)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 pretty-print)
  #:use-module (chip-remote language)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote utilities)
  #:export (generate-page-map
            make-page-map
            page-map?
            page-map-table
            page-map-defaults
            page-map-item-names
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

(define (page-map-defaults rm)
  (map register-map-defaults (page-map-table rm)))

(define (page-map-item-names rm)
  (flatten (map register-map-names (page-map-table rm))))

(define-syntax-rule (define-page-map binding e0 e* ...)
  (define binding (generate-page-map e0 e* ...)))

(define (record-page-map-printer page-map port)
  (format port "<page-map:~%    #:table~%")
  (pretty-print (page-map-table page-map) port #:per-line-prefix "    ")
  (format port ">"))

(set-record-type-printer! <page-map> record-page-map-printer)
