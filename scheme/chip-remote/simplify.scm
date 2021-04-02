(define-module (chip-remote simplify)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  #:use-module (chip-remote item)
  #:use-module (chip-remote register)
  #:use-module (chip-remote semantics)
  #:export (pp:simplify
            pp:simplified))

(define (pp:simplify spec)
  (cond ((null? spec) spec)
        ((list? spec) (map pp:simplify spec))
        ((pair? spec) (cons (pp:simplify (car (spec)))
                            (pp:simplify (cdr (spec)))))
        ((semantics? spec)
         (format #f "#<semantics ~a ~a>"
                 (semantics-type spec)
                 (semantics-name spec)))
        ((item? spec)
         (format #f "#<item ~a ~a ~a>"
                 (item-name spec)
                 (item-offset spec)
                 (item-width spec)))
        ((register? spec)
         (format #f "#<register ~a>" (length (register-items spec))))
        (else spec)))

(define* (pp:simplified spec #:key (per-line-prefix ""))
  (pretty-print (pp:simplify spec)
                #:display? #t
                #:per-line-prefix per-line-prefix
                #:width 128
                #:max-expr-width 96))
