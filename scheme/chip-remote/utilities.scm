(define-module (chip-remote utilities)
  #:export (!!
            2e
            cat
            flatten
            map/last
            kwa-ref
            log2
            fmt
            symbol-upcase))

(define-syntax-rule (cat str ...)
  (string-concatenate (list str ...)))

(define (flatten lst)
  "Flatten deep tree to a shallow list"
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append (flatten (car lst))
                      (flatten (cdr lst))))))

(define (2e n)
  (ash 1 n))

(define log2-scale (log10 2))

(define (log2 n)
  (/ (log10 n) log2-scale))

(define (kwa-ref kw-lst kw default)
  (let ((m (memq kw kw-lst)))
    (if (not m)
        default
        (cadr m))))

(define (!! x)
  (not (not x)))

(define (fmt . rest)
  (apply format (cons #f rest)))

(define (map/last fnc lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (cons (fnc #t (car lst)) '()))
        (else (cons (fnc #f (car lst)) (map/last fnc (cdr lst))))))

(define (symbol-upcase sym)
  (string->symbol (string-upcase (symbol->string sym))))
