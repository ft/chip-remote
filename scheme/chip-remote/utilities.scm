(define-module (chip-remote utilities)
  #:use-module (ice-9 control)
  #:export (!!
            2e
            addr<
            cat
            flatten
            map/last
            kwa-ref
            log2
            fmt
            number->symbol
            symbol-upcase
            either
            all
            list-of-integers?
            list-of-list-of-integers?))

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

(define (number->symbol n)
  (string->symbol (number->string n)))

(define (either pred lst)
  (call/ec (lambda (return)
             (let loop ((rest lst))
               (if (null? (cdr rest))
                   (pred (car rest))
                   (if (pred (car rest))
                       (return #t)
                       (loop (cdr rest))))))))

(define (all pred lst)
  (call/ec (lambda (return)
             (let loop ((rest lst))
               (if (null? (cdr rest))
                   (pred (car rest))
                   (if (not (pred (car rest)))
                       (return #f)
                       (loop (cdr rest))))))))

(define (list-of-list-of-integers? v)
  (and (list? v)
       (all list-of-integers? v)))

(define (list-of-integers? v)
  (and (list? v)
       (all integer? v)))

(define (addr< a b)
  (if (or (null? a)
          (null? b))
      #f
      (cond ((not (car a))
             (and (not (car b))
                  (addr< (cdr a) (cdr b))))
            ((not (car b)) #f)
            ((< (car a) (car b)) #t)
            ((= (car a) (car b)) (addr< (cdr a) (cdr b)))
            (else #f))))
