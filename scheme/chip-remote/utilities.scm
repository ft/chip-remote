(define-module (chip-remote utilities)
  #:export (cat
            flatten
            log2))

(define-syntax-rule (cat str ...)
  (string-concatenate (list str ...)))

(define (flatten lst)
  "Flatten deep tree to a shallow list"
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append (flatten (car lst))
                      (flatten (cdr lst))))))

(define log2-scale (log10 2))

(define (log2 n)
  (/ (log10 n) log2-scale))
