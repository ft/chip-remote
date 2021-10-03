(define-module (chip-remote utilities)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-9 gnu)
  #:export (!!
            2e
            addr=
            addr<
            addr>
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
            list-of-list-of-integers?
            structurally-equal?
            diff*
            diff
            make-diff
            string-ends-in-newline?
            string-strip-newlines
            timeout->select
            whitespace?
            has-data?
            drain-whitespace
            xread
            xselect))

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

(define (structurally-equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) #t)
        ((and (pair? a) (pair? b))
         (and (structurally-equal? (car a) (car b))
              (structurally-equal? (cdr a) (cdr b))))
        (else #f)))

(define-immutable-record-type <diff>
  (make-diff old new)
  diff?
  (old old-part)
  (new new-part))

(define (diff* a b)
  (unless (structurally-equal? a b)
    (throw 'diff:unsupported-arguments a b))
  (cond ((pair? a) (cons (diff* (car a) (car b))
                         (diff* (cdr a) (cdr b))))
        ((equal? a b) a)
        (else (make-diff a b))))

(define (diff a b)
  (values (not (equal? a b))
          (diff* a b)))

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

(define (addr> a b)
  (addr< b a))

(define (addr= a b)
  (and (not (addr< a b))
       (not (addr> a b))))

(define (string-ends-in-newline? s)
  (char=? #\newline (string-ref s (1- (string-length s)))))

(define (string-strip-newlines s)
  (cond ((string-null? s) s)
        ((string-ends-in-newline? s)
         (string-strip-newlines (substring s 0 (1- (string-length s)))))
        (else s)))

(define (timeout->select to)
  (if to
      (let ((s (inexact->exact (truncate to))))
        (values s (inexact->exact (round (* 1e6 (- to s))))))
      (values #f #f)))

(define (whitespace? x)
  (or (= x #x09)
      (= x #x0a)
      (= x #x0d)
      (= x #x20)))

(define (get-octet port)
  (let ((octet (get-bytevector-n port 1)))
    (if (eof-object? octet)
        octet
        (array-ref octet  0))))

(define (has-data? port)
  (let ((rv (select (list port) '() '() 0 0)))
    (not (null? (car rv)))))

(define (drain-whitespace port)
  (let loop ()
    (if (not (has-data? port))
        #f
        (let ((octet (get-octet port)))
          (cond ((eof-object? octet) eof-object)
                ((not (whitespace? octet))
                 (unget-bytevector port (list->u8vector (list octet))))
                (else (loop)))))))

(define (xselect r w e t)
  (let-values (((s us) (timeout->select t)))
    (if s
        (select r w e s us)
        (select r w e #f))))

(define* (xread port #:key handle-timeout timeout)
  (define (do-read port)
    (let ((rv (read port)))
      (drain-whitespace port)
      rv))
  (drain-whitespace port)
  (let ((rv (xselect (list port) '() '() timeout)))
    (if (null? (car rv))
        (if handle-timeout
            (handle-timeout rv)
            (throw 'xread-timeout rv))
        (do-read port))))
