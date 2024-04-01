(define-module (chip-remote utilities)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:export (↔
            !!
            2e
            non-negative-integer?
            addr=
            addr<
            addr>
            index?
            index=
            index<
            index>
            cat
            flatten
            assv-ref-reverse
            assq-change
            assoc-apply
            pair-combine
            list-iterate
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
            diff?
            string-ends-in-newline?
            string-strip-newlines
            now!
            seconds->microseconds
            timeout->select
            whitespace?
            has-data?
            drain-whitespace
            xread
            xselect
            zip2))

(define-syntax-rule (↔ (a v) ...) `((,a . ,v) ...))

(define-syntax-rule (cat str ...)
  (string-concatenate (list str ...)))

(define (flatten lst)
  "Flatten deep tree to a shallow list"
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append (flatten (car lst))
                      (flatten (cdr lst))))))

(define (zip2 la lb)
  "This is like ‘zip’ from `(srfi srfi-1)`, except that it returns a list of
pairs instead of a list of lists with two elements in them.

    (zip2 '(a c e) '(b d f)) => ((a . b) (c . d) (e . f))"
  (let next ((a la)
             (b lb)
             (acc '()))
    (cond ((any null? (list a b))
           (reverse acc))
          (else
           (next (cdr a) (cdr b)
                 (cons (cons (car a) (car b))
                       acc))))))

(define (assv-ref-reverse dict key)
  (let loop ((rest dict))
    (match rest
      (() #f)
      (((v . k) . rest) (if (eqv? k key) v (loop rest)))
      (_ #f))))

(define (assq-change alist key value)
  (let loop ((rest alist))
    (match rest
      (((k . v) . rest*)
       (if (eq? k key)
           (cons (cons key value) rest*)
           (cons (cons k v) (loop rest*))))
      (_ (list (cons key value))))))

(define (assoc-apply compare f v a)
  "Apply a function to the value of a key in an associative array.

The function returns the updated associative array.

  (assoc-apply eq? 1+ '((a . 0) (b . 0) (c . 0)) 'b)
    => ((a . 0) (b . 1) (c . 0))"
  (if (null? v)
      (throw 'unknown-key a)
      (let ((key (caar v)))
        (if (compare key a)
            (cons (cons key (f (cdar v))) (cdr v))
            (cons (car v) (assoc-apply compare f (cdr v) a))))))

(define (pair-combine f l)
  "Call f for pairs of values from l, returning a list of results.

Given a list (a b c d e), it calls f like this:

  (f a b)
  (f b c)
  (f c d)
  (f d e)

The function returns a list comprised of the return values of these function
calls. It returns the empty list for empty and singleton lists."
  (match l
    (() '())
    ((_) '())
    ((a b . rest) (cons (f a b) (pair-combine f (cons b rest))))))

(define (list-iterate f init default lst)
  (call/ec
   (lambda (k)
     (default (fold (lambda (x a)
                      (let* ((i (car a))
                             (next (f x (cdr a) i k)))
                        (cons (1+ i) next)))
                    (cons 0 init) lst)))))

(define (non-negative-integer? obj)
  (and (integer? obj)
       (not (negative? obj))))

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

(define (index? obj)
  (or (not obj)
      (non-negative-integer? obj)))

(define (index= a b)
  (eqv? a b))

(define (index< a b)
  (cond ((and (not a) (not b)) #f)
        ((not b) #f)
        ((not a) #t)
        (else (< a b))))

(define (index> a b)
  (not (or (index= a b)
           (index< a b))))

(define (string-ends-in-newline? s)
  (char=? #\newline (string-ref s (1- (string-length s)))))

(define (string-strip-newlines s)
  (cond ((string-null? s) s)
        ((string-ends-in-newline? s)
         (string-strip-newlines (substring s 0 (1- (string-length s)))))
        (else s)))

(define (seconds->microseconds secs)
  (let* ((full (inexact->exact (truncate secs)))
         (sub  (inexact->exact (round (* 1e6 (- full secs))))))
    (+ (* #e1e6 full) sub)))

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

(define (now!)
  (let ((now (gettimeofday)))
    (+ (* #e1e6 (car now))
       (cdr now))))
