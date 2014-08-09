;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote decode)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 session)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote keyword-assoc)
  #:use-module (chip-remote bit-decoders)
  #:export (decode
            decode-many))

(define (invoke-decoder decoder name offset width bits)
  (let ((args (procedure-arguments decoder)))
    (if (= 1 (length (assq-ref args 'required)))
        (decoder bits)
        (decoder name offset width bits))))

(define (decode-content value content)
  (let* ((name (car content))
         (offset (cadr content))
         (width (caddr content))
         (extract (cadddr content))
         (bits (extract value))
         (decoder* (list-ref content 5))
         (decoder (caddr decoder*))
         (decoder-type (cadr decoder*))
         (decoder-unit (cdr (cadddr decoder*)))
         (decoded (if (eq? decoder-type 'list)
                      (reverse-lookup (decoder) bits)
                      (invoke-decoder decoder name offset width bits))))
    (list name
          (cons 'decoded decoded)
          (cons 'bits bits)
          (cons 'offset offset)
          (cons 'width width)
          (cons 'unit (cond ((and (thunk? decoder)
                                  (eq? decoded 'undefined))
                             ;; The values was retrieved by `reverse-lookup',
                             ;; but that failed. No unit then.
                             #f)
                            ((thunk? decoder-unit)
                             (decoder-unit))
                            ((procedure? decoder-unit)
                             (decoder-unit `((name . ,name)
                                             (raw . ,bits)
                                             (decoder . ,decoder)
                                             (decoded . ,decoded))))
                            (else decoder-unit))))))

(define (decode regmap address value)
  (let ((reg (assoc address regmap)))
    (if (not reg)
        (throw 'cr-no-such-register address)
        (map (lambda (x)
               (decode-content value x))
             (assq-ref (cdr reg) 'contents)))))

;; That's all for decoding a register. The rest of the module is about decoding
;; data that spans across multiple registers or where the values of an entry
;; can only be decoded in connections with the values of other entries.

(define (get-entry register-values address)
  (assoc-ref register-values address))

(define (get-bits entry values)
  (assq-ref (assq-ref values entry) 'bits))

(define (get-decoded entry values)
  (assq-ref (assq-ref values entry) 'decoded))

(define (get-value register-values address entry)
  (get-bits entry (get-entry register-values address)))

(define (get-decoded-value register-values address entry)
  (get-decoded entry (get-entry register-values address)))

(define (get-width regmap address entry)
  (cadr (regmap->entry regmap address entry)))

(define (fill-combination regmap entry values)
  (let* ((name (kwa-ref entry #:into))
         (srcs (kwa-ref entry #:sources))
         (width (kwa-ref entry #:width))
         (source-values (map (lambda (x)
                               (let ((name (car x))
                                     (addr (cdr x)))
                                 (list name
                                       (get-value values addr name)
                                       (get-width regmap addr name)
                                       addr)))
                             srcs))
         (combiner (kwa-ref entry #:combine))
         (combined (combiner source-values))
         (decoder (kwa-ref entry #:finally))
         (decoded (decoder name combined)))
    (list (cons 'combined-value name)
          (cons 'sources source-values)
          (list 'combined combined (if width
                                       width
                                       (fold (lambda (x acc)
                                               (+ (caddr x) acc))
                                             0
                                             source-values)))
          (cons 'decoded decoded))))

(define (fill-depends regmap entry data unresolved resolved)
  (define (get-resolved name)
    (let loop ((rest resolved))
      (cond ((null? rest) #f)
            ((eq? (cdaar rest) name) (car rest))
            (else (loop (cdr rest))))))
  (define (get-raw v)
    (let ((type (caar v)))
      (cond ((eq? type 'combined-value)
             (assq-ref v 'combined))
            (else (assq-ref v 'raw)))))
  (define (lookup-raw name addr)
    (if addr
        (get-value data addr name)
        (get-raw res)))
  (define (lookup-decoded name addr)
    (if addr
        (get-decoded-value data addr name)
        (assq-ref res 'decoded)))
  (let ((type (car entry)))
    (cond ((eq? type 'depends)
           (let* ((target (kwa-ref entry #:target))
                  (name (car target))
                  (addr (cdr target))
                  (deps (kwa-ref entry #:on))
                  (raw (lookup-raw name addr))
                  (dep-raw (map (lambda (x)
                                  (let ((name (car x))
                                        (addr (cdr x)))
                                    (list x
                                          (lookup-raw name addr)
                                          (get-width regmap addr name))))
                                deps))
                  (dep-decoded (map (lambda (x)
                                      (let ((name (caar x))
                                            (addr (cdar x)))
                                        (cons name (lookup-decoded name addr))))
                                    dep-raw))
                  (decoder (kwa-ref entry #:finally))
                  (decoded (decoder name raw dep-raw dep-decoded)))
             (list (cons 'dependent-value name)
                   (cons 'address addr)
                   (cons 'dependencies deps)
                   (cons 'dep-raw (map (lambda (x)
                                         (cons (caar x) (cdr x)))
                                       dep-raw))
                   (cons 'dep-decoded dep-decoded)
                   (list 'raw raw (get-width regmap addr name))
                   (cons 'decoded decoded))))
          (else entry))))

(define (fill-interconnections regmap inter values)
  (let loop ((in inter) (out '()))
    (if (null? in)
        out
        (loop (cdr in)
              (append! out
                       (cond ((eq? (caar in) 'combine)
                              (list (fill-combination regmap
                                                      (car in)
                                                      values)))
                             ((eq? (caar in) 'depends)
                              (list (fill-depends regmap
                                                  (car in)
                                                  values
                                                  in
                                                  out)))
                             (else (throw 'cr-unknown-interconnection-type
                                          (caar in)))))))))

(define* (decode-many #:key
                      register-map
                      reader
                      decoder
                      (filter-predicate #f)
                      (interconnections '()))
  (let* ((all-addresses (map car register-map))
         (wanted-addresses (if (not (procedure? filter-predicate))
                               all-addresses
                               (filter filter-predicate all-addresses)))
         (v (map reader wanted-addresses))
         (decoded-values (map (lambda (x)
                                (cons (car x) (decoder (car x) (cadr x))))
                              (zip wanted-addresses v))))
    (values v
            decoded-values
            (fill-interconnections register-map
                                   interconnections
                                   decoded-values))))
