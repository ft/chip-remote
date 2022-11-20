(define-module (chip-remote device)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  #:use-module (chip-remote decode)
  #:use-module (chip-remote device access)
  #:use-module (chip-remote device transmit)
  #:use-module (chip-remote device spi)
  #:use-module (chip-remote item)
  #:use-module (chip-remote modify)
  #:use-module (chip-remote process-plist)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote utilities)
  #:use-module (data-structures sized-stack)
  #:export (generate-device          ;; Device creation
            make-device
            define-device
            device?                  ;; Device type API
            device-value-suitable?
            device-meta
            device-page-map
            device-combinations
            device-access
            device-state
            new-device-state
            current-device-state     ;; Device utilities
            push-device-state
            reset-device-state
            device-default
            device-diff
            device-history
            device-name
            device-registers
            device-item-names
            device-canonical
            device-ref
            device-address
            device-address:register
            device-address-map
            device-extract))

(define-immutable-record-type <device>
  (make-device* meta page-map combinations access state)
  device?
  (meta device-meta)
  (page-map device-page-map)
  (combinations device-combinations)
  (access device-access)
  (state device-state new-device-state))

(define (current-device-state device)
  (let ((state (device-state device)))
    (if (sized-stack-empty? state)
        (device-default device)
        (cdr (sized-stack-peek state)))))

(define (push-device-state device annotation value)
  (new-device-state device (sized-stack-push (device-state device)
                                             (cons annotation value))))

(define (reset-device-state device)
  (push-device-state device 'default (device-default device)))

(define* (make-device meta page-map combinations access
                      #:key (state (make-sized-stack 128)))
  (reset-device-state
   (make-device* meta page-map combinations access state)))

(define group:page
  (group 'pages
         #:type 'list
         #:predicate
         (lambda (x)
           (memq x '(#:page-map
                     #:page-map*
                     #:register-map
                     #:register-map*
                     #:register)))
         #:transformer
         (lambda (e)
           (syntax-case e ()
             ((#:page-map* exp)
              #'exp)
             ((#:page-map (e0 e* ...) ...)
              #'(list (generate-page-map (e0 e* ...)) ...))
             ((#:register-map* expr)
              #'(list (generate-page-map (#f expr))))
             ((#:register-map (e0 e* ...))
              #'(list (generate-page-map (#f (generate-register-map e0 e* ...)))))
             ((#:register (e0 e* ...))
              #'(list (generate-page-map (#f (generate-register-map
                                              #:table (#f (e0 e* ...)))))))))))

(define (prefix-syn prefix syn)
  (datum->syntax #'generate-device (symbol-append prefix (syntax->datum syn))))

(define group:access
  (group 'access
         #:type 'list
         #:predicate (lambda (x) (memq x '(#:bus #:read #:write)))
         #:transformer
         (lambda (e)
           (syntax-case e ()
             ((#:bus (type e* ...))
              (with-syntax ((make-type (prefix-syn 'make-device-access- #'type)))
                #'(#:bus (make-type e* ...))))
             (else e)))))

(define group:combinations
  (group 'combinations
         #:type 'list
         #:predicate (lambda (x) (eq? x #:combinations))
         #:transformer (lambda (e)
                         (syntax-case e ()
                           ((#:combinations e0 e* ...)  #'(list e0 e* ...))))))

(define group:transmit
  (group 'transmit
         #:type 'list
         #:predicate (lambda (x) (memq x '(#:transmit)))
         #:transformer
         (lambda (e)
           (syntax-case e ()
             ((#:transmit (type var transf))
              #'(make-device-transmit #:type 'type #:variant 'var
                                      #:transform transf))
             ((#:transmit (type var))
              #'(make-device-transmit #:type 'type #:variant 'var))
             ((#:transmit (type))
              #'(make-device-transmit #:type 'type))
             ((#:transmit type)
              #'(make-device-transmit #:type 'type))))))

(define (make-combinations-script x)
  (match x
    ((name e0 e* ...) (cons* name e0 e*))
    (_ (throw 'cr/empty-combination-definition x))))

(define (elaborate-combinations . lst)
  (if (null? lst)
      lst
      (map make-combinations-script (car lst))))

(define-syntax generate-device
  (lambda (x)
    (syntax-case x ()
      ((_ pl ...)
       (with-syntax ((((mp ...)
                       (cmb ...)
                       ((acc-key acc-value) ...)
                       (transf ...)
                       ((key value) ...))
                      (process-plist #'(pl ...)
                                     group:page
                                     group:combinations
                                     group:access
                                     group:transmit
                                     (group 'meta))))
         #`(make-device
            (list (cons key value) ...)
            (page-map-merge (list mp ...))
            (elaborate-combinations cmb ...)
            (make-device-access #,@(zip-syms #'(acc-key ...)
                                             #'(acc-value ...))
                                #,@(if (null? #'(transf ...)) #'()
                                       #'(#:transmit transf ...)))))))))

(define-syntax-rule (define-device binding e0 e* ...)
  (define binding (generate-device #:name 'binding e0 e* ...)))

(define (device-name dev)
  (let* ((meta (device-meta dev))
         (name (assq-ref meta #:name)))
    (if name name '*unnamed-device*)))

(define (device-default dev)
  (page-map-default (device-page-map dev)))

(define (device-item-names dev)
  (page-map-item-names (device-page-map dev)))

(define (device-registers dev)
  (map cdr
       (apply append (map register-map-table
                          (map cdr
                               (page-map-table (device-page-map dev)))))))

(define (device-address-map dev)
  (sort (page-map-fold
         (lambda (page-addr page acc)
           (cons (cons page-addr
                       (sort (register-map-fold
                              (lambda (reg-addr reg acc)
                                (cons reg-addr acc))
                              '() page)
                             index<))
                 acc))
         '() (device-page-map dev))
        (lambda (a b)
          (index< (car a) (car b)))))

(define (device-canonical d . item-address)
  (apply page-map-canonical (cons (device-page-map d) item-address)))

(define (device-address d . item-address)
  (apply page-map-address (cons (device-page-map d) item-address)))

(define (device-address:register d . item-address)
  (apply page-map-address:register (cons (device-page-map d) item-address)))

(define (device-ref d . args)
  (apply page-map-ref (cons (device-page-map d) args)))

(define (device-value-suitable? d v)
  "Test whether ‘v’ is a suitable value for the device ‘d’."
  (structurally-equal? v (device-default d)))

(define (fe0 lst)
  (filter (compose not null?) lst))

(define (fe1 lst)
  (filter (lambda (x) (or (and (pair? x)
                               (diff? (cdr x)))
                          (> (length x) 1)))
          lst))

(define (minimise-diff/item item)
  (match item
    ((name . value) (if (diff? value)
                        item
                        '()))))

(define (minimise-diff/register reg)
  (match reg
    ((addr items ...) (cons addr (fe0 (map minimise-diff/item items))))
    ((addr . value)   (if (diff? value)
                          (cons addr value)
                          '()))))

(define (minimise-diff/page page)
  (match page
    (('combinations kv ...) (cons 'combinations
                                  (fe0 (map minimise-diff/item kv))))
    ((addr kv ...) (cons addr
                         (fe1 (map minimise-diff/register kv))))))

(define (minimise-diff/device device)
  (fe1 (map minimise-diff/page device)))

(define (minimise-diff data)
  (match data
    ((mark . rest) (cons mark (minimise-diff/device rest)))))

(define (xdiff a b)
  (let ((ra (cdr a))
        (rb (cdr b)))
    (cons (car b)
          (let-values (((diff? v) (diff ra rb)))
            v))))

(define* (device-history dev #:optional count #:key (decode? #t))
  (let* ((dec (lambda (x) (cons (car x) (decode dev (cdr x)))))
         (st (device-state dev))
         (count* (and count (min (1+ count) (sized-stack-used st))))
         (memory (sized-stack-memory st))
         (data (reverse (if count* (take memory count*) memory))))
    (if decode? (map dec data) data)))

(define* (device-diff dev #:optional count #:key (decode? #t))
  (map minimise-diff
       (pair-combine xdiff
                     (device-history dev count #:decode? decode?))))

(define (device-extract dev value addr)
  (let* ((faddr (apply device-canonical
                       (cons dev (if (list? addr) addr (list addr)))))
         (part (apply device-ref (cons dev faddr)))
         (pv (apply value-at-address (cons value faddr))))
    `((address . ,faddr)
      (part . ,part)
      (value . ,pv)
      (item . ,(if (item? part) (item-get part pv) pv)))))
