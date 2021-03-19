(define-module (chip-remote device)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  #:use-module (chip-remote device access)
  #:use-module (chip-remote device transmit)
  #:use-module (chip-remote device spi)
  #:use-module (chip-remote item)
  #:use-module (chip-remote process-plist)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote page-map)
  #:export (generate-device
            make-device
            device?
            device-meta
            device-page-map
            device-register
            device-ref
            device-ref->address
            device-access
            device-address
            device-address-map
            address-map->addresses
            device-value-address
            device-registers
            device-item-names
            device-name
            device-default
            define-device
            find-canonical-address
            device-extract))

(define-record-type <device>
  (make-device meta page-map access)
  device?
  (meta device-meta)
  (page-map device-page-map)
  (access device-access))

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

(define-syntax generate-device
  (lambda (x)
    (syntax-case x ()
      ((_ pl ...)
       (with-syntax ((((mp ...)
                       ((acc-key acc-value) ...)
                       (transf ...)
                       ((key value) ...))
                      (process-plist #'(pl ...)
                                     group:page
                                     group:access
                                     group:transmit
                                     (group 'meta))))
         #`(make-device
            (list (cons key value) ...)
            (page-map-merge (list mp ...))
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

(define (device-register dev)
  (let ((pm (page-map-table (device-page-map dev))))
    ;; Only one page-map entry and no address on the page means there is no
    ;; real page-map in the chip, so it just has a single register-map.
    (if (and (= (length pm) 1)
             (eq? (caar pm) #f))
        (register-map-register (cdar pm))
        (throw 'more-than-single-register-map dev))))

(define (device-ref->address device name)
  (page-map-ref->address (device-page-map device) name))

(define (device-ref device name)
  (page-map-ref (device-page-map device) name))

(define device-address
  (case-lambda
    ((device page-addr)
     (page-map-address (device-page-map device) page-addr))
    ((device page-addr reg-addr)
     (register-map-address (page-map-address (device-page-map device)
                                             page-addr)
                           reg-addr))
    ((device page-addr reg-addr item-addr)
     (register-map-address (page-map-address (device-page-map device)
                                             page-addr)
                           reg-addr item-addr))
    ((device page-addr reg-addr name cnt)
     (register-map-address (page-map-address (device-page-map device)
                                             page-addr)
                           reg-addr name cnt))))

(define device-value-address
  (case-lambda
    ((device value page-addr)
     (list-ref value (or (page-address->index (device-page-map device)
                                              page-addr)
                         0)))
    ((device value page-addr reg-addr)
     (list-ref (device-value-address device value page-addr)
               (or (register-address->index (device-address device page-addr)
                                            reg-addr)
                   0)))
    ((device value page-addr reg-addr _)
     (device-value-address device value page-addr reg-addr))
    ((device value page-addr reg-addr _ __)
     (device-value-address device value page-addr reg-addr))))

(define (false? x)
  (not x))

(define (find-canonical-address dev addr)
  (define (maybe-address? a)
    (or (not a) (integer? a)))
  (match addr
    ;; Just a symbol
    ((? symbol? addr)
     (device-ref->address dev addr))
    ;; Just an integer, assume register address
    ((? integer? ra)
     (list #f ra))
    ;; (#f n) => address register n in page #f
    (((? false? pa) (? integer? ra))
     (list pa ra))
    ;; (n m) => Address specific item in page #f
    (((? integer? ra) (? integer? io))
     (list #f ra io))
    ;; This is a fully qualified address already
    (((? maybe-address? pa) (? maybe-address? ra) (? integer? io))
     (list pa ra io))))

(define (device-extract dev value addr)
  (let* ((faddr (apply find-canonical-address (cons dev (if (symbol? addr)
                                                            (list addr)
                                                            addr))))
         (part (apply device-address (cons dev faddr)))
         (pv (apply device-value-address (cons* dev value faddr))))
    `((address . ,faddr)
      (part . ,part)
      (value . ,pv)
      (item . ,(if (item? part) ((item-get part) pv) pv)))))

(define (addr< a b)
  (and (integer? a)
       (integer? b)
       (< a b)))

(define (device-address-map dev)
  (sort (page-map-fold
         (lambda (page-addr page acc)
           (cons (cons page-addr
                       (sort (register-map-fold
                              (lambda (reg-addr reg acc)
                                (cons reg-addr acc))
                              '() page)
                             addr<))
                 acc))
         '() (device-page-map dev))
        (lambda (a b)
          (addr< (car a) (car b)))))

(define (address-map->addresses lst)
  (apply append
         (map (lambda (a)
                (map (lambda (b)
                       (list (car a) b))
                     (cdr a)))
              lst)))
