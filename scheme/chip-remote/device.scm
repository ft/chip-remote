(define-module (chip-remote device)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
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
  #:use-module (data-structures sized-stack)
  #:export (generate-device
            make-device
            device?
            device-state
            new-device-state
            device-meta
            device-page-map
            device-register
            device-ref
            device-ref->address
            device-access
            device-address
            device-canonical-address
            device-address-map
            address-map->addresses
            device-value-address
            device-registers
            device-combinations
            device-item-names
            device-name
            device-default
            define-device
            find-canonical-address
            canonical-address->index
            device-extract))

(define-immutable-record-type <device>
  (make-device* meta page-map combinations access state)
  device?
  (meta device-meta)
  (page-map device-page-map)
  (combinations device-combinations)
  (access device-access)
  (state device-state new-device-state))

(define* (make-device meta page-map combinations access
                      #:key (state (make-sized-stack 128)))
  (make-device* meta page-map combinations access state))

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
     (if (eq? page-addr 'combinations)
         (assq-ref (device-combinations device) reg-addr)
         (register-map-address (page-map-address (device-page-map device)
                                                 page-addr)
                               reg-addr)))
    ((device page-addr reg-addr item-addr)
     (register-map-address (page-map-address (device-page-map device)
                                             page-addr)
                           reg-addr item-addr))
    ((device page-addr reg-addr name cnt)
     (register-map-address (page-map-address (device-page-map device)
                                             page-addr)
                           reg-addr name cnt))))

(define (device-canonical-address device addr-lst)
  (apply device-address (cons device addr-lst)))

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
     (let ((c (device-address dev 'combinations addr)))
       (if c
           (list 'combinations addr)
           (device-ref->address dev addr))))
    ;; Just an integer, assume register address
    ((? integer? ra)
     (list #f ra))
    ;; (#f n) => address register n in page #f
    (((? false? pa) (? integer? ra))
     (list pa ra))
    ;; (combinations name) => address combination name
    (('combinations (? symbol? name))
     (list 'combinations name))
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

(define (canonical-address->index device lst)
  (let* ((pm (device-page-map device))
         (pi (page-address->index pm (car lst)))
         (rm (cdr (list-ref (page-map-table pm) pi)))
         (ri (register-map-address->index rm (cadr lst))))
    (list pi ri (caddr lst))))
