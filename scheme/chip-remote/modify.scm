;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote modify)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (chip-remote combination)
  #:use-module (chip-remote device)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote item)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote interpreter)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote simplify)
  #:use-module (chip-remote utilities)
  #:use-module (chip-remote validate)
  #:export (modify
            modify*
            chain-modify
            chain-modify*
            chain-modify-script
            minimise-modify-script
            merge-minimised-script
            replace-register-value
            values-for-minimised-script
            apply-modify-script
            register-matches?
            regmap-matches?
            make-item-mod-expr))

(define (not-integer? x)
  "Predicate for values that are anything **but** integers."
  (not (integer? x)))

(define (modify-ref reg addr)
  "Reference an item in a register

This function supports all addressing schemes that chain-modify advertises at a
register level:

    INTEGER             Reference the Nth item in the register.
    (INTEGER)           Same as the previous.
    SYMBOL              Reference the first item named SYMBOL.
    (SYMBOL)            Same as the previous.
    (SYMBOL INTEGER)    Reference the Nth item named SYMBOL."
  (match addr
    (((? integer? i)) (register-address reg i))
    ((? integer? i) (register-address reg i))
    (((? not-integer? name)) (register-ref reg name))
    ((n (? integer? i)) (register-address reg n i))
    ((? not-integer? name) (register-ref reg name))
    (_ (throw 'unknown-addressing-scheme addr))))

(define (modify-register reg init addr value)
  "Modification backend for registers.

See the modify function about parameters' semantics."
  ;;(format #t "debug: ~a ~a ~a~%" init addr value)
  (let ((item (modify-ref reg addr)))
    (unless (item? item)
      (throw 'addressing-returned-non-item item addr))
    (if (validate-item-value item value)
        ((item-set item) init (item-encode item value))
        (throw 'invalid-value-for-item value item))))

(define (register-matches? reg addr)
  "Checks if an address (`addr`) references something in the given
register (`reg`)."
  (match addr
    ((ra . rest) (eqv? ra (car reg)))
    (name (register-ref (cdr reg) name))
    (_  #f)))

(define (regmap-matches? regmap addr)
  "Checks if an address (`addr`) references something in the given
register-map (`regmap`)."
  (match addr
    ((rma . rest) (eqv? rma (car regmap)))
    (name (register-map-ref (cdr regmap) name))
    (_  #f)))

(define (modify-register-map rm init addr value)
  "Modification backend for register-maps.

See the modify function about parameters' semantics."
  (map (lambda (r v)
         (if (register-matches? r addr)
             (modify-register (cdr r) v
                              (if (list? addr) (cdr addr) addr)
                              value)
             v))
       (register-map-table rm)
       init))

(define (modify-page-map pm init addr value)
  "Modification backend for page-maps.

See the modify function about parameters' semantics."
  (map (lambda (rm v)
         (if (regmap-matches? rm addr)
             (modify-register-map (cdr rm) v
                                  (if (list? addr) (cdr addr) addr)
                                  value)
             v))
       (page-map-table pm)
       init))

(define (modify-device dev init addr value)
  "Modification backend for devices.

See the modify function about parameters' semantics."
  (modify-page-map (device-page-map dev) init addr value))

(define (modify target init address value)
  "Performs a modification on a target

The ‘address’ parameter will be used to reference an item in the target. The
‘init’ parameter is the value to apply the modification to. And ‘value’ is the
value used with the item's semantics to produce the desired result."
  ((cond ((register? target) modify-register)
         ((register-map? target) modify-register-map)
         ((page-map? target) modify-page-map)
         ((device? target) modify-device)
         (else (throw 'invalid-target target init address value)))
   target init address value))

(define (default-by-target target)
  "Returns the default value of a target."
  ((cond ((register? target) register-default)
         ((register-map? target) register-map-default)
         ((page-map? target) page-map-default)
         ((device? target) device-default)
         (else (throw 'invalid-target target)))
   target))

(define (modify* target address value)
  "This is the same as ‘modify’ with its ‘init’ parameter set to the default
value that can be derived for ‘target’."
  (modify target (default-by-target target) address value))

(define (chain-modify target init . lst)
  "Apply multiple modifications to a target

With a `target` being one of: `register`, `register-map`, `page-map` or
`device`. The `init` parameter is the value from which to start applying all
the listed modifications, which are passed to the function as all parameters
following the `init` parameter.

Example:

    (chain-modify (device-default adf4169)
                  adf4169
                  '(deviation-offset 9)
                  '(phase -12)
                  '(delay-clock-select pfd-clock-times-clock1)
                  '(ramp-mode triangular)
                  '(ramp-enabled? yes)))

The modifications are of the form (ADRESS VALUE), where ADDRESS is either a
symbol, that could be passed to one of the `*-ref` functions (like device-ref)
or a list that could be passed to one of the `*-address` functions (like
device-address) to reference an item."
  (if (device? target)
      (apply-modify-script target init
                           (apply chain-modify-script (cons target lst)))
      (if (null? lst)
          init
          (apply chain-modify
                 (cons target
                       (cons (modify target init (caar lst) (cadar lst))
                             (cdr lst)))))))

(define (chain-modify* target . lst)
  "This is the same as ‘chain-modify’ with its ‘init’ parameter set to the
default value that can be derived for ‘target’."
  (apply chain-modify (cons target (cons (default-by-target target) lst))))

(define (make-item-mod-expr d addr v)
  (match addr
    ((p r i) (let* ((reg (device-address d p r))
                    (item (register-address reg i)))
               (list reg p r i v item)))))

(define (chain-modify-script device . lst)
  (fold (lambda (av acc)
          (match av
            ((addr value)
             (let* ((full-addr (find-canonical-address device addr))
                    (description (device-canonical-address device full-addr)))
               (match full-addr
                 (('combinations name)
                  (append acc (combination-partition device description value)))
                 (else (append acc (list (make-item-mod-expr device full-addr value)))))))))
        '()
        lst))

(define (iterate-to-index idx data cb)
  (let loop ((i 0) (rest data))
    (cond ((null? rest) '())
          ((= i idx) (cons (cb (car rest))
                           (loop (1+ i) (cdr rest))))
          ((< i idx) (cons (car rest) (loop (1+ i) (cdr rest))))
          (else rest))))

(define (update-item register-value item item-value)
  (let ((set (item-set item)))
    (set register-value
         (if (validate-item-value item item-value)
             (item-encode item item-value)
             (throw 'invalid-value-for-item item-value item)))))

(define (modify-value-by-index device-value index item item-value)
  (let ((update (lambda (v) (update-item v item item-value))))
    (match index
      ((pi ri ii)
       (iterate-to-index pi device-value
                         (lambda (regs)
                           (iterate-to-index ri regs update)))))))

(define (page+reg-address->index device lst)
  (let* ((pm (device-page-map device))
         (pi (page-address->index pm (car lst)))
         (rm (cdr (list-ref (page-map-table pm) pi)))
         (ri (register-map-address->index rm (cadr lst))))
    (list pi ri)))

(define (replace-register-value device device-value address value)
  (let ((replace (lambda (x) value)))
    (match (page+reg-address->index device address)
      ((pi ri)
       (iterate-to-index pi device-value
                         (lambda (regs)
                           (iterate-to-index ri regs replace)))))))

(define (script-expression->register-address expr)
  (match expr
    ((reg p r i v item) (list p r))
    (((reg p r) ((is os vs items) ...)) (list p r))))

(define (script-address< a b)
  (addr< (script-expression->register-address a)
         (script-expression->register-address b)))

(define (expr->segmented-expr expr)
  (match expr
    ((reg p r i v item) (list (list i 0 v item)))
    (((reg p r) (segmented-expr ...)) segmented-expr)))

(define (make-segmented-script-state first script)
  (match first
    ((reg p r i v item)
     (list (list p r) (list reg p r) (list (list i 0 v item)) script))
    (((reg p r) (expr ...))
     (list (list p r) (list reg p r) expr script))))

(define (add-expr-to-segment expr last-address segment segment-exprs script)
  (list last-address
        segment
        (append (expr->segmented-expr expr) segment-exprs)
        script))

(define (extend-segmented-script segment segment-exprs script)
  (append script (list (list segment (reverse segment-exprs)))))

(define (add-segment-to-script expr segment segment-exprs script)
  (make-segmented-script-state expr (extend-segmented-script segment
                                                             segment-exprs
                                                             script)))

(define (flush-segmented-script state)
  (match state
    ((last-address segment segment-exprs script)
     (if (null? segment-exprs)
         script
         (extend-segmented-script segment segment-exprs script)))))

(define (make-segmented-script expr state)
  (match state
    ((last-address segment segment-exprs script)
     (let ((expr-address (script-expression->register-address expr)))
       (if (addr= last-address expr-address)
           (add-expr-to-segment expr last-address segment segment-exprs script)
           (add-segment-to-script expr segment segment-exprs script))))))

(define (minimise-modify-script script)
  (if (null? script)
      script
      (flush-segmented-script
       ;; This needs to be stable to keep the order of changes in sub
       ;; expressions.
       (let ((sorted-script (stable-sort script script-address<)))
         (fold make-segmented-script
               (make-segmented-script-state (car sorted-script) '())
               (cdr sorted-script))))))

(define (merge-segment segment value)
  (match segment
    (((reg p r) ((is os vs items) ...))
     (list p r (fold (lambda (i o v item last)
                       ((item-set item) last (item-encode item v)))
                     value
                     is os vs items)))))

(define (merge-minimised-script script lst)
  (map merge-segment script lst))

(define (values-for-minimised-script device script value)
  (map (lambda (expr)
         (match expr
           (((reg p r) (e ...))
            (device-value-address device value p r))))
       script))

(define (apply-modify-expr device value expr)
  (match expr
    ((reg p r i v item) ;; Item modification expression
     (modify-value-by-index value
                            (canonical-address->index device (list p r i))
                            item v))
    (((reg p r) ((is os vs items) ...)) ;; Combination modification expression
     (fold (lambda (i v item acc)
             (apply-modify-expr device acc `(,reg ,p ,r ,i ,v ,item)))
           value
           is vs items))))

(define (apply-modify-script device value script)
  (fold (lambda (expr acc)
          (apply-modify-expr device acc expr))
        value
        script))
