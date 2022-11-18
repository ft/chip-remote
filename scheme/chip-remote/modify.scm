;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
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
  #:export (chain-modify
            chain-modify*
            xchain-modify
            xchain-modify*
            xcanonical
            xdefault
            xref
            modify
            modify*
            apply-at-address
            value-at-address
            chain-modify-script
            minimise-modify-script
            merge-minimised-script
            replace-register-value
            values-for-minimised-script
            apply-modify-script
            make-item-mod-expr))

(define (apply-at-address* f v addr)
  (match addr
    ((a)     (assoc-apply eqv? f v a))
    ((a . r) (assoc-apply eqv? (lambda (v) (apply-at-address* f v r)) v a))))

(define (apply-at-address f v a)
  "Apply the function f to the element in the value v addressed by a.

The address ‘a’ has to be a canonical address as used by the -ref functions in
the library's modules, applicable to the given value ‘v’. The function ’f’ is
applied to the extracted value and the return value of ‘apply-at-address’ is
the original value with the result of the function application at the addressed
position."
  (if (and (list? a) (> (length a) 1))
      (catch 'unknown-key
        (lambda () (apply-at-address* f v (drop-right a 1)))
        (lambda (k . s) (throw 'invalid-address (car s) a)))
      (throw 'invalid-address a)))

(define (value-at-address v . addr)
  (if (integer? v)
      v
      (match addr
        (((? index? n)) (assv-ref v n))
        (((? index? n) . rest)
         (apply value-at-address (cons (value-at-address v n) rest)))
        (_ (throw 'invalid-address addr)))))

(define (xdefault target)
  "Returns the default value of a target."
  ((cond ((register? target) register-default)
         ((register-map? target) register-map-default)
         ((page-map? target) page-map-default)
         ((device? target) device-default)
         (else (throw 'invalid-target target)))
   target))

(define (xcanonical target . args)
  (apply (cond ((register? target) register-canonical)
               ((register-map? target) register-map-canonical)
               ((page-map? target) page-map-canonical)
               ((device? target) device-canonical)
               (else (throw 'invalid-target target)))
         (cons target args)))

(define (xref target . args)
  (apply (cond ((register? target) register-ref)
               ((register-map? target) register-map-ref)
               ((page-map? target) page-map-ref)
               ((device? target) device-ref)
               (else (throw 'invalid-target target)))
         (cons target args)))

(define (modify target init address value)
  (let* ((args ((if (list? address) cons list) target address))
         (ca (apply xcanonical args))
         (item (apply xref (cons target ca)))
         (f (lambda (v)
              (item-set item v (item-encode item value)))))
    (cond ((register? target)
           (update-item init (apply register-ref (cons target ca)) value))
          (else (apply-at-address f init ca)))))

(define (modify* target address value)
  (modify target (xdefault target) address value))

(define (xchain-modify target init . lst)
  (fold (lambda (e acc)
          (modify target acc (car e) (cadr e)))
        init lst))

(define (xchain-modify* target . lst)
  (apply xchain-modify (cons* target (xdefault target) lst)))

(define (modify* target address value)
  "This is the same as ‘modify’ with its ‘init’ parameter set to the default
value that can be derived for ‘target’."
  (modify target (xdefault target) address value))

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
  (cond ((null? lst) init)
        ((device? target)
         (apply-modify-script
          target init (apply chain-modify-script (cons target lst))))
        (else (apply xchain-modify (cons* target init lst)))))

(define (chain-modify* target . lst)
  "This is the same as ‘chain-modify’ with its ‘init’ parameter set to the
default value that can be derived for ‘target’."
  (apply chain-modify (cons target (cons (xdefault target) lst))))

(define (make-item-mod-expr d addr v)
  (match addr
    ((p r i) (let ((reg (apply device-ref (cons d (drop-right addr 1))))
                   (item (apply device-ref (cons d addr))))
               (list reg p r i v item)))))

(define (chain-modify-script device . lst)
  (fold (lambda (av acc)
          (match av
            ((addr value)
             (let* ((full-addr (apply device-canonical ((if (list? addr) cons list) device addr)))
                    (description (apply device-ref (cons device full-addr))))
               (match full-addr
                 (('combinations name)
                  (append acc (combination-partition device description value)))
                 (else
                  (append acc (list (make-item-mod-expr device full-addr value)))))))))
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
  (item-set item register-value
            (if (validate-item-value item item-value)
                (item-encode item item-value)
                (throw 'invalid-value-for-item item-value item))))

(define (modify-value-by-index device-value index item item-value)
  (let ((update (lambda (v) (update-item v item item-value))))
    (match index
      ((pi ri ii)
       (iterate-to-index pi device-value
                         (lambda (regs)
                           (iterate-to-index ri regs update)))))))

(define (replace-register-value device device-value address value)
  (apply-at-address (const value) device-value address))

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
                       (item-set item last (item-encode item v)))
                     value
                     is os vs items)))))

(define (merge-minimised-script script lst)
  (map merge-segment script lst))

(define (values-for-minimised-script device script value)
  (map (lambda (expr)
         (match expr
           (((reg p r) (e ...)) (value-at-address value (list p r)))))
       script))

(define (apply-modify-expr device value expr)
  (match expr
    ((reg p r i v item) ;; Item modification expression
     (modify (device-page-map device) value (list p r i) v))
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
