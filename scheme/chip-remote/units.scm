;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote units)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-9 gnu)
  #:export (fundamental-unit?
            make-unit unit? unit-name unit-symbol unit-dim
            metre gram second ampere kelvin mole candela
            yotta zetta exa peta tera giga mega kilo hecto deca
            deci centi milli micro nano pico femto atto zepto yocto
            make-fundamental-dimension
            fundamental-dimension?
            fundamental-dimension-name
            length
            mass
            time
            electrical-current
            thermodynamic-temperature
            amount-of-substance
            luminous-intensity
            put-unit
            value+unit?
            vu-value
            vu-unit
            convert
            si-combine
            define-unit))

(define-immutable-record-type <si-prefix>
  (make-si-prefix name symbol convert-to convert-from combine)
  si-prefix?
  (name si-prefix-name)
  (symbol si-prefix-symbol)
  ;; Conversion from fundamental unit TO this one.
  (convert-to si-prefix-convert-to)
  ;; Conversion FROM this unit to the fundamental one.
  (convert-from si-prefix-convert-from)
  (combine si-prefix-combine))

(define-syntax-rule (new-si-prefix name sym factor)
  (define name
    (make-si-prefix 'name 'sym
                    (lambda (n) (/ 1 (* n factor)))
                    (lambda (n) (* n factor))
                    (lambda (s)
                      (symbol-append (si-prefix-symbol name) s)))))

(new-si-prefix yotta Y #e1e24)
(new-si-prefix zetta Z #e1e21)
(new-si-prefix exa   E #e1e18)
(new-si-prefix peta  P #e1e15)
(new-si-prefix tera  T #e1e12)
(new-si-prefix giga  G #e1e9)
(new-si-prefix mega  M #e1e6)
(new-si-prefix kilo  k #e1e3)
(new-si-prefix hecto h #e1e2)
(new-si-prefix deca da #e1e1)
(new-si-prefix deci  d #e1e-1)
(new-si-prefix centi c #e1e-2)
(new-si-prefix milli m #e1e-3)
(new-si-prefix micro µ #e1e-6)
(new-si-prefix nano  n #e1e-9)
(new-si-prefix pico  p #e1e-12)
(new-si-prefix femto f #e1e-15)
(new-si-prefix atto  a #e1e-18)
(new-si-prefix zepto z #e1e-21)
(new-si-prefix yocto y #e1e-24)

(define si-prefix-inversion-table
  (list (cons yotta yocto)
        (cons zetta zepto)
        (cons  exa   atto)
        (cons  peta femto)
        (cons  tera  pico)
        (cons  giga  nano)
        (cons  mega micro)
        (cons  kilo milli)
        (cons hecto centi)
        (cons  deca  deci)))

(define-immutable-record-type <fundamental-dimension>
  (make-fundamental-dimension name)
  fundamental-dimension?
  (name fundamental-dimension-name))

(define-syntax-rule (define-fundamental-dimension name rest ...)
  (define name (make-fundamental-dimension 'name)))

(define (record-fundim-printer dim port)
  (format port "<fundamental-dimension: ~a>"
          (fundamental-dimension-name dim)))

(set-record-type-printer! <fundamental-dimension> record-fundim-printer)

(define-fundamental-dimension length)
(define-fundamental-dimension mass)
(define-fundamental-dimension time)
(define-fundamental-dimension electrical-current)
(define-fundamental-dimension thermodynamic-temperature)
(define-fundamental-dimension amount-of-substance)
(define-fundamental-dimension luminous-intensity)

(define-immutable-record-type <unit>
  (make-unit* name symbol dimension convert-to convert-from)
  unit?
  (name unit-name change-unit-name)
  (symbol unit-symbol change-unit-symbol)
  (dimension unit-dim)
  ;; Conversion from fundamental unit TO this one.
  (convert-to unit-convert-to change-unit-convert-to)
  ;; Conversion FROM this unit to the fundamental one.
  (convert-from unit-convert-from change-unit-convert-from))

(define (record-unit-printer unit port)
  (format port "<unit: ~a, symbol: ~a, ~a>"
          (unit-name unit)
          (unit-symbol unit)
          (unit-dim unit)))

(set-record-type-printer! <unit> record-unit-printer)

(define* (make-unit name
                    #:key
                    (symbol #f)
                    (dimension #f)
                    (to identity)
                    (from identity))
  (make-unit* name symbol dimension to from))

(define-syntax-rule (define-unit name rest ...)
  (define name (make-unit 'name rest ...)))

(define (fundamental-unit? u)
  (and (fundamental-dimension? (unit-dim u))
       (equal? (unit-convert-to u) identity)
       (equal? (unit-convert-from u) identity)))

(define-unit metre   #:symbol 'm   #:dimension length)
(define-unit gram    #:symbol 'g   #:dimension mass)
(define-unit second  #:symbol 's   #:dimension time)
(define-unit ampere  #:symbol 'A   #:dimension electrical-current)
(define-unit kelvin  #:symbol 'K   #:dimension thermodynamic-temperature)
(define-unit mole    #:symbol 'mol #:dimension amount-of-substance)
(define-unit candela #:symbol 'cd  #:dimension luminous-intensity)

(define-immutable-record-type <value+unit>
  (put-unit value unit)
  value+unit?
  (value vu-value)
  (unit vu-unit))

(define (record-vu-printer x port)
  (format port "<value+unit: ~a ~a>"
          (vu-value x)
          (vu-unit x)))

(set-record-type-printer! <value+unit> record-vu-printer)

(define (convert x unit)
  (let ((xu (vu-unit x))
        (xv (vu-value x)))
    (put-unit ((unit-convert-to unit) ((unit-convert-from xu) xv))
              unit)))

(define (si-combine prefix unit)
  (let ((old-name (unit-name unit))
        (prefix-name (si-prefix-name prefix)))
    (change-unit-symbol
     (change-unit-name
      (change-unit-convert-from
       (change-unit-convert-to
        unit
        (lambda (x) ((si-prefix-convert-to prefix)
                     ((unit-convert-to unit) x))))
       (lambda (x) ((si-prefix-convert-from prefix)
                    ((unit-convert-from unit) x))))
      (if (list? old-name)
          (cons prefix-name old-name)
          (list prefix-name old-name)))
     ((si-prefix-combine prefix) (unit-symbol unit)))))
