;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote combination)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote device)
  #:use-module (chip-remote item)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:export (<=>
            make-default-combination
            combination-assemble
            combination-partition
            combination?
            c:device
            c:spec
            c:raw
            c:semantics
            c:width))

(define-syntax-rule (<=> e ...) (generate-semantics e ...))

(define-immutable-record-type <combination>
  (make-combination device spec raw semantics width)
  combination?
  (device c:device)
  (spec c:spec)
  (raw c:raw)
  (semantics c:semantics)
  (width c:width))

(define (make-default-combination device spec)
  (make-combination device spec 0 unsigned-integer 0))

(define (assemble:concatenate state value items*)
  (let* ((device (c:device state))
         (items (map (lambda (i) (device-extract device value i)) items*))
         (result (fold-right
                  (lambda (x a)
                    (let ((item (assq-ref x 'part))
                          (v (assq-ref x 'item))
                          (n (cdr a)))
                      (cons (logior (car a) (ash v n))
                            (+ n (item-width item)))))
                  (cons (c:raw state) 0)
                  items)))
    (set-fields state
                ((c:raw) (car result))
                ((c:width) (+ (c:width state) (cdr result))))))

(define (partition:concatenate state value items*)
  (let* ((device (c:device state))
         (items (map (lambda (i)
                       (let* ((addr (device-canonical device i))
                              (item (apply device-ref (cons device addr))))
                         (cons item addr)))
                     items*))
         (result (fold-right
                  (lambda (x a)
                   (let ((item (car x))
                          (lst (car a))
                          (w (cdr a)))
                      (cons (cons (cons w x) lst)
                            (+ w (item-width item)))))
                  (cons '() 0)
                  items)))

    (set-fields state
                ((c:raw) (cons 'concatenate (car result)))
                ((c:width) (+ (c:width state) (cdr result))))))

(define (regaddr a)
  (take a 2))

(define (partition:merge:concatenation device value parts)
  (let ((sorted (sort parts (lambda (a b) (addr< (cddr a) (cddr b))))))
    (fold (lambda (x acc)
            (let* ((offset (car x))
                   (item (cadr x))
                   (v (bit-extract-width value offset (item-width item)))
                   (ia (cddr x))
                   (iia (caddr ia))
                   (ra (regaddr ia))
                   (reg (apply device-ref device ra)))
              (if (null? acc)
                  (list (list (cons reg ra) (list (list iia offset v item))))
                  (match acc
                    ((((r . addr) lst) rest ...)
                     (if (equal? addr ra)
                         (cons (list (cons r addr)
                                     (cons (list iia offset v item)
                                           lst))
                               rest)
                         (cons (list (cons reg ra)
                                     (list (list iia offset v item)))
                               acc)))))))
          '()
          sorted)))

(define (partition:merge value state)
  (let ((raw-value (semantics-encode (c:semantics state)
                                     (c:width state)
                                     value)))
    (match (c:raw state)
      (('concatenate parts ...)
       (set-field state (c:raw)
                  (partition:merge:concatenation (c:device state)
                                                 raw-value
                                                 parts)))
      (_ (throw 'cr/invalid-combination-type (c:raw state))))))

(define (resolve-semantics state)
  (let ((s (c:semantics state)))
    (if (procedure? s)
        (set-field state (c:semantics) (s (c:width state)))
        state)))

(define* (run state spec value #:key (post identity) (concatenate identity))
  (match spec
    (() (post state))
    ((? semantics? x) (set-field state (c:semantics) x))
    (('~ exprs ...) (post (fold (lambda (x a)
                                  (run a x value
                                       #:post post
                                       #:concatenate concatenate))
                                state
                                exprs)))
    (('concatenate items ...) (post (concatenate state value items)))
    (_ (throw 'cr/combination-unknown-expression spec))))

(define (run-assemble state spec value)
  (run state spec value
       #:post resolve-semantics
       #:concatenate assemble:concatenate))

(define (run-partition state spec value)
  (c:raw (run state spec value
              #:post (compose (lambda (s) (partition:merge value s))
                              resolve-semantics)
              #:concatenate partition:concatenate)))

(define* (combination-assemble device spec #:optional value/maker)
  (unless (device? device)
    (throw 'cr/invalid-argument 'device device))
  (let ((value (cond ((procedure? value/maker) (value/maker device))
                     ((not value/maker) (device-default device))
                     ((list? value/maker) value/maker)
                     (else (throw 'cr/invalid-argument
                                  'procedure-or-list value/maker)))))
    (run-assemble (make-default-combination device spec) spec value)))

(define* (combination-partition device spec value)
  "Return a script that can apply the combination VALUE to a DEVICE via SPEC.

The returned script has the following form:

  (((reg page-addr reg-addr) ((item-addr combination-offset value item)
                              ...))
   ...)

This structure can be used to modify all required registers of a device to
reflect the desired value of the entire combination."
  (unless (device? device)
    (throw 'cr/invalid-argument 'device device))
  (run-partition (make-default-combination device spec) spec value))
