;; Copyright (c) 2017-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote decode)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote combination)
  #:use-module (chip-remote decode types)
  #:use-module (chip-remote interpreter)
  #:use-module (chip-remote device)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-window)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote item)
  #:use-module (chip-remote semantics)
  #:export (decode
            decode*
            make-processor
            make-processor-state
            ps-address
            ps-content
            ps-debug?
            ps-level
            processor?
            p:item
            p:register
            p:window
            p:register-map
            p:page-map
            p:combination
            p:combinations
            p:device
            p:list
            p:pair
            p:other
            process))

(define-immutable-record-type <processor-state>
  (make-processor-state* level address debug?)
  processor-state?
  (content ps-content new-content)
  (level ps-level new-level)
  (address ps-address new-address)
  (debug? ps-debug? new-debug))

(define* (make-processor-state #:key (level '()) (address #f) (debug? #f))
  (make-processor-state* level address debug?))

(define (ps-level-up state lvl)
  (new-level state (cons lvl (ps-level state))))

(define-immutable-record-type <processor>
  (make-processor* item register window rm pm cmb cmbs dev lst pair other)
  processor?
  (item p:item)
  (register p:register)
  (window p:window)
  (rm p:register-map)
  (pm p:page-map)
  (cmb p:combination)
  (cmbs p:combinations)
  (dev p:device)
  (lst p:list)
  (pair p:pair)
  (other p:other))

(define* (make-processor
          #:key
          (item (lambda (proc state d:item)
                  (cons (item-name (decoder-item-description d:item))
                        (decoder-item-decoded d:item))))
          (register (lambda (proc state d:reg)
                      (let ((c (ps-content state)) (a (ps-address state)))
                        (if (integer? a) (cons a c) c))))
          (window (lambda (proc state d:win win lsi msi)
                    (cons (list `(lsi-complete? ,(lsi-complete? win) ,lsi)
                                `(msi-complete? ,(msi-complete? win) ,msi))
                          (ps-content state))))
          (register-map (lambda (proc state d:rm)
                          (cons (ps-address state) (ps-content state))))
          (page-map (lambda (proc state d:pm)
                      (ps-content state)))
          (combination (lambda (proc state d:combination)
                         (cons (decoder-combination-name d:combination)
                               (decoder-combination-decoded d:combination))))
          (combinations (lambda (proc state d:combinations)
                          (let ((c (ps-content state)))
                            (if (null? c)
                                c
                                (cons 'combinations c)))))
          (device (lambda (proc state d:dev)
                    (ps-content state)))
          (lst (lambda (proc state d:lst)
                 (ps-content state)))
          (pair (lambda (proc state d:pair)
                  (ps-content state)))
          (other (lambda (proc state d:other)
                   d:other)))
  (make-processor* item register window register-map page-map
                   combination combinations device
                   lst pair other))

(define (into-item proc state item)
  ((p:item proc) proc state item))

(define (into-register proc state reg)
  (define (into item)
    (process proc (ps-level-up state 'register) item))
  (let ((cb (p:register proc)))
    (cb proc (new-content state (map into (decoder-register-items reg))) reg)))

(define (into-register-window proc state regwin)
  (let* ((win (decoder-register-window-description regwin))
         (items (window-items win))
         (lsi (and (not (null? items)) (item-name (first items))))
         (msi (and (not (null? items)) (item-name (last items))))
         (c (cons (list `(lsi-complete? ,(lsi-complete? win) ,lsi)
                        `(msi-complete? ,(msi-complete? win) ,msi))
                  (map (lambda (i)
                         (process proc (ps-level-up state 'register-window) i))
                       (decoder-register-window-items regwin)))))
    ((p:register proc) proc (new-content state c) regwin)))

(define (into-register-map proc state regmap)
  (define (into reg)
    (process proc
             (set-fields state
                         ((ps-level) (cons 'register-map (ps-level state)))
                         ((ps-address) (car reg)))
             (cdr reg)))
  (let ((cb (p:register-map proc)))
    (cb proc
        (new-content state (map into (decoder-register-map-registers regmap)))
        regmap)))

(define (into-page-map proc state pagemap)
  (define (into regmap)
    (process proc
             (set-fields state
                         ((ps-level) (cons 'page-map (ps-level state)))
                         ((ps-address) (car regmap)))
             (cdr regmap)))
  (let ((cb (p:page-map proc)))
    (cb proc
        (new-content state (map into (decoder-page-map-register-maps pagemap)))
        pagemap)))

(define (into-combination proc state cmb)
  ((p:combination proc) proc state cmb))

(define (into-combinations proc state cmbs)
  (define (into combination)
    (process proc (ps-level-up state 'combination) combination))
  (let ((cb (p:combinations proc)))
    (cb proc (new-content state
                          (map into (decoder-combinations cmbs)))
        cmbs)))

(define (into-device proc state device)
  (let* ((cb (p:device proc))
         (next-level (ps-level-up state 'device))
         (with/page-map (process proc
                                 next-level
                                 (decoder-device-page-map device)))
         (with/combinations (process proc
                                     next-level
                                     (decoder-device-combinations device))))
    (cb proc (new-content state (if (null? with/combinations)
                                    with/page-map
                                    (cons with/combinations
                                          with/page-map)))
        device)))

(define (into-list proc state lst)
  (let ((cb (p:list proc)))
    (cb proc
        (new-content state (map (lambda (x) (process proc state x)) lst))
        lst)))

(define (into-pair proc state pair)
  (let ((cb (p:pair proc)))
    (cb proc
        (new-content state (cons (process proc state (car pair))
                                 (process proc state (cdr pair))))
        pair)))

(define (into-other proc state thing)
  ((p:other proc) proc state thing))

(define (thing->name thing)
  (cond ((decoder-item? thing) 'item)
        ((decoder-register? thing) 'register)
        ((decoder-register-window? 'register-window))
        ((decoder-register-map? 'register-map))
        ((decoder-page-map? 'page-map))
        ((decoder-combination? thing) 'combination)
        ((decoder-combinations? thing) 'combinations)
        ((decoder-device? 'device))
        ((list? 'list))
        ((pair? 'pair))
        (else 'other)))

(define (thing->cb thing)
  (cond ((decoder-item? thing) into-item)
        ((decoder-register? thing) into-register)
        ((decoder-register-window? thing) into-register-window)
        ((decoder-register-map? thing) into-register-map)
        ((decoder-page-map? thing) into-page-map)
        ((decoder-combination? thing) into-combination)
        ((decoder-combinations? thing) into-combinations)
        ((decoder-device? thing) into-device)
        ((list? thing) into-list)
        ((pair? thing) into-pair)
        (else into-other)))

(define (maybe-trace state thing)
  (when (ps-debug? state)
    (format #t "debug: (~a) [~a] <addr: ~a>~%"
            (thing->name thing)
            (ps-level state)
            (ps-address state))))

(define (process proc state thing)
  (maybe-trace state thing)
  ((thing->cb thing) proc state thing))

(define (decode* desc value)
  (cond ((item? desc)
         (let ((item-decoded (item-decode desc value)))
           (make-item/decoder item-decoded value desc)))
        ((register? desc)
         (make-register/decoder
          value
          (map (lambda (x)
                 (let ((getter (item-get x)))
                   (decode* x (getter value))))
               (register-items desc))
          desc))
        ((register-window? desc)
         (let ((new-value (ash value (window-offset desc))))
           (make-register-window/decoder
            new-value
            (map (lambda (x)
                   (let ((getter (item-get x)))
                     (decode* x (getter new-value))))
                 (window-items desc))
            desc)))
        ((register-map? desc)
         (make-register-map/decoder
          value
          (map (lambda (r v)
                 (cons (car r)
                       (decode* (cdr r) v)))
               (register-map-table desc)
               value)
          desc))
        ((page-map? desc)
         (make-page-map/decoder
          value
          (map (lambda (rm v)
                 (cons (car rm)
                       (decode* (cdr rm) v)))
               (page-map-table desc)
               value)
          desc))
        ((device? desc)
         (make-device/decoder
          value
          (let ((pm (device-page-map desc)))
            (make-page-map/decoder
             value
             (map (lambda (rm v)
                    (cons (car rm)
                          (decode* (cdr rm) v)))
                  (page-map-table pm)
                  value)
             pm))
          (let ((cs (device-combinations desc)))
            (make-combinations/decoder
             (map (lambda (c)
                    (let* ((data (combination-assemble desc (cdr c) value))
                           (raw (c:raw data))
                           (w (c:width data))
                           (s (c:semantics data))
                           (decoded (s:decode s w raw)))
                      (make-combination/decoder (car c) data decoded c)))
                  cs)
             cs))
          desc))
        (else (throw 'invalid-data-type desc))))

(define* (decode description #:optional value #:key debug?)
  (when (and (not (device? description))
             (not value))
    (throw 'missing-value-argument 'decode))
  (let ((value* (or value (current-device-state description))))
    (process (make-processor) (make-processor-state #:debug? debug?)
             (decode* description value*))))
