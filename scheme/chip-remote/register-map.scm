(define-module (chip-remote register-map)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  #:use-module (chip-remote item)
  #:use-module (chip-remote register)
  #:use-module (chip-remote utilities)
  #:use-module (data-structures records)
  #:use-module (data-structures records utilities)
  #:export (register-map                    ;; RegisterMap creation
            rm→
            make-register-map
            define-register-map
            register-map?                   ;; RegisterMap type API
            register-map-name
            register-map-width
            register-map-table
            register-map-table:sorted       ;; RegisterMap utilities
            register-map-registers
            register-map-default
            register-map-item-names
            register-map-register
            register-map-fold
            register-map-fold-right
            register-map-canonical
            register-map-find-item
            register-map-ref
            register-map-address
            register-map-address:register))

(define-record-type* <register-map>
  register-map make-register-map register-map? this-register-map
  (name  register-map-name (default #f))
  (width register-map-width (default #f))
  (table register-map-table
         (sanitize
          (need 'table
                (lambda (tab)
                  (let ((err (lambda (desc obj)
                               (format (current-error-port)
                                       "# field error (rm), ~a: ~a~%"
                                       desc obj))))
                    (every (lambda (x)
                             (match x
                               ((a . r)
                                (let ((address-ok? (index? a))
                                      (register-ok? (register? r)))
                                  (unless address-ok?  (err 'address x))
                                  (unless register-ok? (err 'register x))
                                  (and address-ok? register-ok?)))
                               (_ (err "key-value" x)
                                  #f)))
                           tab)))))))

(define-syntax-rule (rm→ e* ...) (register-map e* ...))

(new-record-definer define-register-map register-map)

(set-record-type-printer! <register-map>
  (lambda (rm port)
    (simple-format port "#<register-map name: ~a registers: ~a>"
                   (register-map-name rm)
                   (length (register-map-table rm)))))

(define (register-map-table:sorted rm)
  (sort (register-map-table rm)
        (lambda (a b) (index< (car a) (car b)))))

(define (register-map-default rm)
  (map (lambda (x) (cons (car x) (register-default (cdr x))))
       (register-map-table rm)))

(define (register-map-item-names rm)
  (flatten (map register-item-names (register-map-registers rm))))

(define (register-map-registers rm)
  (map cdr (register-map-table rm)))

(define (register-map-register rm)
  (let ((rm* (register-map-table rm)))
    (if (and (= (length rm*) 1)
             (eq? (caar rm*) #f))
        (cdar rm*)
        (throw 'more-than-single-register rm))))

(define (register-map-fold fnc init rm)
  (fold (lambda (reg acc)
          (fnc (car reg) (cdr reg) acc))
        init
        (register-map-table rm)))

(define (register-map-fold-right fnc init rm)
  (fold-right (lambda (reg acc)
                (fnc (car reg) (cdr reg) acc))
              init
              (register-map-table rm)))

(define (register-map-ref rm . args)
  (match args
    (((? index? address))
     (let ((entry (assv address (register-map-table rm))))
       (if entry
           (cdr entry)
           (throw 'unknown-register-address address))))
    (((? index? address) (? non-negative-integer? idx))
     (apply register-ref (list (register-map-ref rm address) idx)))
    (_ (throw 'invalid-register-map-reference args))))

(define* (register-map-find-item rm name idx #:optional (default (const #f)))
  (list-iterate (lambda (x a n k)
                  (let* ((addr (car x))
                         (reg (cdr x))
                         (m (+ a (register-named-items-count reg name))))
                    (if (< idx m)
                        (k (cons addr (register-canonical reg name (- idx a))))
                        m)))
                0 default
                (register-map-table:sorted rm)))

(define (register-map-canonical rm . item-address)
  (match item-address
    (((? symbol? name))
     (register-map-find-item rm name 0))
    (((? symbol? name) (? non-negative-integer? n))
     (register-map-find-item rm name n))
    (((? index? ra) . rest)
     (cons ra (apply register-canonical (cons (register-map-ref rm ra) rest))))
    (_ (throw 'invalid-item-address item-address))))

(define (register-map-address rm . item-address)
  (let ((ca (apply register-map-canonical (cons rm item-address))))
    (and ca (apply register-map-ref (cons rm ca)))))

(define (register-map-address:register rm . item-address)
  (let ((ca (apply register-map-canonical (cons rm item-address))))
    (and ca (apply register-map-ref (cons rm (drop-right ca 1))))))
