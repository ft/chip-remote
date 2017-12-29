(define-module (chip-remote register-map)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 control)
  #:use-module (ice-9 pretty-print)
  #:use-module (chip-remote process-plist)
  #:use-module (chip-remote item)
  #:use-module (chip-remote register)
  #:use-module (chip-remote utilities)
  #:export (generate-register-map
            make-register-map
            register-map?
            register-map-meta
            register-map-table
            register-map-defaults
            register-map-item-names
            register-map-register
            register-map-fold
            register-map-ref
            define-register-map))

(define-record-type <register-map>
  (make-register-map meta table)
  register-map?
  (meta register-map-meta)
  (table register-map-table))

(define group:table
  (group 'table
         #:type 'list
         #:predicate (lambda (x)
                       (memq x '(#:table #:table*)))
         #:transformer (lambda (e)
                         (syntax-case e ()
                           ((#:table (addr (exps ...)) ...)
                            #'((cons addr (generate-register exps ...)) ...))
                           ((#:table* (addr exp) ...)
                            #'((cons addr exp) ...))))))

(define-syntax generate-register-map
  (lambda (x)
    (syntax-case x ()
      ((kw exp0 expn ...)
       (is-kw? #'exp0)
       (with-syntax (((((table ...) ...) (meta ...))
                      (process-plist #'(exp0 expn ...)
                                     group:table
                                     (group 'meta))))
         #'(make-register-map (list meta ...)
                              (list table ... ...)))))))

(define (register-map-defaults rm)
  (map register-defaults (register-map-table rm)))

(define (register-map-item-names rm)
  (flatten (map register-item-names (register-map-table rm))))

(define-syntax-rule (define-register-map binding e0 e* ...)
  (define binding (generate-register-map e0 e* ...)))

(define (record-register-map-printer register-map port)
  (format port "<register-map:~%    #:meta~%")
  (pretty-print (register-map-meta register-map) port #:per-line-prefix "    ")
  (format port "    #:table~%")
  (pretty-print (register-map-table register-map) port #:per-line-prefix "    ")
  (format port ">"))

(set-record-type-printer! <register-map> record-register-map-printer)

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

(define (register-map-ref rm name)
  (call/ec (lambda (return)
             (register-map-fold (lambda (ra reg acc)
                                  (let ((item (register-ref reg name)))
                                    (if (item? item)
                                        (return item)
                                        #f)))
                                #f rm))))
