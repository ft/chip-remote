(define-module (chip-remote device)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 pretty-print)
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
            device-address
            device-item-names
            device-default
            define-device))

(define-record-type <device>
  (make-device meta page-map)
  device?
  (meta device-meta)
  (page-map device-page-map))

(define group:page
  (group 'pages
         #:type 'list
         #:predicate
         (lambda (x)
           (memq x '(#:page-map
                     #:page-map*
                     #:register-map
                     #:register)))
         #:transformer
         (lambda (e)
           (syntax-case e ()
             ((#:page-map* exp)
              #'exp)
             ((#:page-map (e0 e* ...) ...)
              #'(list (generate-page-map (e0 e* ...)) ...))
             ((#:register-map (e0 e* ...))
              #'(list (generate-page-map (#f (generate-register-map e0 e* ...)))))
             ((#:register (e0 e* ...))
              #'(list (generate-page-map (#f (generate-register-map
                                              #:table (#f (e0 e* ...)))))))))))

(define-syntax generate-device
  (lambda (x)
    (syntax-case x ()
      ((_ pl ...)
       (with-syntax ((((mp ...) ((key value) ...)) (process-plist #'(pl ...)
                                                                  group:page
                                                                  (group 'meta))))
         #'(make-device (list (cons key value) ...)
                        (page-map-merge (list mp ...))))))))

(define-syntax-rule (define-device binding e0 e* ...)
  (define binding (generate-device #:name 'binding e0 e* ...)))

(define (record-device-printer device port)
  (format port "<device:~%    #:meta~%")
  (pretty-print (device-meta device) port #:per-line-prefix "    ")
  (format port "    #:page-map~%")
  (pretty-print (device-page-map device) port #:per-line-prefix "    ")
  (format port ">"))

(set-record-type-printer! <device> record-device-printer)

(define (device-default dev)
  (page-map-default (device-page-map dev)))

(define (device-item-names dev)
  (page-map-item-names (device-page-map dev)))

(define (device-register dev)
  (let ((pm (page-map-table (device-page-map dev))))
    ;; Only one page-map entry and no address on the page means there is no
    ;; real page-map in the chip, so it just has a single register-map.
    (if (and (= (length pm) 1)
             (eq? (caar pm) #f))
        (register-map-register (cdar pm))
        (throw 'more-than-single-register-map dev))))

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
