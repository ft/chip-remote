(define-module (chip-remote device)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 pretty-print)
  #:use-module (chip-remote language)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote page-map)
  #:export (generate-device
            make-device
            device?
            device-meta
            device-page-map
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
       (with-syntax ((((mp ...) (meta ...)) (process-plist #'(pl ...)
                                                           group:page
                                                           (group 'meta))))
         #'(make-device (list meta ...) (page-map-merge (list mp ...))))))))

(define-syntax-rule (define-device binding e0 e* ...)
  (define binding (generate-device e0 e* ...)))

(define (record-device-printer device port)
  (format port "<device:~%    #:meta~%")
  (pretty-print (device-meta device) port #:per-line-prefix "    ")
  (format port "    #:page-map~%")
  (pretty-print (device-page-maps device) port #:per-line-prefix "    ")
  (format port ">"))

(set-record-type-printer! <device> record-device-printer)
