;; Copyright (c) 2011-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; Low-level IO library for `chip-remote'

(define-module (chip-remote io)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:use-module (protocol length-prefix)
  #:use-module (protocol slip)
  #:export (cr-connection?
            cr-connection-frame-meta
            cr-connection-frame-method
            cr-connection-info
            cr-connection-port
            cr-connection-uri
            cr-path
            io-close
            io-open
            io-opt/get
            io-opt/set
            io-read
            io-write
            make-cr-connection))

(define (protocol-tcp) (protoent:proto (getprotobyname "tcp")))
(define (default-cr-port) 12345)

(define-record-type <ip-connection>
  (make-ip-connection host address-string address port family)
  ip-connection?
  (family ipconn/get-family)
  (host ipconn/get-host)
  (address-string ipconn/get-address-string)
  (address ipconn/get-address)
  (port ipconn/get-port))

(define-record-type <unix-socket-connection>
  (make-unix-socket-connection file-name)
  unix-socket-connection?
  (file-name unix/get-file-name))

(define-record-type <serial-connection>
  (make-serial-connection file-name)
  serial-connection?
  (file-name serial/get-file-name))

(define-record-type <cr-connection>
  (make-bare-cr-connection)
  cr-connection?
  (uri cr-connection-uri set-cr-connection-uri!)
  (frame-method cr-connection-frame-method set-cr-connection-frame-method!)
  (frame-meta cr-connection-frame-meta set-cr-connection-frame-meta!)
  (info cr-connection-info set-cr-connection-info!)
  (port cr-connection-port set-cr-connection-port!))

(define (cr-make-slip-state)
  (make-slip-state #:encoding (make-slip-encoding #:with-sof? #t)))

(define* (make-cr-connection string-uri #:key frame-method frame-meta)
  (define (uri-defaults-to-serial string-uri)
    (or (string->uri string-uri)
        (string->uri (string-concatenate (list "serial://" string-uri)))))

  (when frame-method
    (unless (member frame-method '(length-prefix slip))
      (throw 'cr/invalid-frame-method frame-method)))
  (when frame-meta
    (unless (eq? frame-method 'slip)
      (throw 'cr/invalid-frame-meta frame-method frame-meta)))

  (let* ((uri (uri-defaults-to-serial string-uri))
         (scheme (uri-scheme uri))
         (framing (or frame-method (case scheme
                                     ((unix tcp) 'length-prefix)
                                     ((serial)   'slip))))
         (meta (or frame-meta (if (eq? framing 'slip)
                                  (cr-make-slip-state)))))
    (unless (member scheme '(tcp unix serial))
      (throw 'cr/unsupported-uri-scheme uri))
    (let ((new (make-bare-cr-connection)))
      (set-cr-connection-uri! new uri)
      (set-cr-connection-frame-method! new framing)
      (set-cr-connection-frame-meta! new meta)
      (set-cr-connection-info!
       new
       (case scheme
         ((serial) (make-serial-connection      (uri-path uri)))
         ((unix)   (make-unix-socket-connection (uri-path uri)))
         ((tcp)    (let* ((host (uri-host uri))
                          (port (or (uri-port uri)
                                    (default-cr-port)))
                          (ai (car (catch #t
                                     (lambda () (getaddrinfo host))
                                     (lambda (key . args)
                                       (throw 'cr/getaddrinfo-failed
                                              uri key args
                                              (gai-strerror (car args)))))))
                          (family (addrinfo:fam ai))
                          (addr (sockaddr:addr (addrinfo:addr ai)))
                          (addr-string (catch #t
                                         (lambda () (inet-ntop family addr))
                                         (lambda (key . args)
                                           (throw 'cr/inet-ntop-failed
                                                  uri key args)))))
                     (make-ip-connection host addr-string addr port family)))))
      new)))

(define (cr-path connection)
  (uri-path (cr-connection-uri connection)))

(define io-options (list (cons 'timeout 2)
                         (cons 'trace #f)))

(define (io-opt/get key)
  (let ((value (assq key io-options)))
    (if value
        (cdr value)
        (throw 'unknown-option key))))

(define (io-opt/set key value)
  (let ((current-value (assq key io-options)))
    (if current-value
        (assq-set! io-options key value)
        (throw 'unknown-option `(,key ,value)))))

(define (io-read c)
  (case (cr-connection-frame-method c)
    ((length-prefix) (recv-bytevector (cr-connection-port c)))
    ((slip)          (slip-recv (cr-connection-frame-meta c)
                                (cr-connection-port c)))))
(define (io-write c data)
  (case (cr-connection-frame-method c)
    ((length-prefix) (send-bytevector (cr-connection-port c) data))
    ((slip)          (slip-send (cr-connection-frame-meta c)
                                (cr-connection-port c)
                                data))))

(define (io-type-unix? c)
  (unix-socket-connection? (cr-connection-info c)))

(define (io-type-tcp? c)
  (ip-connection? (cr-connection-info c)))

(define (io-type-serial? c)
  (serial-connection? (cr-connection-info c)))

(define (io-connected? c)
  (not (not (cr-connection-port c))))

(define (make-unix-socket conn)
  (socket AF_UNIX SOCK_STREAM 0))

(define (make-tcp-socket conn)
  (socket (ipconn/get-family (cr-connection-info conn))
          SOCK_STREAM (protocol-tcp)))

(define (disect-connect-error args)
  (match args
    ((_ _ (str) (num)) (list num str))
    (_ args)))

(define (connect-unix conn)
  (catch #t
    (lambda ()
      (connect (cr-connection-port conn) AF_UNIX
               (unix/get-file-name (cr-connection-info conn))))
    (lambda (key . args)
      (throw 'cr/connect-failed key (disect-connect-error args)))))

(define (connect-tcp conn)
  (let ((be (cr-connection-info conn)))
    (catch #t
      (lambda ()
        (connect (cr-connection-port conn)
                 (ipconn/get-family be)
                 (ipconn/get-address be)
                 (ipconn/get-port be)))
      (lambda (key . args)
        (throw 'cr/connect-failed key (disect-connect-error args))))))

(define (io-open c)
  (when (io-connected? c)
    (io-close c))
  (set-cr-connection-port!
   c (cond ((io-type-serial? c) (open-file (cr-path c) "r+l"))
           ((io-type-unix? c)   (make-unix-socket c))
           ((io-type-tcp? c)    (make-tcp-socket c))))
  (cond ((io-type-unix? c) (connect-unix c))
        ((io-type-tcp? c)  (connect-tcp c)))
  c)

(define (io-close c)
  (cond ((io-type-serial? c) (close-port (cr-connection-port c)))
        ((io-type-unix? c)   (shutdown (cr-connection-port c) 2))
        ((io-type-tcp? c)    (shutdown (cr-connection-port c) 2))
        (else (throw 'cr/unknown-connection-type c)))
  (set-cr-connection-port! c #f)
  c)
