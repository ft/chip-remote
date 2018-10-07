;; Copyright (c) 2011-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; Low-level IO library for `chip-remote'

(define-module (chip-remote io)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:export (cr-connection?
            cr-connection-uri
            cr-connection-port
            get-cr-capability
            set-cr-capability!
            cr-path
            io-close
            io-open
            io-opt/get
            io-opt/set
            io-read
            io-write
            make-cr-connection))

(define-record-type <cr-connection>
  (make-bare-cr-connection)
  cr-connection?
  (uri cr-connection-uri set-cr-connection-uri!)
  (port cr-connection-port set-cr-connection-port!)
  (capabilities cr-capabilities set-cr-capabilities!))

(define (get-cr-capability connection key)
  (assq-ref (cr-capabilities connection) key))

(define (set-cr-capability! connection key value)
  (set-cr-capabilities! connection (assq-set! (cr-capabilities connection)
                                              key
                                              value)))

(define (uri-defaults-to-file string-uri)
  (or (string->uri string-uri)
      (string->uri (string-concatenate (list "file://" string-uri)))))

(define (make-cr-connection string-uri)
  (let* ((uri (uri-defaults-to-file string-uri))
         (scheme (uri-scheme uri)))
    (unless (eq? scheme 'file)
      (throw 'unsupported-uri-scheme uri))
    (let ((new (make-bare-cr-connection)))
      (set-cr-connection-uri! new uri)
      new)))

(define (cr-path connection)
  (uri-path (cr-connection-uri connection)))

(define io-options (list (cons 'serial-timeout 2)
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

(define (io-read connection)
  (let ((result (select (list (cr-connection-port connection))
                  '() '() (io-opt/get 'serial-timeout))))
    (if (null? (car result))
        (throw 'read-timeout connection)
        (let ((string (read-line (cr-connection-port connection) 'trim)))
          (unless (string? string)
            (throw 'io-read-error))
          (when (io-opt/get 'trace)
            (display (string-concatenate (list " >>> " string)))
            (newline))
          string))))

(define (io-write connection string)
  (when (io-opt/get 'trace)
    (display (string-concatenate (list " <<< " string)))
    (newline))
  (let ((p (cr-connection-port connection)))
    (display string p)
    (newline p)))

(define (io-open connection)
  (set-cr-connection-port! connection
                           (open-file (cr-path connection) "r+l")))

(define (io-close connection)
  (close-port (cr-connection-port connection)))
