;; Copyright (c) 2011-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote protocol)
  #:use-module (chip-remote io)
  #:use-module (srfi srfi-1)
  #:export (chip-remote-open!))

(define* (chip-remote-open! #:key uri connection)
  (when (and uri connection)
    (throw 'invalid-arguments 'uri-and-connection-provided))
  (unless (or uri connection)
    (throw 'invalid-arguments 'neither-uri-nor-connection-provided))
  (let ((conn (if uri
                  (make-cr-connection uri)
                  connection)))
    (io-open conn)
    conn))
