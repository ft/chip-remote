;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote semantics)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-9)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote interpreter)
  #:export (make-semantics
            semantics?
            semantics-type
            semantics-decode
            semantics-encode))

(define-record-type <semantics>
  (make-semantics* type decode encode)
  semantics?
  (type semantics-type)
  (decode semantics-decode)
  (encode semantics-encode))

(define* (make-semantics type
                         #:key
                         (decode identity)
                         (encode identity)
                         (table '()))
  (case type
    ((boolean) (make-semantics* type decode-boolean encode-boolean))
    ((boolean/active-low) (make-semantics* type
                                           decode-boolean/active-low
                                           encode-boolean/active-low))
    ((state) (make-semantics* type decode-state encode-state))
    ((state/active-low) (make-semantics* type
                                         decode-state/active-low
                                         encode-state/active-low))
    ((unsigned-integer) (make-semantics* type identity identity))
    ((offset-binary) (make-semantics* type
                                      decode-offset-binary
                                      encode-offset-binary))
    ((ones-complement) (make-semantics* type
                                        decode-ones-complement
                                        encode-ones-complement))
    ((twos-complement) (make-semantics* type
                                        decode-twos-complement
                                        encode-twos-complement))
    ((sign-magnitude) (make-semantics* type
                                       decode-sign-magnitude
                                       encode-sign-magnitude))
    ((table-lookup) (make-semantics* type
                                     (make-table-decoder table)
                                     (make-table-encoder table)))
    ((interpreter) (make-semantics* type
                                    (make-evaluation decode)
                                    (make-evaluation encode)))
    ((scheme) (make-semantics* type decode encode))
    (else (throw 'unknown-semantics type decode encode))))
