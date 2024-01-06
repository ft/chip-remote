;; -*- scheme -*-

;; Copyright (c) 2024 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 binary-ports)
             (ice-9 format)
             (ice-9 pretty-print)
             (test tap)
             (test setup)
             (protocol ufw-regp))

(init-test-tap!)

(with-fs-test-bundle
  (plan 13)

  (define-test "12 octet frame of zeros is a valid frame"
    (pass-if-equal? (regp:decode #vu8(#x00
                                      #x00
                                      #x00 #x00
                                      #x00 #x00 #x00 #x00
                                      #x00 #x00 #x00 #x00))
                    '((version . 0)
                      (type . read-request)
                      (options)
                      (meta . valid)
                      (sequence-number . 0)
                      (address . 0)
                      (block-size . 0)
                      (header-crc . #f)
                      (payload-crc . #f)
                      (payload . #f))))

  (define-test "Large block-size with no payload bytes is an error"
    (pass-if-true (member 'implausible-block-size
                          (assq-ref (regp:decode #vu8(#x03
                                                      #x20
                                                      #x52 #x24
                                                      #x6c #xb9 #x24 #x4a
                                                      #x29 #xda #xe9 #xe1
                                                      #xad #xb1))
                                    'errors))))

  (define-test "read-request round-trip test (encode → decode) works"
    (pass-if-equal? (regp:decode (regp:read-request
                                  #x1234 #x10
                                  #:header-crc? #t
                                  #:sequence-number #x321))
                    '((version . 0)
                      (type . read-request)
                      (options with-header-crc)
                      (meta . valid)
                      (sequence-number . 801)
                      (address . 4660)
                      (block-size . 16)
                      (header-crc (transmitted . 58158)
                                  (calculated . 58158))
                      (payload-crc . #f)
                      (payload . #f))))

  (define-test "write-request round-trip test (encode → decode) works"
    (pass-if-equal? (regp:decode (regp:write-request
                                  #x1234 #vu8(#x12 #x34 #x56 #x78)
                                  #:word-size-16? #t
                                  #:header-crc? #t
                                  #:payload-crc? #t
                                  #:sequence-number #x321))
                    '((version . 0)
                      (type . write-request)
                      (options word-size-16
                               with-header-crc
                               with-payload-crc)
                      (meta . valid)
                      (sequence-number . #x321)
                      (address . #x1234)
                      (block-size . 2)
                      (header-crc  (transmitted . 43728)
                                   (calculated  . 43728))
                      (payload-crc (transmitted . 13435)
                                   (calculated  . 13435))
                      (payload . #vu8(#x12 #x34 #x56 #x78)))))

  ;; These are example frames from ufw's C test suite:
  (define-test "read-request addr 100 size 1 (16-bit) works"
    (pass-if-equal? (regp:decode
                     #vu8(#x01
                          #x00
                          #x00 #x00
                          #x00 #x00 #x00 #x64
                          #x00 #x00 #x00 #x01))
                    '((version . 0)
                      (type . read-request)
                      (options word-size-16)
                      (meta . valid)
                      (sequence-number . 0)
                      (address . 100)
                      (block-size . 1)
                      (header-crc . #f)
                      (payload-crc . #f)
                      (payload . #f))))

  (define-test "read-response addr 100 size 1 (16-bit) works"
    (pass-if-equal? (regp:decode
                     #vu8(#x01
                          #x10
                          #x00 #x00
                          #x00 #x00 #x00 #x64
                          #x00 #x00 #x00 #x01
                          #x64 #x00))
                    '((version . 0)
                      (type . read-response)
                      (options word-size-16)
                      (meta . acknowledge)
                      (sequence-number . 0)
                      (address . 100)
                      (block-size . 1)
                      (header-crc . #f)
                      (payload-crc . #f)
                      (payload . #vu8(100 0)))))

  (define-test "write-request addr 100 size 1 (16-bit) works"
    (pass-if-equal? (regp:decode
                     #vu8(#x01
                          #x20
                          #x00 #x01
                          #x00 #x00 #x00 #x64
                          #x00 #x00 #x00 #x01
                          #x00 #x01))
                    '((version . 0)
                      (type . write-request)
                      (options word-size-16)
                      (meta . valid)
                      (sequence-number . 1)
                      (address . 100)
                      (block-size . 1)
                      (header-crc . #f)
                      (payload-crc . #f)
                      (payload . #vu8(0 1)))))

  (define-test "read-request addr 100 size 1 (16-bit,crc) works"
    (pass-if-equal? (regp:decode
                     #vu8(#x03
                          #x00
                          #x00 #x00
                          #x00 #x00 #x00 #x64
                          #x00 #x00 #x00 #x01
                          #x0c #xb4))
                    '((version . 0)
                      (type . read-request)
                      (options word-size-16 with-header-crc)
                      (meta . valid)
                      (sequence-number . 0)
                      (address . 100)
                      (block-size . 1)
                      (header-crc (transmitted . 3252)
                                  (calculated . 3252))
                      (payload-crc . #f)
                      (payload . #f))))

  (define-test "read-response addr 100 size 1 (16-bit,crc) works"
    (pass-if-equal? (regp:decode
                     #vu8(#x07
                          #x10
                          #x00 #x00
                          #x00 #x00 #x00 #x64
                          #x00 #x00 #x00 #x01
                          #x8e #x9d
                          #xc0 #x2a
                          #x64 #x00))
                    '((version . 0)
                      (type . read-response)
                      (options
                       word-size-16
                       with-header-crc
                       with-payload-crc)
                      (meta . acknowledge)
                      (sequence-number . 0)
                      (address . 100)
                      (block-size . 1)
                      (header-crc (transmitted . 36509)
                                  (calculated . 36509))
                      (payload-crc (transmitted . 49194)
                                   (calculated . 49194))
                      (payload . #vu8(100 0)))))

  (let* ((frame (regp:read-request #x100 #x10))
         (wire (call-with-output-bytevector
                (lambda (port)
                  (regp:send (regp:tcp-connection port) frame)))))
    (define-test "Producing length prefixing works"
      (pass-if-equal? wire
                      #vu8(12 0 0 0 0 0 0 1 0 0 0 0 #x10)))
    (define-test "Consuming length prefixing works"
      (pass-if-equal? (regp:decode frame)
                      (call-with-input-bytevector
                       wire (lambda (port)
                              (regp:recv (regp:tcp-connection port)))))))

  (let* ((frame (regp:read-request #xc0 #x10 #:header-crc? #t))
         (wire (call-with-output-bytevector
                (lambda (port)
                  (regp:send (regp:serial-connection port) frame)))))
    (define-test "Producing SLIP framing works"
      (pass-if-equal? wire
                      #vu8(#x2
                           #x0
                           #x0 #x0
                           #x0 #x0 #x0 #xdb #xdc ; db,dc → c0
                           #x0 #x0 #x0 #x10
                           #x1a #xf8
                           #xc0)))
    (define-test "Consuming SLIP framing works"
      (pass-if-equal? (regp:decode frame)
                      (call-with-input-bytevector
                       wire (lambda (port)
                              (regp:recv (regp:serial-connection port))))))))
