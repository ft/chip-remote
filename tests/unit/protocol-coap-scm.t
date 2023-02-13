;; -*- scheme -*-

(use-modules (test tap)
             (test setup)
             (srfi srfi-1)
             (protocol coap)
             (protocol coap message)
             (protocol coap option))

(init-test-tap!)

(with-fs-test-bundle
    (plan 17)

  (define-test "coap-code: method empty"
    (pass-if-equal? (resolve-code 'method 'empty)
                    '(0 . 0)))
  (define-test "coap-code: signal pong"
    (pass-if-equal? (resolve-code 'signal 'pong)
                    '(7 . 3)))
  (define-test "coap-code: foobar pong (invalid class)"
    (pass-if-exception 'coap:encode:invalid-message-class
                       (resolve-code 'foobar 'pong)))
  (define-test "coap-code: method pong (invalid detail)"
    (pass-if-exception 'coap:encode:invalid-message-class-detail
                       (resolve-code 'method 'pong)))
  (define-test "encode: encode without argument generates an empty method"
    (pass-if-equal? (encode)
                    #vu8(#x50 #x00 #x00 #x00)))

  (define-test "option: if-none-match"
    (pass-if-equal? (cdr (encodable-option 0 'if-none-match))
                    '(0 5 0 0 #f)))

  (define-test "option: uri-path /foo/bar"
    (pass-if-equal? (cdr (encodable-option 0 '(uri-path "/foo/bar")))
                    '(0 11 0 8 #vu8(47 102 111 111 47 98 97 114))))

  (let ((opts (encodable-options '((uri-host "localhost")
                                   (uri-path "/foo/bar/baz/quux")
                                   (uri-port 9910)))))
    (define-test "option: (uri-host …) (uri-path …) (uri-port …)"
      (pass-if-equal? opts
                      ;; Sorting has to work here too.
                      '((0 3 0 9 #vu8(108 111 99 97 108 104 111 115 116))
                        (0 4 0 2 9910)
                        (0 4 1 4 #vu8(47 102 111 111 47 98 97 114 47 98
                                         97 122 47 113 117 117 120)))))
    (define-test "option-size: (uri-host …) → 10"
      (pass-if-= 10 (option-size (first opts))))
    (define-test "option-size: (uri-port …) → 3"
      (pass-if-= 3 (option-size (second opts))))
    (define-test "option-size: (uri-path …) → 19"
      (pass-if-= 19 (option-size (third opts))))
    (define-test "option: sizeof (uri-host …) (uri-path …) (uri-port …)"
      (pass-if-= (+ 10 3 19) (options-size opts))))

  (define-test "encode a larger message"
    (pass-if-equal?
     (encode #:payload #vu8(1 2 3 4)
             #:token #x12345678
             #:message-id #xabcd
             #:type 'confirmable
             #:code 'put ;; equivalent: '(method put)
             #:options '((uri-host "localhost")
                         (uri-path "/foo/bar/baz/quux")
                         (uri-port 9910)))
     #vu8(#x14                ;; ver: 1, type: 0, toklen: 4
          #x03                ;; class: 0, detail: 3 (method:put)
          #xAB #xCD           ;; message-id
          ;; Mandatory ↑ ↓ Optional
          #x12 #x34 #x56 #x78 ;; token
          #x39                ;; opt-delta: 3, opt-value-len: 9
          ;; "localhost"
          #x6C #x6F #x63 #x61 #x6C #x68 #x6F #x73 #x74
          #x42      ;; opt-delta: 4, opt-value-len: 2
          #x26 #xB6 ;; uint: 9910 (big-endian)
          #x4D      ;; opt-delta: 4, opt-value-len: 1-octet-extend
          #x4       ;; opt-len: 13+4 = 17
          ;; "/foo/bar/baz/quux"
          #x2F #x66 #x6F #x6F
          #x2F #x62 #x61 #x72
          #x2F #x62 #x61 #x7A
          #x2F #x71 #x75 #x75 #x78
          #xFF       ;; There's payload so here's the marker.
          1 2 3 4))) ;; Finally the payload

  (define-test "coap-decode: method empty"
    (let ((spec '((version . 1)
                  (type . non-confirmable)
                  (code method empty)
                  (message-id . 0)
                  (token)
                  (options)
                  (payload))))
      (pass-if-equal? spec (decode (encode* spec)))))

  (define-test "coap-decode: standard option"
    (let ((spec '((version . 1)
                  (type . non-confirmable)
                  (code method empty)
                  (message-id . 0)
                  (token)
                  (options (uri-host "localhost"))
                  (payload))))
      (pass-if-equal? spec (decode (encode* spec)))))

  (define-test "coap-decode: more than one standard option"
    (let ((spec '((version . 1)
                  (type . non-confirmable)
                  (code method empty)
                  (message-id . 0)
                  (token)
                  (options (uri-host "localhost")
                           (uri-path "/foo/bar/baz/quux"))
                  (payload))))
      (pass-if-equal? spec (decode (encode* spec)))))

  (define-test "coap-decode: more than one standard option plus payload"
    (let ((spec '((version . 1)
                  (type . non-confirmable)
                  (code method empty)
                  (message-id . 0)
                  (token)
                  (options (uri-host "localhost")
                           (uri-path "/foo/bar/baz/quux"))
                  (payload . #vu8(1 2 3 4 5 6 7 8 9 0)))))
      (pass-if-equal? spec (decode (encode* spec))))))
