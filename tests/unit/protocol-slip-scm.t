;; -*- scheme -*-

;; Copyright (c) 2019 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (rnrs bytevectors)
             (protocol slip))

(init-test-tap!)

(force-import (protocol slip)
              *slip-default-encoding*
              esc eof sof
              esc-esc esc-eof esc-sof)

(define extended-slip (make-slip-encoding #:escape         '(#xda . #xdd)
                                          #:start-of-frame '(#xb0 . #xdb)
                                          #:end-of-frame   '(#xc0 . #xdc)
                                          #:with-sof? #t))

(with-fs-test-bundle
    (plan 6)
  (let* ((encoder (make-slip-state))
         (decoder (make-slip-state))
         (data '(1 2 3 4 5 6 7 8 9 0))
         (trivial (u8-list->bytevector data))
         (end-of-frame (car (assq-ref *slip-default-encoding* 'end-of-frame)))
         (encoded (u8-list->bytevector (append data (list end-of-frame)))))
    (define-test "slip,default: Trivial octet-stream only gets EOF"
      (pass-if-equal? (slip-encode encoder trivial) encoded))
    (define-test "slip,default: Decoding result returns the same trivial vector"
      (pass-if-equal? (slip-decode! decoder encoded) (list trivial))))

  (let* ((encoder (make-slip-state #:encoding extended-slip))
         (decoder (make-slip-state #:encoding extended-slip))
         (data '(1 2 3 4 5 6 7 8 9 0))
         (trivial (u8-list->bytevector data))
         (escape (esc extended-slip))
         (esc-escape (esc-esc extended-slip))
         (start-of-frame (sof extended-slip))
         (esc-start-of-frame (esc-sof extended-slip))
         (end-of-frame (eof extended-slip) )
         (esc-end-of-frame (esc-eof extended-slip) )
         (encoded (u8-list->bytevector (append (list start-of-frame)
                                               data
                                               (list end-of-frame))))
         (complex (u8-list->bytevector (list start-of-frame
                                             1 2 3
                                             end-of-frame
                                             4 5 6
                                             escape
                                             7 8 9
                                             esc-escape
                                             9 8 7
                                             esc-start-of-frame
                                             6 5 4
                                             esc-end-of-frame
                                             3 2 1)))
         (encoded* (u8-list->bytevector (list start-of-frame
                                              escape esc-start-of-frame
                                              1 2 3
                                              escape esc-end-of-frame
                                              4 5 6
                                              escape esc-escape
                                              7 8 9
                                              esc-escape
                                              9 8 7
                                              esc-start-of-frame
                                              6 5 4
                                              esc-end-of-frame
                                              3 2 1
                                              end-of-frame))))
    (define-test "slip,extended: Trivial octet-stream only gets SOF+EOF"
      (pass-if-equal? (slip-encode encoder trivial) encoded))
    (define-test "slip,extended: Decoding result returns the same trivial vector"
      (pass-if-equal? (slip-decode! decoder encoded) (list trivial)))
    (define-test "slip-extended: Complex octet-stream encodes correctly"
      (pass-if-equal? (slip-encode encoder complex) encoded*))
    (define-test "slip,extended: Decoding result returns the same complex vector"
      (pass-if-equal? (slip-decode! decoder encoded*) (list complex)))))
