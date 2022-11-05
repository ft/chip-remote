;; -*- scheme -*-

;; Copyright (c) 2022 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 binary-ports)
             (rnrs bytevectors)
             (srfi srfi-1)
             (test tap)
             (test setup)
             (data-structures variable-width-integer)
             (protocol length-prefix))

(init-test-tap!)

(define (→ fmt . rest)
  (apply format (cons* #f fmt rest)))

(define (bv-slice bv start len)
  ;;(format #t "# DEBUG: ~a ~a ~a~%" start len bv)
  (catch #t
    (lambda ()
      (list->u8vector (take (catch #t
                              (lambda () (drop (u8vector->list bv) start))
                              (lambda (k . a) (throw 'could-not-drop
                                                     (bytevector-length bv)
                                                     start)))
                            len)))
    (lambda (k . a) (throw 'could-not-take
                           (bytevector-length bv)
                           start len))))

(define (check-codec payload)
  ;; Encode payload into a frame, check length, prefix and payload parts. Then
  ;; decode and check if decoded payload matches input.
  (let* ((pl-n (bytevector-length payload))
         (pfx (varint-encode pl-n))
         (pfx-n (bytevector-length pfx))
         (bv (call-with-output-bytevector
              (lambda (port)
                (send-bytevector port payload))))
         (bv-n (bytevector-length bv))
         (actual-prefix (bv-slice bv 0 pfx-n))
         (actual-payload (bv-slice bv pfx-n pl-n)))
    (define-test (→ "Encoding bytevector of length ~a works: length: ~a"
                    pl-n bv-n)
      (pass-if-= (+ pl-n pfx-n) bv-n))
    (define-test (→ "Encoding bytevector of length ~a works: prefix" pl-n)
      (pass-if-equal? actual-prefix pfx))
    (define-test (→ "Encoding bytevector of length ~a works: payload" pl-n)
      (pass-if-equal? actual-payload payload))

    (let* ((decoded (call-with-input-bytevector
                     bv (lambda (port)
                          (recv-bytevector port))))
           (decoded-n (bytevector-length decoded)))
      (define-test (→ "Decoding bytevector of length ~a works: length: ~a"
                      bv-n decoded-n)
        (pass-if-= pl-n decoded-n))
      (define-test (→ "Decoding bytevector of length ~a works: payload"
                      bv-n)
        (pass-if-equal? payload decoded)))))

(define *cases-per-test* 5)

(define *tests*
  ;; Zero length frame (one octet for length), one, two and three byte pre-
  ;; fixes. Beyond that, things get a little memory hungry, so never mind with
  ;; that.
  (list #vu8()
        #vu8(#x11)
        (make-bytevector 1024 #xce)
        (make-bytevector (* 1024 1024) #xbe)))

(with-fs-test-bundle
  (plan (* *cases-per-test*
           (length *tests*)))
  (for-each check-codec *tests*))
