;; -*- scheme -*-

;; Copyright (c) 2022 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (web uri)
             (chip-remote io))

(define (→ fmt . rest)
  (apply format (cons* #f fmt rest)))

(init-test-tap!)

(with-fs-test-bundle
  (plan (+ 1 (* 2 3) 1))
  (define-test "Default connection scheme is 'serial'"
    (pass-if-eq? (uri-scheme (cr-connection-uri
                              (make-cr-connection "/dev/ttyUSB0")))
                 'serial))
  (for-each
   (lambda (s)
     (let* ((c (make-cr-connection (cdr s)))
            (scheme (uri-scheme (cr-connection-uri c)))
            (expect-frame (if (eq? scheme 'serial) 'slip 'length-prefix)))
       (format #t "# DEBUG: ~s~%" c)
       (define-test (→ "Connection scheme ~a is allowed" (car s))
         (pass-if-eq? scheme (car s)))
       (define-test (→ "Connection scheme ~a, framing: ~a"
                       scheme expect-frame)
         (pass-if-eq? (cr-connection-frame-method c)
                      expect-frame))))
   '((serial . "serial:///dev/ttyUSB0")
     (unix   . "unix:///var/run/chip-remote.socket")
     (tcp    . "tcp://192.168.22.123")))

  (define-test "Unknown URI scheme throws cr/unsupported-uri-scheme"
    (pass-if-exception 'cr/unsupported-uri-scheme
                       (make-cr-connection "foobar://quux"))))
