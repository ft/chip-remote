;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test validate registers-have-content)
  #:use-module (test tap)
  #:use-module (chip-remote device)
  #:use-module (chip-remote register)
  #:export (non-empty-registers/check
            non-empty-registers/count))

(define (non-empty-registers/check dev cfg)
  (let loop ((rest (device-registers dev)) (n 0))
    (unless (null? rest)
      (let* ((reg (car rest)) (len (length (register-items reg))))
        (define-test
          (format #f "Device register ~a has at least one item of content: ~a"
                  n len)
          (pass-if-> len 0))
        (loop (cdr rest) (+ 1 n))))))

(define (non-empty-registers/count dev cfg)
  (length (device-registers dev)))
