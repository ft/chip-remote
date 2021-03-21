;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 format)
             (ice-9 match)
             (test tap)
             (chip-remote codecs)
             (chip-remote semantics))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
    (no-plan)

  (for-each (lambda (n)
              (define-test (format #f "32-bit unsigned-integer ~a → ~a" n n)
                (pass-if-= n (s:encode unsigned-integer 32 n)))
              (define-test (format #f "32-bit unsigned-integer ~a ← ~a" n n)
                (pass-if-= n (s:decode unsigned-integer 32 n))))
            '(0 1 2 1024 1025 4000000000))

  (for-each (lambda (c)
              (let ((n (car c))
                    (e (cdr c)))
                (define-test (format #f "32-bit two's-complement ~a → ~8,'0x"
                                     n e)
                  (pass-if-= e (s:encode twos-complement 32 n)))
                (define-test (format #f "32-bit two's-complement ~a ← ~8,'0x"
                                     n e)
                  (pass-if-= n (s:decode twos-complement 32 e)))))
            '((          0 . #x00000000)
              (          1 . #x00000001)
              (         -1 . #xffffffff)
              (          2 . #x00000002)
              (         -2 . #xfffffffe)
              (       1024 . #x00000400)
              (      -1024 . #xfffffc00)
              ( 2000000000 . #x77359400)
              (-2000000000 . #x88ca6c00)))

  (for-each (lambda (c)
              (let ((n (car c))
                    (e (cdr c)))
                (define-test (format #f "32-bit one's-complement ~a → ~8,'0x"
                                     n e)
                  (pass-if-= e (s:encode ones-complement 32 n)))
                (define-test (format #f "32-bit one's-complement ~a ← ~8,'0x"
                                     n e)
                  (pass-if-= n (s:decode ones-complement 32 e)))))
            '((          0 . #x00000000)
              (          1 . #x00000001)
              (         -1 . #xfffffffe)
              (          2 . #x00000002)
              (         -2 . #xfffffffd)
              (       1024 . #x00000400)
              (      -1024 . #xfffffbff)
              ( 2000000000 . #x77359400)
              (-2000000000 . #x88ca6bff)))

  (define-test "one's complement's negative zero decodes to zero"
    (pass-if-= 0 (s:decode ones-complement 8 #xff)))

  (for-each (lambda (c)
              (let ((n (car c))
                    (e (cdr c)))
                (define-test (format #f "32-bit signed-magnitude ~a → ~8,'0x"
                                     n e)
                  (pass-if-= e (s:encode signed-magnitude 32 n)))
                (define-test (format #f "32-bit signed-magnitude ~a ← ~8,'0x"
                                     n e)
                  (pass-if-= n (s:decode signed-magnitude 32 e)))))
            '((          0 . #x80000000)
              (          1 . #x80000001)
              (         -1 . #x00000001)
              (          2 . #x80000002)
              (         -2 . #x00000002)
              (       1024 . #x80000400)
              (      -1024 . #x00000400)
              ( 2000000000 . #xf7359400)
              (-2000000000 . #x77359400)))

  (define-test "signed magnitude's negative zero decodes to zero"
    (pass-if-= 0 (s:decode signed-magnitude 8 #x00)))

  (for-each (lambda (c)
              (let ((n (car c))
                    (e (cdr c)))
                (define-test (format #f "32-bit offset-binary ~a → ~8,'0x"
                                     n e)
                  (pass-if-= e (s:encode offset-binary 32 n)))
                (define-test (format #f "32-bit offset-binary ~a ← ~8,'0x"
                                     n e)
                  (pass-if-= n (s:decode offset-binary 32 e)))))
            '((          0 . #x80000000)
              (          1 . #x80000001)
              (         -1 . #x7fffffff)
              (          2 . #x80000002)
              (         -2 . #x7ffffffe)
              (       1024 . #x80000400)
              (      -1024 . #x7ffffc00)
              ( 2000000000 . #xf7359400)
              (-2000000000 . #x08ca6c00)))

  (for-each (lambda (c)
              (let ((n (car c))
                    (e (cdr c)))
                (define-test (format #f "boolean ~a → ~a" n e)
                  (pass-if-= e (s:encode boolean 1 n)))
                (let ((expect (if (boolean-true? n) 'enabled 'disabled)))
                  (define-test (format #f "boolean ~a ← ~a" n expect)
                    (pass-if-eq? expect (s:decode boolean 1 e))))))
            '((      #t . 1)
              (       1 . 1)
              (      on . 1)
              (    true . 1)
              (  enable . 1)
              ( enabled . 1)
              (      #f . 0)
              (       0 . 0)
              (     off . 0)
              (   false . 0)
              ( disable . 0)
              (disabled . 0)))

  (for-each (lambda (c)
              (let ((n (car c))
                    (e (cdr c)))
                (define-test (format #f "boolean/active-low ~a → ~a" n e)
                  (pass-if-= e (s:encode boolean/active-low 1 n)))
                (let ((expect (if (boolean-true? n) 'enabled 'disabled)))
                  (define-test (format #f "boolean/active-low ~a ← ~a" n expect)
                    (pass-if-eq? expect (s:decode boolean/active-low 1 e))))))
            '((      #t . 0)
              (       1 . 0)
              (      on . 0)
              (    true . 0)
              (  enable . 0)
              ( enabled . 0)
              (      #f . 1)
              (       0 . 1)
              (     off . 1)
              (   false . 1)
              ( disable . 1)
              (disabled . 1)))

  (for-each (lambda (c)
              (match c
                ((number encoded epsilon)
                 (define-test (format #f "ieee-754-single ~12a → ~8,'0x" number encoded)
                   (pass-if-= encoded (s:encode ieee-754-single 32 number)))
                 (define-test (format #f "ieee-754-single ~12a ← ~8,'0x (epsilon: ~a)"
                                      number encoded epsilon)
                   (if (zero? epsilon)
                       (pass-if-= number (s:decode ieee-754-single 32 encoded))
                       (pass-if-~= number (s:decode ieee-754-single 32 encoded) epsilon))))))
            '((  0.0e+0     #x00000000  0)
              ( -0.0e+0     #x80000000  0)
              (  0.1e+0     #x3dcccccd  1.5e-9)
              ( -0.1e+0     #xbdcccccd  1.5e-9)
              (  1.0e+0     #x3f800000  0)
              ( -1.0e+0     #xbf800000  0)
              (  1.0e+3     #x447a0000  0)
              (  1.0e-3     #x3a83126f  5e-11)
              (123.0e+0     #x42f60000  0)
              (123.123e+0   #x42f63efa  1.1e-6)
              (123.123e+3   #x47f07980  0)
              (123.123e-3   #x3dfc27e9  2.5e-9)
              (123.123e+12  #x56dff59d  3e6)
              (123.123e-12  #x2f07600b  2.1e-18)))

  (for-each (lambda (c)
              (match c
                ((number encoded epsilon)
                 (define-test (format #f "ieee-754-double ~12a → ~16,'0x" number encoded)
                   (pass-if-= encoded (s:encode ieee-754-double 64 number)))
                 (define-test (format #f "ieee-754-double ~12a ← ~16,'0x (epsilon: ~a)"
                                      number encoded epsilon)
                   (if (zero? epsilon)
                       (pass-if-= number (s:decode ieee-754-double 64 encoded))
                       (pass-if-~= number (s:decode ieee-754-double 64 encoded) epsilon))))))
            ;; Precision was approximated by using data from:
            ;;
            ;;   https://baseconvert.com/ieee-754-floating-point
            ;;
            ;; while using a machine that has ieee-754-doubles a its maximum
            ;; precision. On this machine, you could actually do the conversion
            ;; back and forth, and the machine wouldn't be able to detect an
            ;; encoding error, because it is at the limits of its precision.
            `((  0.0e+0     #x0000000000000000  0)
              ( -0.0e+0     #x8000000000000000  0)
              (  0.1e+0     #x3fb999999999999a  1e-17)
              ( -0.1e+0     #xbfb999999999999a  1e-17)
              (  1.0e+0     #x3ff0000000000000  0)
              ( -1.0e+0     #xbff0000000000000  0)
              (  1.0e+3     #x408f400000000000  0)
              (  1.0e-3     #x3f50624dd2f1a9fc  1e-18)
              (123.0e+0     #x405ec00000000000  0)
              (123.123e+0   #x405ec7df3b645a1d  1e-14)
              (123.123e+3   #x40fe0f3000000000  0)
              (123.123e-3   #x3fbf84fd2a62aa19  7e-18)
              (123.123e+12  #x42dbfeb3ab6f8000  0)
              (123.123e-12  #x3de0ec0164d2cb80  2e-26)))

  (define-test "default semantics for width 1 work (boolean)"
    (pass-if-eq? (semantics-name (deduce-semantics 1 #f))
                 'boolean))
  (define-test "default semantics for width 2.. work (unsigned-integer)"
    (pass-if-eq? (semantics-name (deduce-semantics 2 #f))
                 'unsigned-integer))

  )
