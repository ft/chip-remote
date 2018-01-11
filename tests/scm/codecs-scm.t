;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote codecs))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (plan 23)

 (define-test "decode-boolean 1 works"
   (pass-if-eq? (decode-boolean 1)
                'enabled))
 (define-test "decode-boolean 0 works"
   (pass-if-eq? (decode-boolean 0)
                'disabled))
 (define-test "active-low: decode-boolean 1 works"
   (pass-if-eq? (decode-boolean/active-low 1)
                'disabled))

 (define-test "decode-twos-complement 4 0: works"
   (pass-if-= (decode-twos-complement 4 0)
              0))
 (define-test "decode-twos-complement 4 7: works"
   (pass-if-= (decode-twos-complement 4 7)
              7))
 (define-test "decode-twos-complement 4 8: works"
   (pass-if-= (decode-twos-complement 4 8)
              -8))
 (define-test "decode-twos-complement 4 15: works"
   (pass-if-= (decode-twos-complement 4 15)
              -1))

 (define-test "decode-ones-complement 4 0: works"
   (pass-if-= (decode-ones-complement 4 0)
              0))
 (define-test "decode-ones-complement 4 7: works"
   (pass-if-= (decode-ones-complement 4 7)
              7))
 (define-test "decode-ones-complement 4 8: works"
   (pass-if-= (decode-ones-complement 4 8)
              -7))
 (define-test "decode-ones-complement 4 15: works (negative zero)"
   (pass-if-= (decode-ones-complement 4 15)
              0))

 (define-test "decode-signed-magnitude 4 0: works (negative zero)"
   (pass-if-= (decode-signed-magnitude 4 0)
              0))
 (define-test "decode-signed-magnitude 4 0: works (positive zero)"
   (pass-if-= (decode-signed-magnitude 4 8)
              0))
 (define-test "decode-signed-magnitude 4 7: works"
   (pass-if-= (decode-signed-magnitude 4 7)
              -7))
 (define-test "decode-signed-magnitude 4 9: works"
   (pass-if-= (decode-signed-magnitude 4 9)
              1))
 (define-test "decode-signed-magnitude 4 15: works"
   (pass-if-= (decode-signed-magnitude 4 15)
              7))

 (define-test "decode-offset-binary 4 8: works"
   (pass-if-= (decode-offset-binary 4 8)
              0))
 (define-test "decode-offset-binary 4 15: works"
   (pass-if-= (decode-offset-binary 4 15)
              7))
 (define-test "decode-offset-binary 4 0: works"
   (pass-if-= (decode-offset-binary 4 0)
              -8))

 (let* ((table '((something . 3)
                 (more . 5)
                 (stuff . 23)))
        (decoder (make-table-decoder table))
        (encoder (make-table-encoder table)))
   (define-test "table decoding works"
     (pass-if-eq? (decoder 3)
                  'something))
   (define-test "table decoding works (undefined)"
     (pass-if-eq? (decoder 42)
                  'undefined))
   (define-test "table encoding works"
     (pass-if-= (encoder 'more)
                5))
   (define-test "table encoding works (undefined)"
     (pass-if-eq? (encoder 'does-not-exist)
                  'undefined))))
