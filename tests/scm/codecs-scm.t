;; -*- scheme -*-

;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote codecs))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (plan 26)

 (define-test "decode-state 1 works"
   (pass-if-eq? (decode-state 1)
                'enabled))
 (define-test "decode-state 0 works"
   (pass-if-eq? (decode-state 0)
                'disabled))
 (define-test "decode-boolean 1 works"
   (pass-if-eq? (decode-boolean 1)
                'true))
 (define-test "decode-boolean 0 works"
   (pass-if-eq? (decode-boolean 0)
                'false))
 (define-test "active-low: decode-state 1 works"
   (pass-if-eq? (decode-state/active-low 1)
                'disabled))
 (define-test "active-low: decode-boolean 1 works"
   (pass-if-eq? (decode-boolean/active-low 1)
                'false))
 (define-test "encode-state true works"
   (pass-if-= (encode-state 'enabled)
              1))
 (define-test "boolean->data true works"
   (pass-if-= (encode-state #t)
              1))
 (define-test "active-low: boolean->data true works"
   (pass-if-= (encode-state/active-low 0)
              1))
 (define-test "active-low: encode-state true works"
   (pass-if-= (encode-state/active-low 'disabled)
              1))

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

 (define-test "decode-sign-magnitude 4 0: works (negative zero)"
   (pass-if-= (decode-sign-magnitude 4 0)
              0))
 (define-test "decode-sign-magnitude 4 0: works (positive zero)"
   (pass-if-= (decode-sign-magnitude 4 8)
              0))
 (define-test "decode-sign-magnitude 4 7: works"
   (pass-if-= (decode-sign-magnitude 4 7)
              -7))
 (define-test "decode-sign-magnitude 4 9: works"
   (pass-if-= (decode-sign-magnitude 4 9)
              1))
 (define-test "decode-sign-magnitude 4 15: works"
   (pass-if-= (decode-sign-magnitude 4 15)
              7))

 (define-test "decode-offset-binary 4 8: works"
   (pass-if-= (decode-offset-binary 4 8)
              0))
 (define-test "decode-offset-binary 4 15: works"
   (pass-if-= (decode-offset-binary 4 15)
              7))
 (define-test "decode-offset-binary 4 0: works"
   (pass-if-= (decode-offset-binary 4 0)
              -8)))
