;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote register)
             (chip-remote register-map)
             (chip-remote combination))

(force-import (chip-remote combination) value-fits-target?)
(force-import (chip-remote combination) find-register-value)

(primitive-load "tests/test-tap-cfg.scm")

(define-register test-register
  #:contents
  (foo 0 3)
  (bar 3 4)
  (baz 7 1))

(define-register-map test-register-map
  #:table
  (#x0 (#:contents (foo? 0 1)
                   (bar? 1 1)
                   (stuff-low 2 6)))
  (#x1 (#:contents (stuff-high 0 2)
                   (boofar 2 6)))
  (#x2 (#:contents (thing-low 0 4)
                   (foobar 4 2)
                   (thing-high 6 2))))

(with-fs-test-bundle
 (plan 9)

 (let* ((p-foo (make-part 'foo))
        (p-baz (make-part 'baz))
        (foobaz (make-combination (list p-foo p-baz))))
   (define-test "p-foo is a part"
     (pass-if-true (part? p-foo)))
   (define-test "p-baz is a part"
     (pass-if-true (part? p-baz)))
   (define-test "foobaz is a combination"
     (pass-if-true (combination? foobaz)))
   (define-test "test-register can eat integers"
     (pass-if-true (value-fits-target? test-register 23)))
   (define-test "test-register can't eat lists of integers"
     (pass-if-false (value-fits-target? test-register '(23))))
   (define-test "register value for foo is trivial if target is a register"
     (pass-if-= (find-register-value test-register 'foo 23) 23))
   (define-test "test-register-map can eat a list of integers"
     (pass-if-true (value-fits-target? test-register-map '(23 42 7))))
   (define-test "test-register-map can't eat a integers"
     (pass-if-false (value-fits-target? test-register-map 23)))
   (define-test "register value for boofar is correct"
     (pass-if-= (find-register-value test-register-map 'boofar '(23 #x83 7))
                #x83))))
