;; -*- scheme -*-

;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote item)
             (chip-remote register))

(primitive-load "tests/test-tap-cfg.scm")

(define (fnct-that-returns-an-item name)
  (generate-item #:name name #:offset 0 #:width 8))

(with-fs-test-bundle
 (plan 14)

 (define-test "generate-register, call structure works"
   (pass-if-true (register? (generate-register #:default #x12
                                               #:contents
                                               (foo 0 3)
                                               (bar 3 5)))))
 (let* ((reg (generate-register #:default #x12
                                #:contents
                                (foo 0 3)
                                (bar 3 5)
                                #:contents*
                                (fnct-that-returns-an-item 'thing)))
        (meta (register-meta reg))
        (items (register-items reg))
        (foo (car items))
        (bar (cadr items)))
   (define-test "reg: meta looks good"
     (pass-if-equal? meta '((#:default . #x12))))
   (define-test "reg: first item is an <item>"
     (pass-if-true (item? foo)))
   (define-test "reg: its name is foo"
     (pass-if-eq? (item-name foo) 'foo))
   (define-test "reg: second item is an <item>, too"
     (pass-if-true (item? bar)))
   (define-test "reg: its name is bar"
     (pass-if-eq? (item-name bar) 'bar))
   (define-test "reg: item names are as expected"
     (pass-if-equal? (register-item-names reg)
                     '(foo bar thing)))
   (define-test "reg: the register's width is correct"
     (pass-if-= (register-width reg) 8))
   (define-test "reg: the regsiter has an item foo"
     (pass-if-true (register-contains? reg 'foo)))
   (define-test "reg: and another item called bar"
     (pass-if-true (register-contains? reg 'foo)))
   (define-test "reg: referencing foo yields the correct item"
     (pass-if-equal? (register-ref reg 'foo)
                     foo))
   (define-test "reg: referencing bar works as well"
     (pass-if-equal? (register-ref reg 'bar)
                     bar))
   (define-test "reg: setting an item works: foo"
     (pass-if-= (register-set reg 0 'foo 5)
                5))
   (define-test "reg: setting an item works: bar"
     (pass-if-= (register-set reg 0 'bar 33)
                (ash 33 (item-offset bar))))))
