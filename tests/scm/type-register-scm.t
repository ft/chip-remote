;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote item)
             (chip-remote register))

(primitive-load "tests/test-tap-cfg.scm")

(define (fnct-that-returns-an-item name)
  (generate-item #:name name #:offset 3 #:width 2))

(with-fs-test-bundle
 (plan 23)

 (define-test "generate-register, call structure works"
   (pass-if-true (register? (generate-register #:default #x12
                                               #:contents
                                               (foo 0 3)
                                               (bar 3 5)))))
 (let* ((reg (generate-register #:default #x12
                                #:contents
                                (foo 0 3)
                                (bar 5 3)
                                #:contents*
                                (fnct-that-returns-an-item 'thing)
                                #:contents
                                (foo 8 8)))
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
                     '(foo bar thing foo)))
   (define-test "reg: the register's width is correct"
     (pass-if-= (register-width reg) 16))
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
                (ash 33 (item-offset bar))))
   (define-test "reg: register-address/n works #1"
     (pass-if-eq? (item-name (register-address reg 0))
                  'foo))
   (define-test "reg: register-address/n works #2"
     (pass-if-eq? (item-name (register-address reg 1))
                  'thing))
   (define-test "reg: register-address/n works #3"
     (pass-if-eq? (item-name (register-address reg 2))
                  'bar))
   (define-test "reg: register-address/name+cnt works #1"
     (pass-if-eq? (item-name (register-address reg 'foo 0))
                  'foo))
   (define-test "reg: register-address/name+cnt works #2"
     (pass-if-= (item-offset (register-address reg 'foo 0))
                0))
   (define-test "reg: register-address/name+cnt works #3"
     (pass-if-= (item-offset (register-address reg 'foo 1))
                8))
   (define-test "reg: register-ref/address works #1"
     (pass-if-eq? (item-name (register-ref/address reg 2))
                  'bar))
   (define-test "reg: register-ref/address works #2"
     (pass-if-eq? (item-name (register-ref/address reg 'foo))
                  'foo))
   (define-test "register-default works"
     (pass-if-= (register-default reg) #x12))))
