;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (chip-remote item)
             (chip-remote register))

(init-test-tap!)

(define (address default)
  (generate-item #:name 'address #:offset 14 #:width 2
                 #:access read-only #:default default))

(define (fnct-that-returns-an-item name)
  (generate-item #:name name #:offset 3 #:width 2))

(with-fs-test-bundle
 (plan 33)

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
                                (foo 8 6)
                                (=> (address 0))))
        (meta (register-meta reg))
        (items (register-items reg))
        (foo (car items))
        (bar (cadr items)))
   (define-test "canon: Negative indices are rejected"
     (pass-if-false (register-canonical reg -1)))
   (define-test "canon: Exceedingly large indices are rejected"
     (pass-if-false (register-canonical reg 5)))
   (define-test "canon: 0 is (0)"
     (pass-if-equal? '(0) (register-canonical reg 0)))
   (define-test "canon: foo is 0"
     (pass-if-equal? '(0) (register-canonical reg 'foo)))
   (define-test "canon: foo:0 is 0"
     (pass-if-equal? '(0) (register-canonical reg 'foo 0)))
   (define-test "canon: thing is 1"
     (pass-if-equal? '(1) (register-canonical reg 'thing)))
   (define-test "canon: thing:0 is 1"
     (pass-if-equal? '(1) (register-canonical reg 'thing 0)))
   (define-test "canon: bar is 2"
     (pass-if-equal? '(2) (register-canonical reg 'bar)))
   (define-test "canon: bar:0 is 2"
     (pass-if-equal? '(2) (register-canonical reg 'bar 0)))
   (define-test "canon: foo:1 is 3"
     (pass-if-equal? '(3) (register-canonical reg 'foo 1)))
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
                     '(foo bar thing foo address)))
   (define-test "reg: the register's width is correct"
     (pass-if-= (register-width reg) 16))
   (define-test "reg: the regsiter has an item foo"
     (pass-if-true (register-contains? reg 'foo)))
   (define-test "reg: and another item called bar"
     (pass-if-true (register-contains? reg 'foo)))
   (define-test "reg: addressing foo yields the correct item"
     (pass-if-equal? (register-address reg 'foo)
                     foo))
   (define-test "reg: addressing bar works as well"
     (pass-if-equal? (register-address reg 'bar)
                     bar))
   (define-test "reg: register-ref works #1"
     (pass-if-eq? (item-name (register-ref reg 0))
                  'foo))
   (define-test "reg: register-ref works #2"
     (pass-if-eq? (item-name (register-ref reg 1))
                  'thing))
   (define-test "reg: register-ref works #3"
     (pass-if-eq? (item-name (register-ref reg 2))
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
   (define-test "register-default works"
     (pass-if-= (register-default reg) #x12)))

 (let ((reg (generate-register #:contents
                               (address 0 4 #:default 10)
                               (foo 4 4) (bar 8 4) (baz? 12 1) (reserved 13 3)
                               (thing? 16 1) (fish? 17 1) (reserved 18 24)))
       (moar (generate-item moar 8 4))
       (at-the-end (generate-item at-the-end 32 4)))
   (define-test "Removing an item works (bar)"
     (pass-if-equal? (register->alist (derive-register-from reg #:remove 'bar))
                     '((address 0 4) (foo 4 4) (baz? 12 1) (reserved 13 3)
                       (thing? 16 1) (fish? 17 1) (reserved 18 24))))
   (define-test "Inserting an item works (moar)"
     (pass-if-equal? (register->alist
                      (derive-register-from reg #:insert moar))
                     '((address 0 4) (foo 4 4) (moar 8 4) (bar 8 4) (baz? 12 1)
                       (reserved 13 3) (thing? 16 1) (fish? 17 1)
                       (reserved 18 24))))
   (define-test "Removing and inserting an item works (moar for bar)"
     (pass-if-equal? (register->alist
                      (derive-register-from reg #:insert moar #:remove 'bar))
                     '((address 0 4) (foo 4 4) (moar 8 4) (baz? 12 1)
                       (reserved 13 3) (thing? 16 1) (fish? 17 1)
                       (reserved 18 24))))
   (define-test "Inserting at the end works"
     (pass-if-equal? (register->alist
                      (derive-register-from reg #:insert at-the-end))
                     '((address 0 4) (foo 4 4) (bar 8 4) (baz? 12 1)
                       (reserved 13 3) (thing? 16 1) (fish? 17 1)
                       (reserved 18 24) (at-the-end 32 4))))))
