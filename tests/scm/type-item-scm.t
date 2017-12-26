;; -*- scheme -*-

;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote item))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (plan 13)

 ;; Test the different call structures of the generate-item macro
 (define-test "generate-item, default call structure"
   (pass-if-true (item? (generate-item foo 2 4))))
 (define-test "generate-item, annotated call structure"
   (pass-if-true (item? (generate-item foo #:offset 2 #:width 4))))
 (define-test "generate-item, fully annotated call structure"
   (pass-if-true (item? (generate-item #:name 'foo #:offset 2 #:width 4))))
 (define-test "generate-item, default call structure (with meta)"
   (pass-if-true (item? (generate-item foo 2 4
                                       #:semantics 'quuz))))
 (define-test "generate-item, annotated call structure (with meta)"
   (pass-if-true (item? (generate-item foo #:offset 2 #:width 4
                                       #:semantics 'quuz))))
 (define-test "generate-item, fully annotated call structure (with meta)"
   (pass-if-true (item? (generate-item #:name 'foo #:offset 2 #:width 4
                                       #:semantics 'quuz))))

 ;; Generate items and test their properties
 (let* ((item (generate-item thing 4 12)))
   (define-test "No meta, leads to empty meta list. Good."
     (pass-if-equal? (item-meta item)
                     '())))

 (let* ((item (generate-item thing 4 12 #:semantics 'integer))
        (getter (item-get item))
        (setter (item-set item)))
   (define-test "item-name => 'thing"
     (pass-if-eq? (item-name item) 'thing))
   (define-test "item-offset => 4"
     (pass-if-= (item-offset item) 4))
   (define-test "item-width => 12"
     (pass-if-= (item-width item) 12))
   (define-test "item-meta => #:sem..."
     (pass-if-equal? (item-meta item)
                     '((#:semantics . integer))))
   (define-test "item-getter works"
     (pass-if-= (getter #b1011110111111010)
                #b101111011111))
   (define-test "item-setter works"
     (pass-if-= (setter #b1011110111111010
                        #b100000001111)
                #b1000000011111010))))
