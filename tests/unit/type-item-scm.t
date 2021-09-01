;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (chip-remote codecs)
             (chip-remote item)
             (chip-remote semantics))

(init-test-tap!)

(with-fs-test-bundle
 (plan 17)

 ;; Test the different call structures of the generate-item macro
 (define-test "generate-item, default call structure"
   (pass-if-true (item? (generate-item foo 2 4))))
 (define-test "generate-item, annotated call structure"
   (pass-if-true (item? (generate-item foo #:offset 2 #:width 4))))
 (define-test "generate-item, fully annotated call structure"
   (pass-if-true (item? (generate-item #:name 'foo #:offset 2 #:width 4))))
 (define-test "generate-item, default call structure (with meta)"
   (pass-if-true (item? (generate-item foo 2 4
                                       #:semantics* unsigned-integer))))
 (define-test "generate-item, annotated call structure (with meta)"
   (pass-if-true (item? (generate-item foo #:offset 2 #:width 4
                                       #:semantics* offset-binary))))
 (define-test "generate-item, fully annotated call structure (with meta)"
   (pass-if-true (item? (generate-item #:name 'foo #:offset 2 #:width 4
                                       #:semantics* twos-complement))))

 (define-test "generate-item, width 1 defaults to boolean"
   (pass-if-eq? (semantics-name (item-semantics (generate-item foo 2 1)))
                'boolean))

 (define-test "generate-item, longer widths default to unsigned-integer"
   (pass-if-eq? (semantics-name (item-semantics (generate-item foo 2 2)))
                'unsigned-integer))

 ;; Generate items and test their properties
 (let* ((item (generate-item thing 4 12)))
   (define-test "No meta, leads to empty meta list. Good."
     (pass-if-equal? (item-meta item)
                     '())))

 (let* ((item (generate-item thing 4 12 #:semantics* unsigned-integer))
        (getter (item-get item))
        (setter (item-set item))
        (sem (item-semantics item)))
   (define-test "item-name => 'thing"
     (pass-if-eq? (item-name item) 'thing))
   (define-test "item-offset => 4"
     (pass-if-= (item-offset item) 4))
   (define-test "item-width => 12"
     (pass-if-= (item-width item) 12))
   (define-test "item-meta => #:sem..."
     (pass-if-equal? (semantics-name sem) 'unsigned-integer))
   (define-test "item-getter works"
     (pass-if-= (getter #b1011110111111010)
                #b101111011111))
   (define-test "item-setter works"
     (pass-if-= (setter #b1011110111111010
                        #b100000001111)
                #b1000000011111010)))

 (let ((item-a (generate-item thing 4 12 #:default 1000))
       (item-b (generate-item thing 4 12)))
   (define-test "item-default works #1"
     (pass-if-= (item-default item-a)
                1000))
   (define-test "item-default works #2"
     (pass-if-= (item-default item-b)
                0))))
