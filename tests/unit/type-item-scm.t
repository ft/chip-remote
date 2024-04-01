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
  (plan 13)

  ;; Test the different call structures of the generate-item macro
  (define-test "new item, default call structure"
    (pass-if-true (item? (item (name 'foo) (offset 2) (width 4)))))
  (define-test "new item, short-hand syntax"
    (pass-if-true (item? (‣ foo 2 4))))
  (define-test "new item, with semantics and default"
    (pass-if-true (item? (‣ foo 2 4
                                (semantics unsigned-integer)
                                (default 10)))))
  (define-test "item width 1 defaults to boolean"
    (pass-if-eq? (semantics-name (item-semantics (‣ foo 2 1)))
                 'boolean))
  (define-test "generate-item, longer widths default to unsigned-integer"
    (pass-if-eq? (semantics-name (item-semantics (‣ foo 2 2)))
                 'unsigned-integer))

  ;; Generate items and test their properties
  (let* ((item (‣ thing 4 12))
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
      (pass-if-= (item-get item #b1011110111111010)
                 #b101111011111))
    (define-test "item-setter works"
      (pass-if-= (item-set item
                           #b1011110111111010
                           #b100000001111)
                 #b1000000011111010)))

  (let ((item-a (‣ thing 4 12 (default 1000)))
        (item-b (‣ thing 4 12)))
    (define-test "item-default works #1"
      (pass-if-= (item-default item-a)
                 1000))
    (define-test "item-default works #2"
      (pass-if-= (item-default item-b)
                 0))))
