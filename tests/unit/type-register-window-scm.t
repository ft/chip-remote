;; -*- scheme -*-

;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (chip-remote item)
             (chip-remote register)
             (chip-remote register-window))

(init-test-tap!)

(with-fs-test-bundle
 (plan 15)
 (let* ((reg (register (items (list (‣ foo 0 3)
                                    (‣ start? 3 1)
                                    (‣ bar 4 4)))))
        (win00 (make-register-window reg 3 5))
        (win01 (make-register-window reg 2 6))
        (win02 (make-register-window reg 3 4))
        (win03 (make-register-window reg 2 5)))
   (define-test "win00 is a register-window"
     (pass-if-true (register-window? win00)))
   (define-test "win01 is a register-window"
     (pass-if-true (register-window? win01)))
   (define-test "win02 is a register-window"
     (pass-if-true (register-window? win02)))
   (define-test "win03 is a register-window"
     (pass-if-true (register-window? win03)))
   (define-test "win00 has two items"
     (pass-if-= (length (window-items win00)) 2))
   (define-test "win01 is msi-complete"
     (pass-if-true (msi-complete? win01)))
   (define-test "win01 is NOT lsi-complete"
     (pass-if-false (lsi-complete? win01)))
   (define-test "win02 is NOT msi-complete"
     (pass-if-false (msi-complete? win02)))
   (define-test "win02 is lsi-complete"
     (pass-if-true (lsi-complete? win02)))
   (define-test "win03 is NOT msi-complete"
     (pass-if-false (msi-complete? win03)))
   (define-test "win03 is NOT lsi-complete"
     (pass-if-false (lsi-complete? win03)))
   (define-test "win00 has the correct items in the right order"
     (pass-if-equal? (map item-name (window-items win00))
                     '(start? bar)))
   (define-test "win01 has the correct items in the right order"
     (pass-if-equal? (map item-name (window-items win01))
                     '(foo start? bar)))
   (define-test "win02 has the correct items in the right order"
     (pass-if-equal? (map item-name (window-items win02))
                     '(start? bar)))
   (define-test "win03 has the correct items in the right order"
     (pass-if-equal? (map item-name (window-items win03))
                     '(foo start? bar)))))
