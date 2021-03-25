;; -*- scheme -*-

;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (srfi srfi-1)
             (test tap)
             (chip-remote codecs)
             (chip-remote device)
             (chip-remote item)
             (chip-remote page-map)
             (chip-remote semantics)
             (chip-remote register-map)
             (chip-remote combination))

(primitive-load "tests/test-tap-cfg.scm")

(define-device thingy
  #:register-map (#:table (#x0 (#:contents (thing-mid 0 8 #:default #b10110001)
                                           (foo? 8 1)
                                           (bar? 9 1)
                                           (stuff-low 10 6  #:default #b101100)))
                          (#x1 (#:contents (stuff-high 0 2 #:default #b10)
                                           (boofar 2 6)))
                          (#x2 (#:contents (thing-low 0 4 #:default #b1001)
                                           (foobar 4 2)
                                           (thing-high 6 2 #:default #b10)))))

(with-fs-test-bundle (no-plan)

  (define-test "thingy is a device"
    (pass-if-true (device? thingy)))

  (define-test "Empty spec returns default combination"
    (pass-if-equal? (make-default-combination thingy '())
                    (combination-assemble thingy '())))

  (define-test "Non-empty spec returns a combination"
    (pass-if-true (combination? (combination-assemble
                                 thingy '(concatenate stuff-high stuff-low)))))

  (define-test "Raw value of 'stuff' combination checks out"
    (pass-if-eqv? (c:raw (combination-assemble
                          thingy '(concatenate stuff-high stuff-low)))
                  #b10101100))

  (define-test "Raw value of 'thing' combination checks out"
    (pass-if-eqv? (c:raw (combination-assemble
                          thingy '(concatenate thing-high thing-mid thing-low)))
                  #b10101100011001))

  (let* ((sl-addr (find-canonical-address thingy 'stuff-low))
         (sh-addr (find-canonical-address thingy 'stuff-high))
         (sl-item (apply device-address (cons thingy sl-addr)))
         (sh-item (apply device-address (cons thingy sh-addr)))
         (sl-reg (apply device-address (cons thingy (take sl-addr 2))))
         (sh-reg (apply device-address (cons thingy (take sh-addr 2)))))
    (define-test "Combination 'stuff' partitions correctly"
      (pass-if-equal? (combination-partition thingy
                                             '(concatenate stuff-high stuff-low)
                                             #b01010011)
                      `((,(cons sh-reg (take sh-addr 2))
                         ((,(caddr sh-addr) ,(item-width sl-item)
                           #b01 ,sh-item)))
                        (,(cons sl-reg (take sl-addr 2))
                         ((,(caddr sl-addr) 0
                           #b010011 ,sl-item)))))))

  (let* ((tl-addr (find-canonical-address thingy 'thing-low))
         (th-addr (find-canonical-address thingy 'thing-high))
         (tm-addr (find-canonical-address thingy 'thing-mid))
         (tl-item (apply device-address (cons thingy tl-addr)))
         (th-item (apply device-address (cons thingy th-addr)))
         (tm-item (apply device-address (cons thingy tm-addr)))
         (tlh-reg (apply device-address (cons thingy (take tl-addr 2))))
         (tm-reg (apply device-address (cons thingy (take tm-addr 2)))))
    (define-test "Combination 'thing' partitions correctly"
      (pass-if-equal? (combination-partition thingy
                                             '(concatenate thing-high
                                                           thing-mid
                                                           thing-low)
                                             #b01010011100110)
                      `((,(cons tlh-reg (take th-addr 2))
                         ((,(caddr th-addr) ,(+ (item-width tl-item)
                                                (item-width tm-item))
                           #b01 ,th-item)
                          (,(caddr tl-addr) 0 #b0110 ,tl-item)))
                        (,(cons tm-reg (take tm-addr 2))
                         ((,(caddr tm-addr) ,(item-width tl-item)
                           #b01001110 ,tm-item))))))))
