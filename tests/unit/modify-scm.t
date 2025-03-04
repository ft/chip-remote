;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (chip-remote codecs)
             (chip-remote interpreter)
             (chip-remote item)
             (chip-remote register)
             (chip-remote register-map)
             (chip-remote page-map)
             (chip-remote device)
             (chip-remote devices microchip mcp4351)
             (chip-remote devices linear-technology ltc6603)
             (chip-remote decode)
             (chip-remote decode to-text)
             (chip-remote semantics)
             (chip-remote utilities)
             (chip-remote modify))

(init-test-tap!)

(define r:ltc (device-ref ltc6603 #f #f))
(define rm:ltc (device-ref ltc6603 #f))
(define pm:ltc (device-page-map ltc6603))

(with-fs-test-bundle
  (plan 29)

  (let* ((dev (device
               (page-map
                (pm→
                 (table
                  (↔ ( 0 (rm→
                          (table
                           (↔ (   0 († (‣ thing 0 4)
                                       (‣ fish 4 4 (default 8))))
                              (   1 († (‣ more 0 4)
                                       (‣ stuff 4 4)))))))
                     ( 1 (rm→
                          (table (↔ (   0 († (‣ thing* 0 4)
                                             (‣ fish* 4 4)))
                                    (   1 († (‣ more* 0 4)
                                             (‣ stuff* 4 4)))
                                    (   2 († (‣ stuff 0 8 (default 111))))))))
                     (10 (rm→
                          (table (↔ (   8 († (‣ there 0 4)
                                             (‣ really 4 4)))
                                    ( 200 († (‣ is 0 4)
                                             (‣ lots 4 4)))
                                    (1025 († (‣ more-things 0 8 (default 101))))))))))))))
         (dev-default (device-default dev)))
    (define-test "Extract value by page-address"
      (pass-if-equal? (value-at-address dev-default 1)
                      '((0 . 0) (1 . 0) (2 . 111))))
    (define-test "Extract value by page-address with holes"
      (pass-if-equal? (value-at-address dev-default 10)
                      '((8 . 0) (200 . 0) (1025 . 101))))
    (define-test "Extract value by page-address and register-address"
      (pass-if-equal? (value-at-address dev-default 1 2) 111))
    (define-test "Extract value by page-address and register-address with holes"
      (pass-if-equal? (value-at-address dev-default 10 1025) 101)))

  ;; Modifying registers
  (define-test "Modify item in register by name"
    (pass-if-= 10 (modify r:ltc 8 'shutdown? #t)))
  (define-test "Modify item in register by idx"
    (pass-if-= 10 (modify r:ltc 8 1 #t)))
  (define-test "Modify item in register by (name idx)"
    (pass-if-= 10 (modify r:ltc 8 '(shutdown? 0) #t)))

  ;; Modifying registers-maps...
  (define-test "Modify item in register-map by name"
    (pass-if-equal? '((#f . 10))
                    (modify rm:ltc '((#f . 8)) 'shutdown? #t)))
  (define-test "Modify item in register-map by (addr name)"
    (pass-if-equal? '((#f . 10))
                    (modify rm:ltc '((#f . 8)) '(#f shutdown?) #t)))
  (define-test "Modify item in register-map by (addr idx)"
    (pass-if-equal? '((#f . 10))
                    (modify rm:ltc '((#f . 8)) '(#f 1) #t)))
  (define-test "Modify item in register-map by (addr name idx)"
    (pass-if-equal? '((#f . 10))
                    (modify rm:ltc '((#f . 8)) '(#f shutdown? 0) #t)))

  ;; Modifying page-maps...
  (define-test "Modify item in page-map by name"
    (pass-if-equal? '((#f . ((#f . 10))))
                    (modify pm:ltc '((#f . ((#f . 8)))) 'shutdown? #t)))
  (define-test "Modify item in page-map by (addr addr name)"
    (pass-if-equal? '((#f . ((#f . 10))))
                    (modify pm:ltc '((#f . ((#f . 8)))) '(#f #f shutdown?) #t)))
  (define-test "Modify item in page-map by (addr addr idx)"
    (pass-if-equal? '((#f . ((#f . 10))))
                    (modify pm:ltc '((#f . ((#f . 8)))) '(#f #f 1) #t)))
  (define-test "Modify item in page-map by (addr addr name idx)"
    (pass-if-equal? '((#f . ((#f . 10))))
                    (modify pm:ltc '((#f . ((#f . 8)))) '(#f #f shutdown? 0) #t)))

  ;; Modifying devices...
  (define-test "Modify item in device by name"
    (pass-if-equal? '((#f . ((#f . 10))))
                    (modify ltc6603 '((#f . ((#f . 8)))) 'shutdown? #t)))
  (define-test "Modify item in device by (addr addr name)"
    (pass-if-equal? '((#f . ((#f . 10))))
                    (modify ltc6603 '((#f . ((#f . 8)))) '(#f #f shutdown?) #t)))
  (define-test "Modify item in device by (addr addr idx)"
    (pass-if-equal? '((#f . ((#f . 10))))
                    (modify ltc6603 '((#f . ((#f . 8)))) '(#f #f 1) #t)))
  (define-test "Modify item in device by (addr addr name idx)"
    (pass-if-equal? '((#f . ((#f . 10))))
                    (modify ltc6603 '((#f . ((#f . 8)))) '(#f #f shutdown? 0) #t)))

  ;; Modify with default
  (define-test "Modify item in device with default value"
    (pass-if-equal? '((#f . ((#f . 18)))) (modify* ltc6603 'shutdown? #t)))

  ;; Chain modify
  (define-test "Chain-modify on register works"
    (pass-if-= 147
               (chain-modify r:ltc 0
                             '(shutdown? #t)
                             '(enable-output? #t)
                             '(low-pass-cfg div-by-32)
                             '(gain-cfg 6dB))))
  (define-test "Chain-modify on register-map works"
    (pass-if-equal? '((#f . 147))
                    (chain-modify rm:ltc '((#f . 0))
                                  '(shutdown? #t)
                                  '(enable-output? #t)
                                  '(low-pass-cfg div-by-32)
                                  '(gain-cfg 6dB))))
  (define-test "Chain-modify on page-map works"
    (pass-if-equal? '((#f . ((#f . 147))))
                    (chain-modify pm:ltc '((#f . ((#f. 0))))
                                  '(shutdown? #t)
                                  '(enable-output? #t)
                                  '(low-pass-cfg div-by-32)
                                  '(gain-cfg 6dB))))
  (define-test "Chain-modify on device works"
    (pass-if-equal? '((#f . ((#f . 147))))
                    (chain-modify ltc6603 '((#f . ((#f. 0))))
                                  '(shutdown? #t)
                                  '(enable-output? #t)
                                  '(low-pass-cfg div-by-32)
                                  '(gain-cfg 6dB))))
  (define-test "Chain-modify with default on device works"
    (pass-if-equal? '((#f . ((#f . 147))))
                    (chain-modify* ltc6603
                                   '(shutdown? #t)
                                   '(enable-output? #t)
                                   '(low-pass-cfg div-by-32)
                                   '(gain-cfg 6dB))))

  ;; Loading bigger entities than items.
  ;;
  ;; The default operation of the modification functions is to apply changes
  ;; directly to items. In some instances, it may be useful to directly load
  ;; change to a bigger part of a value, like a register, or one of the maps.
  ;;
  ;; This is implemented by prefixing the ADDRESS parameter of a change spec by
  ;; the #:load keyword.
  (define-test "Loading a register value into a device works"
    (pass-if-equal? '((#f . ((#f . 123))))
                    (chain-modify* ltc6603 '(#:load #f #f 123))))
  (define-test "Loading a register-map value into a device works"
    (pass-if-equal? '((#f . ((#f . 123))))
                    (chain-modify* ltc6603 '(#:load #f ((#f . 123))))))
  (define-test "Loading a page-map value into a device works"
    (pass-if-equal? '((#f . ((#f . 123))))
                    (chain-modify* ltc6603 '(#:load ((#f . ((#f . 123))))))))

  (define-test "Loading a register value into a less trivial device"
    (pass-if-equal? '((#f . ((0  . #xa0)
                             (1  . #xa1)
                             (4  . #xa4)
                             (6  . #xa6)
                             (7  . #xa7)
                             (10 . #xaa))))
                    (chain-modify* mcp4351
                                   '(#:load #f  0 #xa0)
                                   '(#:load #f  1 #xa1)
                                   '(#:load #f  4 #xa4)
                                   '(#:load #f  6 #xa6)
                                   '(#:load #f  7 #xa7)
                                   '(#:load #f 10 #xaa)))))
