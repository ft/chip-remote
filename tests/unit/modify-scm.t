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
             (chip-remote device)
             (chip-remote devices bosch bno055)
             (chip-remote devices linear-technology ltc6603)
             (chip-remote devices texas-instruments cdce72010)
             (chip-remote decode)
             (chip-remote decode to-text)
             (chip-remote semantics)
             (chip-remote validate)
             (chip-remote modify))

(init-test-tap!)

(define r:ltc (device-register ltc6603))
(define rm:ltc (device-address ltc6603 #f))
(define pm:ltc (device-page-map ltc6603))

(with-fs-test-bundle
 (plan 21)

 ;; Modifying registers
 (define-test "Modify item in register by name"
   (pass-if-= 10 (modify r:ltc 8 'shutdown? #t)))
 (define-test "Modify item in register by idx"
   (pass-if-= 10 (modify r:ltc 8 1 #t)))
 (define-test "Modify item in register by (name idx)"
   (pass-if-= 10 (modify r:ltc 8 '(shutdown? 0) #t)))

 ;; Modifying registers-maps...
 (define-test "Modify item in register-map by name"
   (pass-if-equal? '(10) (modify rm:ltc '(8) 'shutdown? #t)))
 (define-test "Modify item in register-map by (addr name)"
   (pass-if-equal? '(10) (modify rm:ltc '(8) '(#f shutdown?) #t)))
 (define-test "Modify item in register-map by (addr idx)"
   (pass-if-equal? '(10) (modify rm:ltc '(8) '(#f 1) #t)))
 (define-test "Modify item in register-map by (addr name idx)"
   (pass-if-equal? '(10) (modify rm:ltc '(8) '(#f shutdown? 0) #t)))

 ;; Modifying page-maps...
 (define-test "Modify item in page-map by name"
   (pass-if-equal? '((10)) (modify pm:ltc '((8)) 'shutdown? #t)))
 (define-test "Modify item in page-map by (addr addr name)"
   (pass-if-equal? '((10)) (modify pm:ltc '((8)) '(#f #f shutdown?) #t)))
 (define-test "Modify item in page-map by (addr addr idx)"
   (pass-if-equal? '((10)) (modify pm:ltc '((8)) '(#f #f 1) #t)))
 (define-test "Modify item in page-map by (addr addr name idx)"
   (pass-if-equal? '((10)) (modify pm:ltc '((8)) '(#f #f shutdown? 0) #t)))

 ;; Modifying devices...
 (define-test "Modify item in device by name"
   (pass-if-equal? '((10)) (modify ltc6603 '((8)) 'shutdown? #t)))
 (define-test "Modify item in device by (addr addr name)"
   (pass-if-equal? '((10)) (modify ltc6603 '((8)) '(#f #f shutdown?) #t)))
 (define-test "Modify item in device by (addr addr idx)"
   (pass-if-equal? '((10)) (modify ltc6603 '((8)) '(#f #f 1) #t)))
 (define-test "Modify item in device by (addr addr name idx)"
   (pass-if-equal? '((10)) (modify ltc6603 '((8)) '(#f #f shutdown? 0) #t)))

 ;; Modify with default
 (define-test "Modify item in device with default value"
   (pass-if-equal? '((18)) (modify* ltc6603 'shutdown? #t)))

 ;; Chain modify
 (define-test "Chain-modify on register works"
   (pass-if-= 147 (chain-modify r:ltc 0
                                '(shutdown? #t)
                                '(enable-output? #t)
                                '(low-pass-cfg div-by-32)
                                '(gain-cfg 6dB))))
 (define-test "Chain-modify on register-map works"
   (pass-if-equal? '(147) (chain-modify rm:ltc '(0)
                                        '(shutdown? #t)
                                        '(enable-output? #t)
                                        '(low-pass-cfg div-by-32)
                                        '(gain-cfg 6dB))))
 (define-test "Chain-modify on page-map works"
   (pass-if-equal? '((147)) (chain-modify pm:ltc '((0))
                                          '(shutdown? #t)
                                          '(enable-output? #t)
                                          '(low-pass-cfg div-by-32)
                                          '(gain-cfg 6dB))))
 (define-test "Chain-modify on device works"
   (pass-if-equal? '((147)) (chain-modify ltc6603 '((0))
                                          '(shutdown? #t)
                                          '(enable-output? #t)
                                          '(low-pass-cfg div-by-32)
                                          '(gain-cfg 6dB))))
 (define-test "Chain-modify with default on device works"
   (pass-if-equal? '((147)) (chain-modify* ltc6603
                                           '(shutdown? #t)
                                           '(enable-output? #t)
                                           '(low-pass-cfg div-by-32)
                                           '(gain-cfg 6dB)))))
