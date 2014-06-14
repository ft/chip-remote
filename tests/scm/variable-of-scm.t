;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote)
             (chip-remote devices ti ads4149 registers)
             ((chip-remote devices ti lmk04828 registers) #:prefix lmk/))

;; Tests for functions based on ‘variable-of’ from the (chip-remote) module
;;
;; The way these functions work is a little bit messy. They shouldn't be used
;; in library code. But in scripts, that use the library, it may be useful to
;; get the address of an entry in a register-map without having to traverse the
;; entire register map first, since there is a variable regaddr:foo that points
;; to it. Similarly, you may want to reference the generated getter and setter
;; functions get-foo-bits and set-foo-bits.

(define lmk-module
  (resolve-module '(chip-remote devices ti lmk04828 registers)))

(with-fs-test-bundle
 (plan 9)
 (define-test "register-address-of ads4149 gain"
   (pass-if-= (register-address-of 'gain)
              #x25))
 (define-test "register-address-of lmk fb-mux (#:renamer)"
   (pass-if-= (register-address-of 'fb-mux #:renamer (symbol-prefix-proc 'lmk/))
              #x13f))
 (define-test "register-address-of lmk fb-mux (#:module)"
   (pass-if-= (register-address-of 'fb-mux #:module lmk-module)
              #x13f))
 (define-test "bit-getter-of ads4149 gain"
   (pass-if-eq? (bit-getter-of 'gain)
                get-gain-bits))
 (define-test "bit-getter-of lmk fb-mux (#:renamer)"
   (pass-if-eq? (bit-getter-of 'fb-mux #:renamer (symbol-prefix-proc 'lmk/))
                lmk/get-fb-mux-bits))
 (define-test "bit-getter-of lmk fb-mux (#:module)"
   (pass-if-eq? (bit-getter-of 'fb-mux #:module lmk-module)
                lmk/get-fb-mux-bits))
 (define-test "bit-setter-of ads4149 gain"
   (pass-if-eq? (bit-setter-of 'gain)
                set-gain-bits))
 (define-test "bit-setter-of lmk fb-mux (#:renamer)"
   (pass-if-eq? (bit-setter-of 'fb-mux #:renamer (symbol-prefix-proc 'lmk/))
                lmk/set-fb-mux-bits))
 (define-test "bit-setter-of lmk fb-mux (#:module)"
   (pass-if-eq? (bit-setter-of 'fb-mux #:module lmk-module)
                lmk/set-fb-mux-bits)))
