;; Copyright (c) 2016 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; This is a reimplementation of “legacy/ti/cdce72010” using the DSL compiler
;; framework. It is not API compatible with the old module, but is rather used
;; as a test subject for the new code generators.

(define-module (chip-remote devices ti cdce72010 low-level)
  #:use-module (chip-remote compiler level-one))

;; Here is the main call, that goes about generating the low-level access code:
;;
;;   - Take (chip-remote devices ti cdce72010 register) and look through
;;     guile's load-path to find a “.map” file that matches the pattern.
;;
;;   - From that file read all S-expressions.
;;
;;   - Interpret those expressions in terms of chip-remote's DSL.
;;
;;   - Expand into all level-one code.
;;
;; After that, for example for the “cp-current” setting from register 0x00 in
;; the cdce72010 the following items are generated:
;;
;;   - set-cp-current-bits
;;   - get-cp-current-bits
;;   - regaddr:cp-current
;;
;; …that for all the entries in all the registers in the device definition
;; file, as well as a parameter called “register-map” that contains the entire
;; result of all ‘register’ calls.

(generate-level-one (chip-remote devices ti cdce72010 register))
