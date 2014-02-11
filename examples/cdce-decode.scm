;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules
 ((chip-remote devices ti cdce72010)
  #:renamer (symbol-prefix-proc 'cdce/))
 ((chip-remote devices ti cdce72010 decode)
  #:renamer (symbol-prefix-proc 'cdce/)))

(define foo '(#x683c0ed0
              #x68000051
              #x68000002
              #xeb800003
              #xe90c0004
              #x68000005
              #x68000006
              #x68000117
              #x69800158
              #xeb000049
              #x009c027a
              #x8800848b
              #x66009f0c))

(let next ((reg foo))
  (cond ((null? reg)
         (quit 0))
        (else
         (cdce/decode-register-by-value (car reg))
         (next (cdr reg)))))
