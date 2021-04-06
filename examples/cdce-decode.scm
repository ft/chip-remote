;; Copyright (c) 2011-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules ((chip-remote legacy ti cdce72010)
              #:renamer (symbol-prefix-proc 'cdce/))
             ((chip-remote legacy ti cdce72010 decode)
              #:renamer (symbol-prefix-proc 'cdce/)))

;; These are the factory-default values for CDCE72010 legacy. You can
;; obviously takea look at the reference documentation about what these
;; register values represent...
(define factory-defaults '(#x683c0ed0
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

;; ...or, you could just iterate across the values from the previously defined
;; list and use the decoding module and let it tell you what the register
;; values mean.
(let next ((regs factory-defaults))
  (cond ((null? regs)
         (quit 0))
        (else
         (cdce/decode-register-by-value (car regs))
         (next (cdr regs)))))
