(use-modules
 ((ti cdce72010)
  #:renamer (symbol-prefix-proc 'cdce/))
 ((ti cdce72010 decode)))

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
         (decode-register-by-value (car reg))
         (next (cdr reg)))))
