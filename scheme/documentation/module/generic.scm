;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (documentation module generic)
  #:use-module (ice-9 session)
  #:use-module (documentation more)
  #:export (expand-procedure
            expand-macro
            expand-integer))

(define (expand-procedure mod name value)
  (list name 'procedure
        (or (procedure-documentation value) 'undocumented)
        (procedure-arguments value)
        (procedure-minimum-arity value)))

(define (expand-macro mod name value)
  (let ((tf (macro-transformer value))
        (inlined (inlinable? mod name)))
    (list name (if inlined 'procedure 'macro)
          (or (procedure-documentation tf) 'undocumented)
          (procedure-arguments (or inlined tf))
          (procedure-minimum-arity (or inlined tf)))))

(define (expand-integer mod name value)
  (list name 'integer
        (variable-documentation mod name)))
