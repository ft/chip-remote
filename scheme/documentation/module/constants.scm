;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

;; TODO: There is a lot of redundancy in here, that should be removed.

(define-module (documentation module constants)
  #:use-module (ice-9 format)
  #:export (expand-constants-integer))

(define (expand-constants-integer mod name value)
  (list name 'integer
        (cond ((symbol-prefix? 'CMD- name)
               (format #f "Identifier for the ‘~a’ command of the ‘~a’ object."
                       (substring (string-downcase (symbol->string name)) 4)
                       (last mod)))
              (else 'undocumented))))

