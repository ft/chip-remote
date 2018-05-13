;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test validate-device)
  #:use-module (test tap)
  #:use-module (test validate complete-registers)
  #:use-module (test validate has-register)
  #:use-module (test validate unique-items)
  #:use-module (test validate item-defaults-work)
  #:use-module (test validate items-dont-overlap)
  #:use-module (test validate items-no-holes)
  #:use-module (test validate registers-have-content)
  #:export (validate-device))

(define *tests*
  `((,has-register/check ,has-register/count)
    (,non-empty-registers/check ,non-empty-registers/count)
    (,unique-items/check ,unique-items/count)
    (,items-dont-overlap/check ,items-dont-overlap/count)
    (,items-no-holes/check ,items-no-holes/count)
    (,complete-registers/check ,complete-registers/count)
    (,item-defaults-work/check ,item-defaults-work/count)))

(define (planned-tests dev cfg)
  (apply + (map (lambda (x) (x dev cfg))
                (map cadr *tests*))))

(define (validate-device dev . cfg)
  (plan (planned-tests dev cfg))
  (for-each (lambda (x) (x dev cfg))
            (map car *tests*)))
