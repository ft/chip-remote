;; Copyright (c) 2019-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote register modifiers)
  #:use-module (srfi srfi-1)
  #:use-module (chip-remote register)
  #:use-module (chip-remote item)
  #:use-module ((chip-remote item access) #:prefix item:)
  #:export (read-write read-only write-only none))

(define (register-access reg access-fnc)
  (register (inherit reg)
            (items (map (lambda (i)
                          (item (inherit i)
                                (access (access-fnc))))
                        (register-items reg)))))

(define (read-write reg)
  (register-access reg item:rw))

(define (read-only reg)
  (register-access reg item:ro))

(define (write-only reg)
  (register-access reg item:wo))

(define (none reg)
  (register-access reg item:none))
