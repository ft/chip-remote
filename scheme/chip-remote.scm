;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote)
  #:export (chain-fncs))

;; Functions that alter packs of bits in register values look like this:
;;
;;    (enable-foobar regval)
;;    (set-thisthing regval 'some-thing)
;;
;; To do both, you'd have to do this:
;;
;;    (enable-foobar (set-this-thing regval 'something))
;;
;; With two functions that's not so bad, but it gets unwieldy quickly if you
;; want to chain more functions like that. Here's a macro that expands into the
;; nested call:
;;
;; (chain-fncs regval (set-this-thing 'something) enable-foobar)
(define-syntax chain-fncs
  (lambda (x)
    (syntax-case x ()
      ;; If no functions are specified, just expand to the value.
      ((_ v) #'v)
      ;; If there are one or more functions, recurse...
      ((_ v f0 ... fn)
       (let next ((fnc #'fn)
                  ;; Why `reverse'? Because the functions are to be applied in
                  ;; the order they are specified.
                  (f-rest (reverse #'(f0 ...))))
         (if (null? f-rest)
             (syntax-case fnc ()
               ((f args ...) #'(f v args ...))
               (f #'(f v)))
             (with-syntax ((rest (next (car f-rest)
                                       (cdr f-rest))))
               (syntax-case fnc ()
                 ((f args ...) #'(f rest args ...))
                 (f #'(f rest))))))))))
