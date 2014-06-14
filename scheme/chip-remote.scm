;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote)
  #:use-module (ice-9 optargs)
  #:export (chain-fncs

            bit-getter-of
            bit-setter-of
            register-address-of))

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

(define (variable-of name module generator renamer exception)
  (let* ((vname (renamer (generator name)))
         (var (module-variable module vname)))
    (if (not var)
        (throw exception name vname)
        (variable-ref var))))

(define* (register-address-of name #:key
                              (renamer (lambda (x) x))
                              (module (current-module)))
  "Discover register address for the NAME field in the register map defined by
`define-register-map'. Consider this:

  (define-register-map foo
    (0 (contents (gain 0 4))))

Usually, you would use `regaddr:gain' to get \"0\" - the address for `gain'.
But in cases where you need to computationally tie the name `gain' to the
address in `regaddr:again', this function can be used. There are three basic
situations:

  1. You imported the module containing the corresponding `define-register-map'
     regularly:

        (use-modules (chip-remote devices xxx foo registers))

        (register-address-of 'gain) => 0

  2. You imported the module with a symbol renamer in place:

        (use-modules ((chip-remote devices xxx foo registers)
                      #:renamer (symbol-prefix-proc 'bar/)))

        (register-address-of 'gain #:renamer (symbol-prefix-proc 'bar/))
          => 0

  3. You want the address of `gain' from a module, you didn't import:

        (register-address-of 'gain
          #:module (resolve-module '(chip-remote devices xxx foo registers)))
          => 0"
  (variable-of name module
               (lambda (x) (symbol-append 'regaddr: x))
               renamer
               'cr-no-such-register-address))

(define* (bit-getter-of name #:key
                        (renamer (lambda (x) x))
                        (module (current-module)))
  "Like `register-address-of', but instead returns the getter function for a
register map entry."
  (variable-of name module
               (lambda (x) (symbol-append 'get- x '-bits))
               renamer
               'cr-no-such-getter-function))

(define* (bit-setter-of name #:key
                        (renamer (lambda (x) x))
                        (module (current-module)))
  "Like `register-address-of', but instead returns the setter function for a
register map entry."
  (variable-of name module
               (lambda (x) (symbol-append 'set- x '-bits))
               renamer
               'cr-no-such-setter-function))
