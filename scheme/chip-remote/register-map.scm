;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; Description:
;;
;; Register-maps are one description of configuration tables. They consist of
;; an address, and an alist, that describes the contents of the register in
;; question. Possible keys:
;;
;;     default-value: The default value of the register (if omitted, the
;;                    default-value defaults to zero)
;;
;;     contents: A list of items being configured by the register
;;
;; The content entry is a list of lists, each of which looks like this:
;;
;;     (symbol offset width)
;;
;; Where OFFSET is the offset of the entry within the register and WIDTH being
;; the number of bits the entry is comprised of. SYMBOL is the name for the
;; described entry, that has to be unique within the register-map.
;;
;; Example:
;;
;;     (define-register-map foo-bar
;;       (#x0 (default-value #x54)
;;            (contents (m-divider 0 4)
;;                      (n-divider 4 4))
;;       (#x1 (default-value #xd0)
;;            (contents (power-sabe 0 1)
;;                      (reference-divider 4 4))))
;;
;; That defines (using `define-public') the following functions:
;;
;;   - set-m-divider-bits
;;   - get-m-divider-bits
;;   - set-n-divider-bits
;;   - get-n-divider-bits
;;   - set-reference-divider-bits
;;   - get-reference-divider-bits
;;   - set-power-save-bits
;;   - get-power-save-bits
;;
;; as well as a set of variables:
;;
;;   - foo-bar-register-map
;;
;; that contains the entire register-map for later reference. And a few
;; back-references to register addresses:
;;
;;   - register-address-m-divider
;;   - register-address-n-divider
;;   - register-address-reference-divider
;;   - register-address-power-save

(define-module (chip-remote register-map)
  #:use-module (srfi srfi-1)
  #:export (define-register-map
            register-default
            map-across))

;; This macro gets a value that points to the piece of data (map or function)
;; that is able to decode a register-entry. Add its name and its type to the
;; register map to enable better diagnostics in case something goes wrong. In
;; case a mapping (an alist, so a list really) is the decode, wrap it into a
;; lamda, to point to the variable in question instead of all the data in the
;; variable instead. This should safe space in case big mappings are used often
;; as a decoder in a register-map.
(define-syntax expand-annotation
  (lambda (x)
    (syntax-case x ()
      ((_ name)
       #'(list (if (list? 'name)
                   (last 'name)
                   (quote name))
               (cond ((procedure? name) 'function)
                     ((list? name) 'list)
                     (else (throw 'cr-unsupported-decoder-at name)))
               (if (procedure? name)
                   name
                   (lambda () name)))))))

;; The user provides ‘name’, ‘offset’, ‘width’ and possibly ‘annotation’ (if
;; that's missing, it should default to ‘literal-binary’). However, the actual
;; register-map variable should also have references to these:
;;
;;   - The getter function for the piece of register content
;;   - The setter function for the piece of register content
;;   - An expanded ‘annotation’ into its value, its type and its name
(define-syntax expand-content
  (lambda (x)
    (syntax-case x (=>)
      ((kw (name offset width => annotation))
       (with-syntax ((sname (datum->syntax
                             #'kw (symbol-append 'set-
                                                 (syntax->datum #'name)
                                                 '-bits)))
                     (gname (datum->syntax
                             #'kw (symbol-append 'get-
                                                 (syntax->datum #'name)
                                                 '-bits))))
         #'(list 'name offset width gname sname
                 (expand-annotation annotation))))
      ((_ (name offset width))
       #'(expand-content (name offset width => (@ (chip-remote bit-decoders)
                                                  literal-binary)))))))

;; This one actually adds information to the bits of a register definition.
(define-syntax expand-datum
  (lambda (x)
    (syntax-case x (default-value contents)
      ;; ‘default-value’ can be a simple pair, but the users shouldn't have to
      ;; type (default-value . 23) for no good reason.
      ((_ (default-value v)) #'(cons 'default-value v))
      ;; ‘contents’ is actually the fun one. ‘expand-content’ does the work.
      ((_ (contents entry ...)) #'(list 'contents (expand-content entry) ...))
      ;; Everything else can just go out as it came in.
      ((_ (exp ...)) #'(list exp ...)))))

;; The register-map variable, the ‘define-register-map’ macro (among other
;; things) expands into, needs a bit more information than we burden the user
;; with to enter. This disects the original register-definition into an address
;; and subsequent datums and uses ‘expand-datum’ to add the additional
;; information.
(define-syntax expand-register
  (lambda (x)
    (syntax-case x ()
      ((_ (addr datum ...))
       #'(list addr (expand-datum datum) ...)))))

;; Expand into Level-1 getter code for a given register entry.
(define-syntax reg-expand-getter
  (lambda (x)
    (syntax-case x ()
      ((kw a n o w)
       (with-syntax ((name (datum->syntax
                            #'kw (symbol-append 'get-
                                                (syntax->datum #'n)
                                                '-bits))))
         #'(define-public (name register)
             ((@ (bitops) bit-extract-width) register o w)))))))

;; Expand into Level-1 setter code for a given register entry.
(define-syntax reg-expand-setter
  (lambda (x)
    (syntax-case x ()
      ((kw a n o w)
       (with-syntax ((name (datum->syntax
                            #'kw (symbol-append 'set-
                                                (syntax->datum #'n)
                                                '-bits))))
         #'(define-public (name register value)
             ((@ (bitops) set-bits) register
                                    (logand value (- (ash 1 w) 1)) w o)))))))

;; Expand into a variable definition that links a name to a register address.
(define-syntax reg-expand-address
  (lambda (x)
    (syntax-case x ()
      ((kw a n o w)
       (with-syntax ((name (datum->syntax
                            #'kw (symbol-append 'register-address-
                                                (syntax->datum #'n)))))
         #'(define-public name a))))))

;; This macro's job is to extract the contents bits from the
;; register-definitions, and feed that information into the macros that
;; actually expand into function and variable-definitions.
(define-syntax register-code-expand
  (lambda (x)
    (syntax-case x ()
      ((_ (addr (type exp* ...) ...))
       (with-syntax ((contents (filter (lambda (x)
                                         (let ((xx (syntax->datum x)))
                                           (eq? (car xx) 'contents)))
                                       #'((type exp* ...) ...))))
         (syntax-case #'contents ()
           (((type (n o w rest ...) ...))
            #'(begin (reg-expand-setter addr n o w) ...
                     (reg-expand-getter addr n o w) ...
                     (reg-expand-address addr n o w) ...))))))))

;; This just dispatches each register to ‘register-code-expand’ and assembles
;; the result in a ‘begin’ expression.
(define-syntax expand-definitions
  (lambda (x)
    (syntax-case x ()
      ((kw reg ...)
       #'(begin (register-code-expand reg) ...)))))

;; This is the main entry point. Its input are a bunch of sexps that each
;; describe one register in the register-map. Its job is to turn that into a
;; variable, that contains the whole register map, with derived data added to
;; it, as well as expanding into a bunch of function and variable definitions,
;; that make up the Level-1 API for the chip in question.
;;
;; It actually doesn't do a lot of that work itself, it just dispatches to
;; ‘expand-definitions’ (for the code generation bits) and ‘expand-register’
;; for the additional information for the register-map variable.
(define-syntax define-register-map
  (lambda (x)
    (syntax-case x ()
      ((kw name register ...)
       (with-syntax ((var-name (datum->syntax
                                #'kw (symbol-append (syntax->datum #'name)
                                                    '-register-map))))
         #'(begin
             (expand-definitions register ...)
             (define-public var-name
               (list (expand-register register) ...))))))))

(define (register-default regmap address)
  (let ((reg (assoc address regmap)))
    (if (not reg)
        (throw 'cr-no-such-register address)
        (let ((value (assq 'default-value (cdr reg))))
          (if value
              (cdr value)
              0)))))

(define (map-across fnc what regmap)
  (fold (lambda (x acc)
          (let ((address (car x))
                (item (assq what (cdr x))))
            (if item (cons (fnc address (cdr item)) acc))))
        '()
        regmap))
