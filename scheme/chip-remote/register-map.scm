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
;;   - regaddr:m-divider
;;   - regaddr:n-divider
;;   - regaddr:reference-divider
;;   - regaddr:power-save

(define-module (chip-remote register-map)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:export (define-register-interconns
            define-register-map
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
       #'(expand-annotation name #f))
      ((_ name unit-information)
       #'(list (if (list? 'name)
                   (last 'name)
                   (quote name))
               (cond ((procedure? name) 'function)
                     ((list? name) 'list)
                     (else (throw 'cr-unsupported-decoder-at name)))
               (if (procedure? name)
                   name
                   (lambda () name))
               (cons 'unit unit-information))))))

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
      ((kw (name offset width => annotation ...))
       (with-syntax ((sname (datum->syntax
                             #'kw (symbol-append 'set-
                                                 (syntax->datum #'name)
                                                 '-bits)))
                     (gname (datum->syntax
                             #'kw (symbol-append 'get-
                                                 (syntax->datum #'name)
                                                 '-bits))))
         #'(list 'name offset width gname sname
                 (expand-annotation annotation ...))))
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
                            #'kw (symbol-append 'regaddr:
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

;; The following part implements the ‘define-register-interconns’ macro.

;; This is a little helper for the clause-handler functions below.
(define (need data raw kw)
  (if (not (memq kw raw))
      (syntax-violation 'define-register-interconns
                        (format #f "Mandatory keyword missing: ~a" kw)
                        data)))

;; The (combine ...) handler. Except for #:logic, which defaults to ‘word’, all
;; arguments are mandatory.
(define (parse-combine-args args)

  (define defaults '((#:logic . 'word)))

  (define (check data opts)
    (let ((raw (syntax->datum data)))
      (need data raw #:combine)
      (need data raw #:finally)
      (need data raw #:into)
      (need data raw #:split)
      (let loop ((rest opts))
        (if (null? rest)
            data
            (let ((cur (car rest)))
              (cons (datum->syntax (car args) cur)
                    (cons (datum->syntax (car args)
                                         (cdr (assq cur defaults)))
                          (loop (cdr rest)))))))))

  (let loop ((in args)
             (out '())
             (opts '(#:logic)))
    (syntax-case in ()
      (()
       ;; Wrap the whole thing into a (list ...) expression, that the rest of
       ;; the final expression can be consed onto in ‘expand-clause’.
       (cons #'list (check out opts)))
      ((#:into arg . args)
       (loop #'args (cons #:into (cons #''arg out)) opts))
      ((#:logic arg . args)
       (loop #'args (cons #:logic (cons #''arg out)) (delq #:logic opts)))
      ((#:split arg . args)
       (loop #'args (cons #:split (cons #'arg out)) opts))
      ((#:combine arg . args)
       (loop #'args (cons #:combine (cons #'arg out)) opts))
      ((#:finally arg . args)
       (loop #'args (cons #:finally (cons #'arg out)) opts))
      ((kw arg . args)
       (syntax-violation 'define-register-interconns
                         "Unknown keyword or malformed argument"
                         #'kw
                         #'arg)))))

;; The (depends ...) handler. The #:on keyword can have a list and a single
;; symbol argument. This function turns the latter into the former. Both #:on
;; and #:finally are mandatory arguments.
(define (parse-depends-args args)

  (define (check data)
    (let ((raw (syntax->datum data)))
      (need data raw #:on)
      (need data raw #:finally))
    data)

  (let loop ((in args) (out '()))
    (syntax-case in ()
      (()
       (cons #'list (check out)))
      ((#:on (d ...) . args)
       (loop #'args (cons #:on (cons #'(list 'd ...) out))))
      ((#:on d . args)
       (loop #'args (cons #:on (cons #'(list 'd) out))))
      ((#:finally arg . args)
       (loop #'args (cons #:finally (cons #'arg out))))
      ((kw arg . args)
       (syntax-violation 'define-register-interconns
                         "Unknown keyword or malformed argument"
                         #'kw
                         #'arg)))))

;; This handles all clauses in the ‘define-register-interconns’ macro-call.
;; Each clause has its own handler function, that iterates through the clause
;; arguments to makes sure everything is in order.
(define-syntax expand-clause
  (lambda (x)
    (syntax-case x (combine depends)
      ((_ (combine (source ...) arg ...))
       (with-syntax ((parsed-args (parse-combine-args #'(arg ...))))
         #'(cons 'combine
                 (cons #:sources
                       (cons (list 'source ...) parsed-args)))))
      ((_ (depends target arg ...))
       (with-syntax ((parsed-args (parse-depends-args #'(arg ...))))
         #'(cons 'depends
                 (cons #:target
                       (cons 'target parsed-args))))))))

(define (sort-clauses unsorted)
  ;; First sort all ‘depends’ clauses to the back of the list and then sort the
  ;; ‘depends’ clauses so that all dependencies resolve incrementally. That way
  ;; the decoding code doesn't have to perform a lot of work resolving
  ;; dependencies.
  (define (get-target x)
    (cadr (memq #:target x)))

  (define (get-deps x)
    (cadr (memq #:on x)))

  (define (deps-are-done? deps done)
    (let loop ((d deps))
      (cond ((null? d) #t)
            ((not (memq (car d) done)) #f)
            (else (loop (cdr d))))))

  (define (filter-dependency-symbols lst)
    (map get-target (filter (lambda (x)
                              (eq? (car x) 'depends))
                            lst)))

  (define (find-candidate deps lst done)
    (let ((done-deps (filter-dependency-symbols done)))
      (let loop ((d lst))
        (if (null? d)
            (begin
              (format (current-error-port)
                      "define-register-interconns:~%")
              (format (current-error-port)
                      "  Could not find a candidate to eliminate!~%")
              (format (current-error-port)
                      "  Possible circular dependency?~%")
              (format (current-error-port)
                      "  Done dependencies: ~a~%" done-deps)
              #f)
            (let* ((c (car d))
                   (real-deps (filter (lambda (x) (memq x deps))
                                      (get-deps c))))
              (if (deps-are-done? real-deps done-deps)
                  c
                  (loop (cdr d))))))))

  (define (resolve-deps deps remaining done)
    (let ((candidate (find-candidate deps remaining done)))
      (if candidate
          (values (delete! candidate remaining) (cons candidate done))
          (values remaining done))))

  (let ((deps (filter-dependency-symbols unsorted)))
    (let loop ((in (sort unsorted
                         (lambda (a b)
                           (if (eq? (car a) 'depends) #f #t))))
               (out '()))
      (cond ((null? in) (reverse! out))
            ((eq? (caar in) 'combine)
             (loop (cdr in) (cons (car in) out)))
            (else (let-values (((new-in new-out) (resolve-deps deps in out)))
                    (if (and (equal? new-in in)
                             (equal? new-out out))
                        (begin (format (current-error-port)
                                       "  Remaining dependencies: ~%    ~a~%"
                                       in)
                               (throw 'cr-cannot-resolve-dependencies))
                        (loop new-in new-out))))))))

;; This is the main entry for this macro. It defines a variable
;; ‘<name>-register-interconnections’, dispatches every clause defined in the
;; macro-call off to ‘expand-clause’ and catches and sorts all the entries that
;; returns in a (list ...).
;;
;; XXX: Make sure that the expansion is done entirely at compile time, which it
;;      currently isn't.
(define-syntax define-register-interconns
  (lambda (x)
    (syntax-case x ()
      ((kw name clause ...)
       (with-syntax
           ((var-name (datum->syntax
                       #'kw (symbol-append (syntax->datum #'name)
                                           '-regmap-interconnections)))
            (clauses #'(sort-clauses (list (expand-clause clause) ...))))
         #'(define-public var-name clauses))))))
