(define-module (chip-remote process-plist)
  #:export (group
            group-context
            group-name
            group-mode
            group-predicate
            group-transformer
            scalar-group
            process-plist
            just-value
            zip-syms
            is-kw?
            not-kw?))

(define debug? #f)

(define (not-kw? x)
  (not (keyword? (syntax->datum x))))

(define (is-kw? x)
  (keyword? (syntax->datum x)))

(define group-name car)
(define group-mode cadr)
(define group-predicate caddr)
(define group-transformer cadddr)
(define (group-context x) (cadr (cdddr x)))

(define* (group name
                #:key
                (type 'single) ;; 'list
                (predicate (lambda (x) #t))
                (transformer default-transformer)
                (context 'list)) ;; 'scalar
  "Define a group for processing in plist expressions.

In plists, keys are keywords and anything else is a value. In particular:

  (foobar #:thing   abc 123 def
          #:another bar
          #:finally even more stuff here)

Groups are ways to specify how these key/value(s) combinations should be parsed
into groups of data. These groups are commonly used in (with-syntax ...) forms
to transform plist-style languages to scheme function calls.

Groups match on keywords: The ‘#:predicate’ parameter should specify a function
that lets the group pick exactly the parts of the plist that it is supposed to.
The default here is a function that matches all keywords. This is useful to
swoop up every key value pair in a plist.

The ‘#:type’ parameter allows the user to control whether the group allows more
than one value to follow a keyword. This can be achieved be setting this
parameter to ‘list’. The default is ‘single’.

Another parameter, that must not be confused with the previous is ‘#:context’.
It allows to differentiate between ’list’ (which is the default) and ‘scalar’.
With ‘list’, multiple occurances of a group cause the different values to be
appended into a resulting list. With ‘scalar’ context, multiple occurances of
the same group cause later occurances to override prior ones.

Finally, the ’#:transformer’ parameter allows transformations on the parsed
group data.

Chip-remote's test-suite contains ‘process-plist-scm.t’, which has a number of
illustrative examples uses of ‘plist-process’ and ‘group’."
  (list name type predicate transformer context))

(define (scalar-group name)
  (group name
         #:context 'scalar
         #:predicate (lambda (x) (eq? x (symbol->keyword name)))
         #:transformer just-value))

(define (alist-change is-equal? alist key value)
  "Change an existing value in an association list.

IS-EQUAL? is a binary function used to compare keys of the association list
ALIST against KEY. If KEY is found in ALIST in terms of IS-EQUAL?, a new
association list is returned where the value of KEY is replaced with VALUE.

If KEY is not found in ALIST, #f is returned."
  (let loop ((rest alist) (acc '()))
    (if (null? rest)
        #f
        (let* ((this (car rest))
               (this-key (car this)))
          (if (is-equal? key this-key)
              (append acc (list (cons key value)) (cdr rest))
              (loop (cdr rest) (append acc (list this))))))))

(define (alist-add alist key value)
  "Add key KEY with value VALUE to the association list ALIST.

This function does *not* check whether KEY is part of ALIST, it
conconditionally adds the key/value pair."
  (cons (cons key value)
        alist))

(define (alist-change-or-add is-equal? alist key value)
  "Returns a new association list based on ALIST, where the value of KEY is
VALUE.

See `alist-change' for details about this function's arguments.

The difference between this and `alist-change' is, that in case KEY is not part
of ALIST, *this* function *adds* the key/value pair indicated by KEY and VALUE."
  (or (alist-change is-equal? alist key value)
      (alist-add alist key value)))

(define (set-mode state mode)
  (alist-change-or-add eq? state 'mode mode))

(define (set-delim state delim)
  (alist-change-or-add eq? state 'delim delim))

(define (set-chunk state chunk)
  (alist-change-or-add eq? state 'chunk chunk))

(define (set-group state group)
  (alist-change-or-add eq? state 'group group))

(define (set-chunk/append state chunk)
  (let ((current (or (assq-ref state 'chunk)
                     '())))
    (alist-change-or-add eq? state 'chunk
                         (append current (list chunk)))))

;; TODO: Boy, does this function need refactoring...
(define (save-chunk state)
  (let* ((saved (assq-ref state 'saved-chunks))
         (delim (assq-ref state 'delim))
         (value (assq-ref state 'chunk))
         (group (assq-ref state 'group))
         (gn (if group (group-name group) 'meta)))
    (alist-change-or-add eq? state 'saved-chunks
                         (if saved
                             (alist-change-or-add
                              eq? saved gn
                              (let* ((sc (assq-ref state 'saved-chunks))
                                     (current (assq-ref sc (group-name group)))
                                     (new (if (list? value)
                                              (cons delim value)
                                              (list delim value))))
                                (when debug?
                                  (format #t "save, current: ~a~%" current)
                                  (format #t "save, new: ~a~%" (list new)))
                                (if current
                                    (append current (list new))
                                    (list new))))
                             '()))))

(define (just-value e)
  (when debug? (format #t "just-value, e: ~a~%" e))
  (syntax-case e ()
    ((key value) #'value)
    ((key . value) #'value)))

(define (default-transformer e)
  (when debug? (format #t "default-transformer, e: ~a~%" e))
  (syntax-case e ()
    ((kw exp) #'(kw exp))
    ((kw exp0 expn ...) #'(kw exp0 expn ...))))

(define catch-all-fallback (group 'unmatched-fallback))

(define (find-active groups exp)
  (let loop ((rest groups))
    (if (null? rest)
        catch-all-fallback
        (let ((group (car rest)))
          (if ((group-predicate group) exp)
              group
              (loop (cdr rest)))))))

(define (lowlvl-process groups state exp mode group)
  (when debug? (format #t "lowlvl: group: ~a~%" group))
  (case mode
    ((init) (set-group (set-chunk (set-delim (set-mode (save-chunk state)
                                                       (group-mode group))
                                             exp)
                                  '())
                       group))
    ((single) (set-chunk (set-mode state 'init) exp))
    ((list) (set-chunk/append state exp))
    (else (throw 'process-plist-bug "Unknown mode" groups state exp))))

(define (process-with groups state exp)
  (let* ((mode (assq-ref state 'mode))
         (this (syntax->datum exp)))
    (if (or (eq? mode 'init)
            (and (eq? mode 'list)
                 (keyword? this)))
        (let ((group (find-active groups this)))
          (unless group
            (throw 'syntax-error "Unknown delimiter" groups state exp))
          (lowlvl-process groups state exp 'init group))
        (lowlvl-process groups state exp mode '()))))

(define (recurse-assembly tf exp acc)
  (when debug? (format #t "recurse-assembly, exp: ~a~%" exp))
  (syntax-case exp ()
    (() acc)
    (#f acc)
    ((e . rest) (begin
                  (when debug?
                    (format #t "recurse-assembly, e: ~a~%"
                            (syntax->datum #'e))
                    (format #t "recurse-assembly, tf: ~a~%"
                            (syntax->datum (tf #'e))))
                  (recurse-assembly tf #'rest #`(#,@acc #,(tf #'e)))))))

(define (scalar-transformer tf chunk)
  (if (and (list? chunk)
           (not (null? chunk)))
      (tf (car (last-pair chunk)))
      #'()))

(define (assemble-group-syntax group chunk)
  (when debug? (format #t "assemble-group-syntax, chunk: ~a~%" chunk))
  (let ((tf (group-transformer group))
        (context (group-context group)))
    (when debug? (format #t "chunk is ~a => group: ~a~%" chunk group))
    (if (eq? context 'scalar)
        (scalar-transformer tf chunk)
        (recurse-assembly tf chunk #'()))))

(define (groups->syntax groups state)
  (when debug?
    (format #t "(groups->syntax ...):~%")
    (format #t "*** Pretty state, BEGIN ***~%")
    ((@@ (ice-9 pretty-print) pretty-print) state)
    (format #t "*** Pretty state, END ***~%")
    (format #t "groups->syntax, groups: ~a~%" groups))
  (let* ((chunks (assq-ref state 'saved-chunks))
         (data (map (lambda (g)
                      (when debug?
                        (format #t "---> group: ~a~%" g)
                        (format #t "---> chunks: ~a~%" chunks)
                        (format #t "---> chunk: ~a~%"
                                (assq-ref chunks (group-name g))))
                      (assemble-group-syntax g (assq-ref chunks
                                                         (group-name g))))
                    groups)))
    (when debug?
      (format #t "(groups->syntax ...), data: ~a~%" (syntax->datum data)))
    data))

(define (process-plist exps . groups)
  (when debug? (format #t "process-plist,exps: ~a~%" exps))
  (if (or (null? exps)
          (null? groups))
      (datum->syntax #'process-plist (make-list (length groups) '()))
      (let loop ((exps exps) (state '((mode . init))))
        (syntax-case exps ()
          (() (let ((rv (groups->syntax groups
                                        (set-group
                                         (set-delim
                                          (set-mode
                                           (set-chunk
                                            (save-chunk state)
                                            '())
                                           'done)
                                          #f)
                                         #f))))
                (when debug? ((@@ (ice-9 pretty-print) pretty-print) rv))
                rv))
          ((exp . rest) (loop #'rest (process-with groups state #'exp)))))))

(define (zip-syms a b)
  (cond ((null? a) '())
        ((null? b) '())
        (else (cons (car a) (cons (car b) (zip-syms (cdr a) (cdr b)))))))
