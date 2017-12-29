(define-module (chip-remote language)
  #:export (group
            group-context
            group-name
            group-mode
            group-predicate
            group-transformer
            scalar-group
            process-plist
            just-value
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

(define (just-value e)
  (when debug? (format #t "just-value, e: ~a~%" e))
  (syntax-case e ()
    ((key value) #'value)))

(define (default-transformer e)
  (when debug? (format #t "default-transformer, e: ~a~%" e))
  (syntax-case e ()
    ((kw exp) #'(cons kw exp))
    ((kw exp0 expn ...) #'(list kw exp0 expn ...))))

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

(define (assemble-group-syntax group chunk)
  (when debug? (format #t "assemble-group-syntax, chunk: ~a~%" chunk))
  (let ((tf (group-transformer group))
        (context (group-context group)))
    (when debug? (format #t "chunk is #f => group: ~a~%" group))
    (if (eq? context 'scalar)
        (tf (car (last-pair chunk)))
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
                ((@@ (ice-9 pretty-print) pretty-print) rv)
                rv))
          ((exp . rest) (loop #'rest (process-with groups state #'exp)))))))
