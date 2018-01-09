;; -*- scheme -*-

;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote process-plist))

(primitive-load "tests/test-tap-cfg.scm")
(define ~ syntax->datum)
(force-import (chip-remote item) semantics-group)

(with-fs-test-bundle
 (plan 5)

 (define-test "No groups means no output"
   (pass-if-equal? (~ (process-plist #'(#:foo bar #:baz boz beast #:thing fish)))
                   '()))

 (define-test "Simple catch-all group works"
   (pass-if-equal? (~ (process-plist #'(#:foo bar #:baz boz #:thing fish)
                                     (group 'misc)))
                   '(((#:foo bar)
                      (#:baz boz)
                      (#:thing fish)))))

 (define-test "List-type group + catch-all (single-type) group"
   (pass-if-equal? (~ (process-plist #'(#:foo bar
                                              #:fun stuff here
                                              #:baz boz
                                              #:fun more as-well
                                              #:thing fish)
                                     (group 'fun
                                            #:type 'list
                                            #:transformer just-value
                                            #:predicate (lambda (kw)
                                                          (eq? kw #:fun)))
                                     (group 'misc)))
                   '(((stuff here) (more as-well))
                     ((#:foo bar)
                      (#:baz boz)
                      (#:thing fish)))))

 (define-test "Scalar context works"
   (pass-if-equal? (~ (process-plist #'(#:foo bar #:baz boz #:foo real #:thing fish)
                                     (scalar-group 'foo)
                                     (scalar-group 'baz)
                                     (group 'misc)))
                   '(real boz ((#:thing fish)))))

 (define-test "Semantic group looks good"
   (pass-if-equal? (~ (process-plist #'(#:semantics lookup foo-table)
                                     semantics-group
                                     (group 'meta)))
                   '(((generate-semantics lookup foo-table))
                     ()))))
