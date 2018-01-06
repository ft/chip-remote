;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote interpreter)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:export (cr-eval
            make-evaluation
            evaluation?
            evaluation-name
            evaluation-expression
            evaluation-value
            define-evaluation))

;; Sometimes, an item in a register has a simple arithmetic relation to its
;; semantic value. This interpreter can be used to express these relations.
;;
;; The reason, we did not opt to just use a scheme procedure to express these
;; relations, is that it is much harder to compile all of scheme to other
;; languages, like C or C++. This very minimal language is easy to compile to
;; something else, and it is almost trivial to interpret in our executing
;; Scheme environment.

(define (default-environment x)
  (error (format #f "Unknown variable: ~s" x)))

(define (cr-eval expression)
  (cr-eval* expression default-environment))

(define (cr-eval* expression environment)
  (let ((just-eval (lambda (e) (cr-eval* e environment))))
    (match expression
      ((? number? n) n)
      (('increment e1) (+ 1 (just-eval e1)))
      (('increment e1 e2) (+ (just-eval e1) (just-eval e2)))
      (('decrement e1) (- (just-eval e1) 1))
      (('decrement e1 e2) (- (just-eval e1) (just-eval e2)))
      (('multiply e1 e2) (* (just-eval e1) (just-eval e2)))
      (('integer-divide e1 e2) (truncate (/ (just-eval e1)
                                            (just-eval e2))))
      (('left-shift target how-much)
       (ash (just-eval target) (just-eval how-much)))
      (('right-shift target how-much)
       (ash (just-eval target) (* -1 (just-eval how-much))))
      ((e1 'greater-than? e2) (> (just-eval e1) (just-eval e2)))
      ((e1 'less-than? e2) (< (just-eval e1) (just-eval e2)))
      ((e1 'equal-to? e2) (= (just-eval e1) (just-eval e2)))
      (('if condition consequence alternative)
       (if (just-eval condition)
           (just-eval consequence)
           (just-eval alternative)))
      ((? symbol? b) (environment b))
      ;; This interpreter supports functions with one or two arguments. This
      ;; can likely be implemented more elegantly. But I won't bother for now.
      (('lambda ((? symbol? x)) body)
       (lambda (arg)
         (cr-eval* body (lambda (y)
                          (if (eq? x y)
                              arg
                              (environment y))))))
      (('lambda ((? symbol? a) (? symbol? b)) body)
       (lambda (arg1 arg2)
         (cr-eval* body (lambda (x)
                          (cond ((eq? x a) arg1)
                                ((eq? x b) arg2)
                                (else (environment x)))))))
      ((rator rand) ((just-eval rator) (just-eval rand)))
      ((rator rand1 rand2) ((just-eval rator)
                            (just-eval rand1)
                            (just-eval rand2)))
      (_ (error (format #f "Invalid expression: ~s~%" expression))))))

(define-immutable-record-type <evaluation>
  (make-evaluation* name expression value)
  evaluation?
  (name evaluation-name identify-evaluation)
  (expression evaluation-expression)
  (value evaluation-value))

(define (make-evaluation e)
  (make-evaluation* #f e (cr-eval e)))

(define-syntax-rule (define-evaluation binding e)
  (define binding (identify-evaluation (make-evaluation e) 'binding)))
