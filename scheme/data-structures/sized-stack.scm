;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (data-structures sized-stack)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-sized-stack
            sized-stack?
            sized-stack-empty?
            sized-stack-full?
            sized-stack-slots
            sized-stack-used
            sized-stack-free
            sized-stack-peek
            sized-stack-memory
            sized-stack-load
            sized-stack-push
            sized-stack-pop
            sized-stack-drop))

(define-immutable-record-type <sized-stack>
  (make-sized-stack* slots used memory)
  sized-stack?
  (slots sized-stack-slots)
  (used sized-stack-used update-sized-stack-used)
  (memory sized-stack-memory update-sized-stack-memory))

(define* (make-sized-stack n #:key (preload '()))
  (let* ((m (length preload))
         (k (min m n)))
    (make-sized-stack* n k
                       (if (< n m)
                           (take preload n)
                           preload))))

(define (sized-stack-free stack)
  (- (sized-stack-slots stack)
     (sized-stack-used stack)))

(define (sized-stack-peek stack)
  (car (sized-stack-memory stack)))

(define (sized-stack-overflow? stack)
  (> (sized-stack-used stack)
     (sized-stack-slots stack)))

(define (sized-stack-full? stack)
  (= (sized-stack-slots stack)
     (sized-stack-used stack)))

(define (sized-stack-empty? stack)
  (zero? (sized-stack-used stack)))

(define (sized-stack-set stack lst n)
  (set-fields stack
              ((sized-stack-memory) lst)
              ((sized-stack-used) n)))

(define (sized-stack-trim stack)
  (sized-stack-set stack
                   (take (sized-stack-memory stack)
                         (sized-stack-slots stack))
                   (sized-stack-slots stack)))

(define (sized-do-and-trim proc stack obj)
  (let ((candidate (proc stack obj)))
    (if (sized-stack-overflow? candidate)
        (sized-stack-trim candidate)
        candidate)))

(define (sized-stack-push* stack e)
  (sized-stack-set stack
                   (cons e (sized-stack-memory stack))
                   (1+ (sized-stack-used stack))))

(define (sized-stack-load* stack lst)
  (sized-stack-set stack lst (length lst)))

(define (sized-stack-push stack e)
  (sized-do-and-trim sized-stack-push* stack e))

(define (sized-stack-load stack lst)
  (sized-do-and-trim sized-stack-load* stack lst))

(define (sized-stack-pop stack)
  (if (sized-stack-empty? stack)
      (throw 'pop-from-empty-stack stack)
      (cons (sized-stack-peek stack)
            (sized-stack-set stack
                             (cdr (sized-stack-memory stack))
                             (1- (sized-stack-used stack))))))

(define (sized-stack-drop stack)
  (cdr (sized-stack-pop stack)))
