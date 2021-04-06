;; Copyright (c) 2019-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (data-structures loadable-fifo)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-fifo
            ;; predicates
            fifo?
            fifo-empty?
            fifo-full?
            ;; inspection
            fifo-slots
            fifo-used
            fifo-free
            fifo-memory
            ;; modification
            fifo-load
            fifo-append
            fifo-enqueue
            fifo-dequeue
            fifo-drain
            fifo-take))

(define-immutable-record-type <loadable-fifo>
  (make-fifo* memory slots used free under over)
  fifo?
  (memory fifo-memory update-fifo-memory)
  (slots fifo-slots)
  (used fifo-used update-fifo-used)
  (free fifo-free update-fifo-free)
  (over fifo-overflow)
  (under fifo-underflow))

(define* (make-fifo slots
                    #:key
                    (memory '())
                    (overflow 'throw)
                    (underflow 'throw))
  (unless (member overflow '(throw drop-new drop-old))
    (throw 'invalid-overflow-policy overflow))
  (unless (member underflow '(throw ignore))
    (throw 'invalid-underflow-policy underflow))
  (let ((n (length memory))
        (fifo (make-fifo* '() slots 0 slots underflow overflow)))
    (fifo-load fifo memory)))

(define (load-memory fifo lst)
  (let ((n (length lst))
        (s (fifo-slots fifo)))
    (if (<= n s)
        lst
        (case (fifo-overflow fifo)
          ((drop-old) (drop lst (- n s)))
          ((drop-new) (take lst s))
          (else ;; throw
           (throw 'fifo-overflow fifo lst))))))

(define (fifo-load fifo lst)
  (let ((n (length lst))
        (s (fifo-slots fifo)))
    (set-fields fifo
                ((fifo-memory) (load-memory fifo lst))
                ((fifo-used) n)
                ((fifo-free) (- (fifo-slots fifo) n)))))

(define (fifo-append fifo lst)
  (let* ((new (load-memory fifo (append (fifo-memory fifo) lst)))
         (n (length new)))
    (set-fields fifo
                ((fifo-memory) new)
                ((fifo-used) n)
                ((fifo-free) (- (fifo-slots fifo) n)))))

(define (fifo-drain fifo)
  (cons (fifo-memory fifo)
        (set-fields fifo
                    ((fifo-memory) '())
                    ((fifo-used) 0)
                    ((fifo-free) (fifo-slots fifo)))))

(define (fifo-unload fifo n)
  (if (<= n (fifo-used fifo))
      (take (fifo-memory fifo) n)
      (if (eq? 'ignore (fifo-underflow fifo))
          (fifo-memory fifo)
          (throw 'fifo-underflow fifo n))))

(define (fifo-take fifo n)
  (let* ((rv (fifo-unload fifo n))
         (n (length rv)))
    (cons rv
          (set-fields fifo
                      ((fifo-memory) (drop (fifo-memory fifo) n))
                      ((fifo-used) (- (fifo-used fifo) n))
                      ((fifo-free) (+ (fifo-free fifo) n))))))

(define (fifo-enqueue fifo obj)
  (set-fields fifo
              ((fifo-memory) (load-memory fifo (append (fifo-memory fifo)
                                                       (list obj))))
              ((fifo-used) (1+ (fifo-used fifo)))
              ((fifo-free) (1- (fifo-free fifo)))))

(define (fifo-dequeue fifo)
  (let ((rv (fifo-take fifo 1)))
    (if (null? (car rv))
        (throw 'fifo-underflow fifo 1)
        (cons (caar rv) (cdr rv)))))

(define (fifo-empty? fifo)
  (zero? (fifo-used fifo)))

(define (fifo-full? fifo)
  (= (fifo-slots fifo) (fifo-used fifo)))
