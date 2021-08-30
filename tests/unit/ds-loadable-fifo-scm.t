;; -*- scheme -*-

;; Copyright (c) 2019 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (srfi srfi-1)
             (test tap)
             (data-structures loadable-fifo))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
    (plan 35)

  (define-test "make-fifo returns <loadable-fifo>"
    (pass-if-true (fifo? (make-fifo 32))))

  (let* ((f (make-fifo 32))
         (data '(a b c))
         (f* (fifo-append f data))
         (f** (fifo-enqueue f* 'new))
         (f*** (fifo-load f** '(1 2 3)))
         (df (fifo-dequeue f*))
         (d-elem (car df))
         (d-fifo (cdr df))
         (drained (fifo-drain f**))
         (drained-data (car drained))
         (drained-fifo (cdr drained))
         (taken (fifo-take f** 2))
         (taken-data (car taken))
         (taken-fifo (cdr taken)))

    (define-test "fifo-load loads fifo memory"
      (pass-if-equal? (fifo-memory f***) '(1 2 3)))
    (define-test "used slots in fifo is correct"
      (pass-if-= 3 (fifo-used f***)))
    (define-test "free slots in fifo is correct"
      (pass-if-= (- (fifo-slots f***) (fifo-used f***))
                 (fifo-free f***)))

    (define-test "fifo-append loads fifo memory"
      (pass-if-equal? (fifo-memory f*) data))
    (define-test "used slots in fifo is correct"
      (pass-if-= 3 (fifo-used f*)))
    (define-test "free slots in fifo is correct"
      (pass-if-= (- (fifo-slots f*) (fifo-used f*))
                 (fifo-free f*)))

    (define-test "fifo-enqueue appends to memory"
      (pass-if-equal? (fifo-memory f**)
                      (append data (list 'new))))
    (define-test "fifo-enqueue updates free correctly"
      (pass-if-= (fifo-free f**)
                 (1- (fifo-free f*))))
    (define-test "fifo-enqueue updates used correctly"
      (pass-if-= (fifo-used f**)
                 (1+ (fifo-used f*))))

    (define-test "fifo-dequeue returns car of previous memory"
      (pass-if-equal? d-elem (car data)))
    (define-test "fifo-dequeue returns correct memory"
      (pass-if-equal? (fifo-memory d-fifo) (cdr data)))
    (define-test "fifo-dequeue updates free correctly"
      (pass-if-= (fifo-free d-fifo)
                 (1+ (fifo-free f*))))
    (define-test "fifo-dequeue updates used correctly"
      (pass-if-= (fifo-used d-fifo)
                 (1- (fifo-used f*))))

    (define-test "fifo-drain fetches correct data"
      (pass-if-equal? drained-data (fifo-memory f**)))
    (define-test "fifo-drain returns empty fifo"
      (pass-if-equal? (fifo-memory drained-fifo) '()))
    (define-test "fifo-drain updates free correctly"
      (pass-if-= (fifo-free drained-fifo)
                 (fifo-slots f**)))
    (define-test "fifo-drain updates used correctly"
      (pass-if-= (fifo-used drained-fifo)
                 0))

    (define-test "fifo-take fetches correct data"
      (pass-if-equal? taken-data (take (fifo-memory f**) 2)))
    (define-test "fifo-take returns correctly loaded fifo"
      (pass-if-equal? (fifo-memory taken-fifo)
                      (drop (fifo-memory f**) 2)))
    (define-test "fifo-take updates free correctly"
      (pass-if-= (fifo-free taken-fifo)
                 (+ (fifo-free f**) 2)))
    (define-test "fifo-take updates used correctly"
      (pass-if-= (fifo-used taken-fifo)
                 (- (fifo-used f**) 2))))

  ;; make-fifo uses fifo-load so the latter is tested here as well.
  (define-test "cannot overflow fifo upon creation (throw)"
    (pass-if-exception 'fifo-overflow
                       (make-fifo 4 #:memory '(a b c d e))))
  (define-test "cannot overflow fifo upon creation (drop-old)"
    (pass-if-equal? (fifo-memory (make-fifo 4
                                            #:memory '(a b c d e f g h i)
                                            #:overflow 'drop-old))
                    '(f g h i)))
  (define-test "cannot overflow fifo upon creation (drop-new)"
    (pass-if-equal? (fifo-memory (make-fifo 4
                                            #:memory '(a b c d e f g h i)
                                            #:overflow 'drop-new))
                    '(a b c d)))

  (define-test "fifo-enqueue does not overflow (throw)"
    (pass-if-exception 'fifo-overflow
                       (fifo-enqueue (make-fifo 4 #:memory '(a b c d)) 'e)))
  (define-test "fifo-enqueue does not overflow (drop-old)"
    (pass-if-equal? (fifo-memory (fifo-enqueue (make-fifo 4
                                                          #:memory '(a b c d)
                                                          #:overflow 'drop-old)
                                               'e))
                    '(b c d e)))
  (define-test "fifo-enqueue does not overflow (drop-new)"
    (pass-if-equal? (fifo-memory (fifo-enqueue (make-fifo 4
                                                          #:memory '(a b c d)
                                                          #:overflow 'drop-new)
                                               'e))
                    '(a b c d)))

  (define-test "fifo-append does not overflow (throw)"
    (pass-if-exception 'fifo-overflow
                       (fifo-append (make-fifo 4 #:memory '(a b c d)) '(e f))))
  (define-test "fifo-append does not overflow (drop-old)"
    (pass-if-equal? (fifo-memory (fifo-append (make-fifo 4
                                                         #:memory '(a b c d)
                                                         #:overflow 'drop-old)
                                              '(e f g h)))
                    '(e f g h)))
  (define-test "fifo-append does not overflow (drop-new)"
    (pass-if-equal? (fifo-memory (fifo-append (make-fifo 4
                                                         #:memory '(a b c d)
                                                         #:overflow 'drop-new)
                                              '(e f g h)))
                    '(a b c d)))

  (define-test "fifo-dequeue does not underflow (throw)"
    (pass-if-exception 'fifo-underflow
                       (fifo-dequeue (make-fifo 4))))

  (define-test "fifo-dequeue always throws on underflow (ignore)"
    (pass-if-exception 'fifo-underflow
                       (fifo-dequeue (make-fifo 4 #:underflow 'ignore))))

  (define-test "fifo-take does not underflow (throw)"
    (pass-if-exception 'fifo-underflow
                       (fifo-take (make-fifo 4 #:memory '(a)) 2)))

  (define-test "fifo-dequeue does not underflow (ignore)"
    (pass-if-equal? (car (fifo-take (make-fifo 4 #:memory '(a) #:underflow 'ignore)
                                    2))
                    '(a))))
