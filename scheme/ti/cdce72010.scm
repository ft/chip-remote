(define-module (ti cdce72010)
  :export (read-registers))

(use-modules (ti cdce-primitives))

(define (read-registers)
  (let ((a '()))
    (let loop ((i 0))
      (set! a (append a (list (cdce/get-register i))))
      (if (< i 12)
          (loop (+ i 1))))
    a))
