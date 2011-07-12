(define-module (ti cdce72010)
  :export (read-registers
           toggle-trace))

(use-modules (ti cdce-primitives))

(define (read-registers)
  (let ((a '()))
    (let loop ((i 0))
      (set! a (append a (list (cdce/read-register i))))
      (if (< i 12)
          (loop (+ i 1))))
    a))

(define (toggle-trace)
  (display "-!- ")
  (cond
   (cdce/options:trace
    (display "Dis")
    (set! cdce/options:trace #f))
   (else
    (display "En")
    (set! cdce/options:trace #t)))
  (display "abling serial communication trace.")
  (newline))
