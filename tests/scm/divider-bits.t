(use-modules (ice-9 format)
             (ti cdce72010-tables))

(define cases
  ;; input   expected-output
  '(( 1  #b0100000)
    ( 2  #b1000000)
    ( 4  #b1000010)
    ( 6  #b0000001)
    (16  #b0000110)
    (20  #b0000111)
    (40  #b0001111)
    (80  #b0011111)))

(let next ((c cases))
  (cond ((null? c)
         (quit 0))
        (else
         (let ((got (get-bits-for-divider (caar c)))
               (exp (cadar c))
               (div (caar c)))
           (cond
            ((not (= exp got))
             (display (format "div(~d), exp: ~s, got: ~s.\n"
                              div
                              (number->string exp 2)
                              (number->string got 2)))
             (quit 1))
            (else
             (next (cdr c))))))))

(quit 0)
