(use-modules (ice-9 format)
             (ti cdce72010-tables))

(load "divider-samples.scm")

(let next ((c divider-samples))
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
