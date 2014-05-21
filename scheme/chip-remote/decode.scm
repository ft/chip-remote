;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote decode)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote bit-decoders)
  #:export (decode))

(define (decode-content renamer value content)
  (let* ((name (car content))
         (offset (cadr content))
         (width (caddr content))
         (extract (cadddr content))
         (bits (extract value))
         (decoder* (list-ref content 5))
         (decoder (caddr decoder*))
         (decoder-type (cadr decoder*)))
    (list name
          (cons 'decoded (if (eq? decoder-type 'list)
                             (reverse-lookup (decoder) bits)
                             (decoder name offset width bits)))
          (cons 'bits bits)
          (cons 'offset offset)
          (cons 'width width))))

(define* (decode regmap address value #:key (renamer (lambda (x) x)))
  (let ((reg (assoc address regmap)))
    (if (not reg)
        (throw 'cr-no-such-register address)
        (map (lambda (x)
               (decode-content renamer value x))
             (assq-ref (cdr reg) 'contents)))))
