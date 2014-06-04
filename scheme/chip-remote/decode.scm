;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote decode)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote bit-decoders)
  #:export (decode))

(define (decode-content value content)
  (let* ((name (car content))
         (offset (cadr content))
         (width (caddr content))
         (extract (cadddr content))
         (bits (extract value))
         (decoder* (list-ref content 5))
         (decoder (caddr decoder*))
         (decoder-type (cadr decoder*))
         (decoder-unit (cdr (cadddr decoder*)))
         (decoded (if (eq? decoder-type 'list)
                      (reverse-lookup (decoder) bits)
                      (decoder name offset width bits))))
    (list name
          (cons 'decoded decoded)
          (cons 'bits bits)
          (cons 'offset offset)
          (cons 'width width)
          (cons 'unit (cond ((and (thunk? decoder)
                                  (eq? decoded 'undefined))
                             ;; The values was retrieved by `reverse-lookup',
                             ;; but that failed. No unit then.
                             #f)
                            ((thunk? decoder-unit)
                             (decoder-unit))
                            ((procedure? decoder-unit)
                             (decoder-unit `((name . ,name)
                                             (raw . ,bits)
                                             (decoder . ,decoder)
                                             (decoded . ,decoded))))
                            (else decoder-unit))))))

(define (decode regmap address value)
  (let ((reg (assoc address regmap)))
    (if (not reg)
        (throw 'cr-no-such-register address)
        (map (lambda (x)
               (decode-content value x))
             (assq-ref (cdr reg) 'contents)))))
