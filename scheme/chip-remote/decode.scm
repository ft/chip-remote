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
         (decoder* (cdddr content))
         (decoder (cond ((null? decoder*)
                         literal-binary)
                        ((not (cdar decoder*))
                         (throw 'cr-undefined-decode-callback
                                (caar decoder*)))
                        (else (variable-ref (cdar decoder*)))))
         (getter (renamer (symbol-append 'get- name '-bits)))
         (extract* (module-variable (current-module) getter))
         (extract (if (not extract*)
                      (throw 'cr-unknown-getter getter)
                      (variable-ref extract*)))
         (bits (extract value)))
    (list name
          (cons 'decoded (cond ((list? decoder)
                                (reverse-lookup decoder bits))
                               ((procedure? decoder)
                                (decoder name offset width bits))
                               (else (throw 'cr-unsupported-decoder-at name))))
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
