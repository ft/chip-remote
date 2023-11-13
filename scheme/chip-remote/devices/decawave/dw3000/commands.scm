;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices decawave dw3000 commands)
  #:export (short-commands
            short-code->symbol
            short-symbol->code))

(define short-commands
  '((txrxoff                     . #x00)
    (tx                          . #x01)
    (rx                          . #x02)
    (delayed-tx                  . #x03)
    (delayed-rx                  . #x04)
    (delayed-tx+tts              . #x05)
    (delayed-rx+tts              . #x06)
    (delayed-tx+rts              . #x07)
    (delayed-rx+rts              . #x08)
    (delayed-tx+ref              . #x09)
    (delayed-rx+ref              . #x0a)
    (tx-if-no-preamble           . #x0b)
    (tx&enable-rx                . #x0c)
    (delayed-tx&enable-rx        . #x0d)
    (delayed-tx+tts&enable-rx    . #x0e)
    (delayed-tx+rts&enable-rx    . #x0f)
    (delayed-tx+ref&enable-rx    . #x10)
    (tx-if-no-preamble&enable-rx . #x11)
    (clear-all-irqs              . #x12)
    (double-buffer-toggle        . #x13)))

(define (short-symbol->code sym)
  (assq-ref short-commands sym))

(define (short-code->symbol code)
  (assv-ref (map (lambda (x) (cons (cdr x) (car x))) short-commands) code))
