;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; In the full example in ‘cdce-read-all-registers.scm’ the ‘stty’ is used to
;; setup parameters of the serial link to a remote board, since Guile doesn't
;; have support for termios built-in.
;;
;; There is however <https://github.com/ft/guile-termios> which implements such
;; an interface. And this script uses that interface to setup these parameters.

(use-modules (termios)
             (termios system))

(define tty (open-io-file serial-device))
(define ts (make-termios-struct))

;; This is 8N1 with 19200bd symbol-rate.
(cf-make-raw! ts)
(cf-set-speed! ts termios-B19200)
(tc-set-attr tty ts)

(close-port tty)
