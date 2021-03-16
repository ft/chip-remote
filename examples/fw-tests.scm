(use-modules (termios frontends)
             (termios system)
             (chip-remote decode)
             (chip-remote modify)
             (chip-remote io)
             (chip-remote protocol)
             (chip-remote commander)
             (chip-remote devices analog-devices adf4169))

(define *serial-device* "/dev/ttyACM2")
(termios-8n1 *serial-device* termios-B115200)

(define (run n op . a)
  (let loop ((n n))
    (unless (zero? n)
      (catch #t
        (lambda () (apply op a))
        (lambda (k . ka)
          (format #t "Caught exception ~a with arguments: ~a~%" k ka)))
      (loop (1- n)))))

(define c (make-cr-connection *serial-device*))
(io-open c)
;; (run 7 hi c)
;; (run 100 transmit c #x1004)
;; (run 7 bye c)

(io-opt/set 'trace #t)
(hi* c)

(define pll (make-commander #:device adf4169
                            #:connection c
                            #:decode decode
                            #:data (modify* adf4169 'ramp-enabled? #t)))

(pll 'transmit!)

;; (io-close c)
