(use-modules (chip-remote device)
             (chip-remote item)
             (chip-remote register)
             (chip-remote devices linear-technology ltc6603)
             (chip-remote devices decawave dw1000)
             (chip-remote pretty-print)
             (chip-remote semantics)
             (ice-9 match)
             (ice-9 pretty-print))

;;(pretty-print ltc6603)
;;(pretty-print dw1000)

(define* (pp-indent #:optional (kind 'default))
  (case kind
    ((default) 4)
    ((complex) 2)
    ((list)    1)
    (else      0)))

(define (pp-do-indent port indent)
  (let loop ((n indent))
    (unless (zero? n)
      (write-char #\space port)
      (loop (1- n)))))

(define (eval-indent port prg)
  (for-each (lambda (kind)
              (pp-do-indent port (pp-indent kind)))
            (if (list? prg)
                prg
                (list prg))))

(define (pp-eval* port prg last-op indent)
  (match prg
    (('wrap open close . rest)
     (format port "~a" open)
     (pp-eval* port rest last-op indent)
     (format port "~a~%" close)
     'wrap)

    (('indent kind . rest)
     (if (eq? last-op 'newline)
         (eval-indent port kind))
     (pp-eval* port rest last-op (cons kind indent))
     'indent)

    (('key key)
     (and (eq? last-op 'indent)
          (eval-indent port indent))
     (format port "~a:" key) 'key)
    
    (('space value)
     (format port " ~a" value)
     'space)
    
    (('type type)
     (format port "<~a>" type)
     'type)

    ((expr . rest)
     (pp-eval* port rest
               (pp-eval* port expr last-op indent)
               indent))

    (() last-op)

    ('newline
     (newline port)
     (eval-indent port indent)
     'newline)

    (else (format #t "not-handled-yet ~a~%" prg)
          'unknown-instruction)))

(define (pp-eval port prg)
  (pp-eval* port prg 'nop '()))

(define (dsl/pp-semantics sem)
  `(wrap "#<" ">"
         (type semantics) (newline)
         (indent complex
                 (key name) (space ,(semantics-name sem)) (newline)
                 (key type) (space ,(semantics-type sem)) (newline)
                 (key data) (space ,(semantics-data sem)) (newline)
                 (key encode) (space ,(semantics-encode sem)) (newline)
                 (key decode) (space ,(semantics-decode sem)))))

(pp-eval (current-output-port)
         (dsl/pp-semantics (item-semantics (device-address ltc6603 #f #f 0))))

;;(pp-eval (current-output-port)
;;         '(wrap "#<" ">"
;;                (type item) (newline)
;;                (indent complex
;;                        (key name) (space enable-output?) (newline)
;;                        (key offset) (space 0) (newline)
;;                        (key width) (space 1) (newline)
;;                        (key semantics) (newline)
;;                        (indent complex
;;                                (wrap "#<" ">"
;;                                      (type semantics) (newline)
;;                                      (indent complex
;;                                              (key name) (space #f) (newline)
;;                                              (key type) (space boolean) (newline)
;;                                              (key data) (space #f) (newline)
;;                                              (key encode) (space ...) (newline)
;;                                              (key decode) (space ...))))
;;                        (key access) (space ...) (newline)
;;                        (key meta) (space ()) (newline)
;;                        (key get) (space ...) (newline)
;;                        (key set) (space ...))))
