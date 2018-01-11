;; Copyright (c) 2014-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote decode to-text)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote bit-decoders)
  #:export (decode->text
            display-list
            register->text
            registers->text
            text-decode-header))

(define* (no-wrap colour string #:key (bold? #f)) string)

(define* (wrap-in colour string #:key (bold? #f))
  (let ((in (esc-seq (if bold?
                         bold-seq
                         "")
                     (assq-ref colour-map colour)))
        (out (esc-seq (assq-ref colour-map 'reset))))
    (cat in string out)))

(define (attach-unit v u)
  (let ((unit (if (procedure? u) (u v) u)))
    (if unit
        (format #f "~a~a" v unit)
        v)))

(define (format-with-prefix prefix item unit colour?)
  (let ((w (if colour? wrap-in no-wrap)))
    (format #f
            "~a~a => ~a"
            prefix
            (w 'cyan (scalar->text (car item) colour? #f))
            (scalar->text (attach-unit (cdr item) unit) colour? #t))))

(define (format-first item unit colour?)
  (format-with-prefix "    Decoded: " item unit colour?))

(define (format-other item unit colour?)
  (format-with-prefix "             " item unit colour?))

(define (scalar->text item colour? wrap?)
  (let ((w (if (and colour? wrap?) wrap-in no-wrap)))
    (cond ((boolean? item)
           (if item
               (w 'green "enabled")
               (w 'red "disabled")))
          ((symbol? item)
           (w 'magenta (symbol->string item)))
          (else (w 'magenta (format #f "~a" item))))))

(define (decoded-value->text value unit colour?)
  (cond ((list? value)
         (let next ((v value) (first #t))
           (cond ((null? v)
                  '())
                 (first (cons (format-first (car v) unit colour?)
                              (next (cdr v) #f)))
                 (else (cons (format-other (car v) unit colour?)
                             (next (cdr v) #f))))))
        (else (list (format #f "    Decoded: ~a"
                            (scalar->text (attach-unit value unit)
                                          colour?
                                          #t))))))

(define (pp-raw v w wrap)
  (let ((hw (binwidth->hexwidth w)))
    (cat "[hex: 0x"
         (wrap 'green (format #f "~v,'0x" hw v) #:bold? #t)
         "] [bin: "
         (wrap 'green (format #f "~v,'0b" w v) #:bold? #t)
         "b]")))

(define (combined->text value colour?)
  (let ((w (if colour? wrap-in no-wrap))
        (name (cdar value))
        (raw (assq-ref value 'combined))
        (decoded (assq-ref value 'decoded))
        (sources (assq-ref value 'sources)))
    (define (pp-src-value x)
      (let* ((name (car x))
             (v (cadr x))
             (width (caddr x))
             (h-width (binwidth->hexwidth width))
             (addr (cadddr x)))
        (cat "   - "
             (w 'yellow (symbol->string (car x)))
             (if addr
                 (cat " (addr: 0x"
                      (w 'blue (number->string addr 16) #:bold? #t)
                      "): ")
                 ": ")
             (pp-raw v width w))))
    (flatten (list (cat (w 'red "Decoding combined value" #:bold? #t)
                        ": "
                        (w 'yellow (symbol->string name)))
                   (w 'white "  Sources:" #:bold? #t)
                   (map pp-src-value sources)
                   (cat (w 'magenta "  Combined")
                        ": "
                        (pp-raw (car raw) (cadr raw) w))
                   (cat (w 'white "  Decoded" #:bold? #t)
                        ": "
                        (scalar->text decoded colour? #t))))))

(define (dependent->text value colour?)
  (let ((w (if colour? wrap-in no-wrap))
        (name (cdar value))
        (addr (assq-ref value 'address))
        (raw (assq-ref value 'raw))
        (decoded (assq-ref value 'decoded))
        (deps (assq-ref value 'dependencies))
        (dep-raw (assq-ref value 'dep-raw))
        (dep-decoded (assq-ref value 'dep-decoded)))
    (define (pp-dep-value dep)
      (let* ((name (car dep))
             (addr (cdr dep))
             (r (assq-ref dep-raw name))
             (d (assq-ref dep-decoded name))
             (v (car r))
             (width (cadr r))
             (h-width (binwidth->hexwidth width)))
        (cat "   - "
             (w 'yellow (symbol->string name))
             (if addr
                 (cat " (addr: 0x"
                      (w 'blue (number->string addr 16) #:bold? #t)
                      "): ")
                 ": ")
             (scalar->text d colour? #t)
             " "
             (pp-raw v width w))))
    (flatten (list (cat (w 'red "Decoding dependent value" #:bold? #t)
                        ": "
                        (w 'yellow (symbol->string name))
                        (if addr
                            (cat " (addr: 0x"
                                 (w 'blue (number->string addr 16) #:bold? #t)
                                 ")")
                            ""))
                   (w 'white "  Dependencies:" #:bold? #t)
                   (map pp-dep-value deps)
                   (cat "  Value: "
                        (pp-raw (car raw) (cadr raw) w))
                   (cat (w 'white "  Final decoding" #:bold? #t)
                        ": "
                        (scalar->text (if (list? decoded)
                                          (format #f "~a~a"
                                                  (car decoded)
                                                  (cadr decoded))
                                          decoded)
                                      colour? #t))))))

(define* (interconnection->text value
                                #:key
                                (colour? (isatty? (current-output-port))))
  (let ((type (caar value)))
    (cond ((eq? type 'combined-value)
           (combined->text value colour?))
          ((eq? type 'dependent-value)
           (dependent->text value colour?))
          (else (throw 'cr-unknown-interconnection-type type)))))

(define* (text-decode-header address value width
                             #:key
                             (colour? (isatty? (current-output-port))))
  (let ((hex-width (binwidth->hexwidth width))
        (w (if colour? wrap-in no-wrap)))
    (list
     (cat (w 'red "Decoding register at address" #:bold? #t)
          ": 0x"
          (w 'magenta (format #f "~x" address)))
     (cat
          "  (octal: o"
          (w 'magenta (format #f "~o" address))
          ", binary: "
          (w 'magenta (format #f "~b" address))
          "b, decimal: "
          (w 'magenta (format #f "~d" address))
          ")")
     (cat (w 'white "Value")
          ": [hex: 0x"
          (w 'green (format #f "~v,'0x" hex-width value) #:bold? #t)
          "] [bin: "
          (w 'green (format #f "~v,'0b" width value) #:bold? #t)
          "b]"))))

(define* (registers->text #:key
                          register-map
                          reader
                          decoder
                          width
                          (interconnections '())
                          (filter-predicate #f)
                          (colour? (isatty? (current-output-port))))
  (let-values (((v regs interconns)
                (decode-many #:register-map register-map
                             #:interconnections interconnections
                             #:filter-predicate filter-predicate
                             #:reader reader
                             #:decoder decoder)))
    (append! (map (lambda (x)
                    (append! (text-decode-header (caar x) (cadr x) width
                                                 #:colour? colour?)
                             (decode->text (cdar x) #:colour? colour?)))
                  (zip regs v))
             (if (null? interconns)
                 '()
                 (list (list "Processing interconnected registers...")))
             (map (lambda (x)
                    (interconnection->text x #:colour? colour?))
                  interconns))))

(define* (register->text #:key register-map address width value
                         (colour? (isatty? (current-output-port))))
  (append! (text-decode-header address value width #:colour? colour?)
           (decode->text (decode register-map address value) #:colour? colour?)))

(define (display-list lst)
  (length (map (lambda (x)
                 (display "  ")
                 (display x)
                 (newline))
               lst)))

(define* (decode->text decoded-data
                       #:key
                       (colour? (isatty? (current-output-port))))
  (let ((w (if colour? wrap-in no-wrap)))
    (concatenate!
     (map (lambda (x)
            (let* ((name (car x))
                   (data (cdr x))
                   (dec (assq-ref data 'decoded))
                   (bits (assq-ref data 'bits))
                   (off (assq-ref data 'offset))
                   (bw (assq-ref data 'width))
                   (hw (binwidth->hexwidth bw))
                   (unit (assq-ref data 'unit))
                   (hex-string (format #f "~v'0x" hw bits))
                   (bin-string (format #f "~v,'0b" bw bits)))
              (cons
               (format #f "  Field \"~a\":" (w 'yellow (symbol->string name)))
               (cons
                (format #f "    Value: [0x~a] [~ab] - width: ~a, offset: ~a"
                        (w 'green hex-string #:bold? #t)
                        (w 'green bin-string #:bold? #t)
                        (w 'blue (number->string bw 10) #:bold? #t)
                        (w 'blue (number->string off 10) #:bold? #t))
                (decoded-value->text dec unit colour?)))))
          decoded-data))))
