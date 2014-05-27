;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote decode to-text)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote decode)
  #:export (decode->text
            display-list
            register->text
            text-decode-header))

(define-syntax cat
  (lambda (x)
    (syntax-case x ()
      ((_ exp ...) #'(string-concatenate (list exp ...))))))

(define colour-map '((black . "30")
                     (blue . "34")
                     (cyan . "36")
                     (default . "39")
                     (green . "32")
                     (magenta . "35")
                     (red . "31")
                     (reset . "00")
                     (white . "37")
                     (yellow . "33")))

(define bold-seq "01;")
(define esc-prefix (format #f "~c[" #\esc))
(define esc-suffix "m")

(define (esc-seq . args)
  (format #f "~a~a~a" esc-prefix (string-concatenate args) esc-suffix))

(define* (wrap-in colour string #:key (bold? #f))
  (let ((in (esc-seq (if bold?
                         bold-seq
                         "")
                     (assq-ref colour-map colour)))
        (out (esc-seq (assq-ref colour-map 'reset))))
    (cat in string out)))

(define* (no-wrap colour string #:key (bold? #f)) string)

(define (display-list lst)
  (length (map (lambda (x)
                 (display "  ")
                 (display x)
                 (newline))
               lst)))

(define (binwidth->hexwidth n)
  (let ((mod (modulo n 4)))
    (+ (floor (/ n 4)) (if (> mod 0) 1 0))))

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

(define* (register->text #:key register-map address width value
                         (colour? (isatty? (current-output-port))))
  (append! (text-decode-header address value width #:colour? colour?)
           (decode->text (decode register-map address value) #:colour? colour?)))
