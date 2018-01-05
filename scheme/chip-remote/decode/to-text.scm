;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; This module implements rendering decoded register data to plain text and
;; unix terminals (on which output colourisation is supported).

(define-module (chip-remote decode to-text)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote bit-decoders)
  #:use-module (chip-remote manufacturer)
  #:use-module (chip-remote decode)
  #:use-module (chip-remote decode types)
  #:use-module (chip-remote item)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote device)
  #:export (decode->text
            display-list
            register->text
            registers->text
            text-decode-header
            wat))

(define debug? #f)

(define-syntax cat
  (lambda (x)
    (syntax-case x ()
      ((_ exp ...) #'(string-concatenate (list exp ...))))))

(define (flatten lst)
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append (flatten (car lst))
                      (flatten (cdr lst))))))

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

(define* (make-highlighter colour #:key (bold? #f))
  (lambda (str)
    (let ((in (esc-seq (if bold?
                           bold-seq
                           "")
                       (assq-ref colour-map colour)))
          (out (esc-seq (assq-ref colour-map 'reset))))
      (cat in str out))))

(define* (no-wrap colour string #:key (bold? #f)) string)

(define (display-list lst)
  (length (map (lambda (x)
                 (display "  ")
                 (display x)
                 (newline))
               lst)))

(define (ld2 n)
  (/ (log10 n) (log10 2)))

(define (width->digits base width)
  (let* ((digits (/ width (ld2 base)))
         (int (inexact->exact (truncate digits))))
    (if (integer? digits)
        int
        (+ 1 int))))

(define (number->string/width n base width)
  (let* ((digits (width->digits base width))
         (str (number->string (logand n (one-bits width)) base))
         (strlen (string-length str)))
    (if (<= digits strlen)
        str
        (cat (make-string (- digits strlen) #\0) str))))

(define make-wrapper
  (case-lambda
    ((delim) (lambda (str) (cat delim str delim)))
    ((open close) (lambda (str) (cat open str close)))))

(define (maybe-with-base? base x)
  (or (eq? x #t)
      (and (eq? x 'maybe)
           (not (= base 10)))))

(define* (number->terminal n
                           #:key
                           (base 10)
                           (width #f)
                           (zeropad? 'maybe)
                           (decorate? 'maybe)
                           (prefix? #f)
                           (highlight identity)
                           (enclose identity))
  (define (prefix active? base str)
    (if (not (maybe-with-base? base active?)) str
        (case base
          ((2) (cat "binary: " str))
          ((8) (cat "octal: " str))
          ((10) (cat "decimal: " str))
          ((16) (cat "hex: " str))
          (else (cat "base-" (number->string base) ": " str)))))
  (define (decorate active? base str)
    (if (not (maybe-with-base? base active?)) str
        (case base
          ((2) (cat str "b"))
          ((8) (cat str "o"))
          ((10) (cat str "d"))
          ((16) (cat "0x" str))
          (else (cat (number->string base) "#" str)))))
  (enclose
   (prefix prefix? base
           (decorate decorate? base
                     (highlight (if (and width (maybe-with-base? base zeropad?))
                                    (number->string/width n base width)
                                    (number->string n base)))))))

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

(define double-quote (make-wrapper "\"" "\""))
(define square-backets (make-wrapper "[" "]"))

(define (maybe-string x)
  (format #f "~a" x))

(define (make-indent n)
  (make-string n #\space))

(define d:item:highlight:name (make-highlighter 'yellow))
(define d:item:highlight:decoded (make-highlighter 'red #:bold? #t))
(define d:regmap:highlight:name (make-highlighter 'red))
(define d:pagemap:highlight:name (make-highlighter 'white #:bold? #t))
(define d:device:highlight:title (make-highlighter 'white #:bold? #t))
(define d:device:highlight:name (make-highlighter 'cyan))
(define d:device:highlight:key (make-highlighter 'white))
(define d:device:highlight:value identity)

(define (d:reg:num b v w)
  (number->terminal v #:base b #:width w #:prefix? #t
                    #:enclose square-backets
                    #:highlight (make-highlighter 'green #:bold? #t)))

(define (d:reg:hex v w) (d:reg:num 16 v w))
(define (d:reg:octal v w) (d:reg:num 8 v w))
(define (d:reg:binary v w) (d:reg:num 2 v w))
(define (d:reg:decimal v w) (d:reg:num 10 v w))

(define (d:register-value r indent)
  (let* ((v (decoder-register-raw r))
         (w (register-width (decoder-register-description r)))
         (title "Raw Register Value: ")
         (tl (string-length title))
         (more (make-indent (+ tl indent))))
    (list (cat (make-indent indent)
               title
               (d:reg:decimal v w))
          (cat more
               (d:reg:hex v w))
          (cat more
               (d:reg:octal v w))
          (cat more
               (d:reg:binary v w)))))

(define (d:item:num b v w)
  (number->terminal v #:base b #:width w #:prefix? #f #:decorate? #t
                    #:enclose square-backets
                    #:highlight (make-highlighter 'green #:bold? #t)))

(define (d:item:hex v w) (d:item:num 16 v w))
(define (d:item:octal v w) (d:item:num 8 v w))
(define (d:item:binary v w) (d:item:num 2 v w))
(define (d:item:decimal v w) (d:item:num 10 v w))
(define d:item:offset+width (make-highlighter 'blue #:bold? #t))

(define (d:item-raw-value item indent)
  (let* ((i (decoder-item-description item))
         (v ((item-get i) (decoder-item-raw item)))
         (w (item-width i))
         (o (item-offset i)))
    (cat (make-indent indent)
         "Value: "
         (d:item:decimal v w)
         " "
         (d:item:hex v w)
         " "
         (d:item:octal v w)
         " "
         (d:item:binary v w)
         " - width: "
         (d:item:offset+width (number->string w))
         ", offset: "
         (d:item:offset+width (number->string o)))))

(define (d:item-decoded-value item indent)
  (let ((v (decoder-item-decoded item)))
    (cat (make-indent indent)
         "Decoded: "
         (d:item:highlight:decoded (maybe-string v)))))

(define (d:item-value item indent)
  (list (cat (make-indent indent)
             "Item "
             (double-quote
              (d:item:highlight:name
               (maybe-string (item-name (decoder-item-description item)))))
             ":")
        (d:item-raw-value item (+ 2 indent))
        (d:item-decoded-value item (+ 2 indent))))

(define (d:regmap:num b v)
  (number->terminal v #:base b #:prefix? #f #:decorate? #t #:zeropad? #f
                    #:highlight (make-highlighter 'magenta #:bold? #t)))

(define (d:regmap:hex v) (d:regmap:num 16 v))
(define (d:regmap:octal v) (d:regmap:num 8 v))
(define (d:regmap:binary v) (d:regmap:num 2 v))
(define (d:regmap:decimal v) (d:regmap:num 10 v))

(define (d:register-map addr reg indent)
  (if (integer? addr)
      (cat (make-indent indent)
           (d:regmap:highlight:name "Decoding Register at address: ")
           (d:regmap:decimal addr)
           ", "
           (d:regmap:hex addr)
           ", "
           (d:regmap:octal addr)
           ", "
           (d:regmap:binary addr))
      (cat (make-indent indent)
           (d:regmap:highlight:name "Decoding Register: "))))

(define (d:pagemap:num b v)
  (number->terminal v #:base b #:prefix? #f #:decorate? #t #:zeropad? #f
                    #:highlight (make-highlighter 'magenta #:bold? #t)))

(define (d:pagemap:hex v) (d:pagemap:num 16 v))
(define (d:pagemap:octal v) (d:pagemap:num 8 v))
(define (d:pagemap:binary v) (d:pagemap:num 2 v))
(define (d:pagemap:decimal v) (d:pagemap:num 10 v))

(define (d:page-map addr rm indent)
  (if (integer? addr)
      (cat (make-indent indent)
           (d:pagemap:highlight:name "Decoding Register Map at address: ")
           (d:pagemap:decimal addr)
           ", "
           (d:pagemap:hex addr)
           ", "
           (d:pagemap:octal addr)
           ", "
           (d:pagemap:binary addr))
      (cat (make-indent indent)
           (d:pagemap:highlight:name "Decoding Register Map: "))))

(define-syntax-rule (thunk body* ...)
  (lambda () body* ...))

(define (maybe-add thing thunk)
  (if thing (thunk) '()))

(define (d:device:key/value k v indent)
  (cat (make-indent indent)
       (d:device:highlight:key (maybe-string k))
       ": "
       (d:device:highlight:value (maybe-string v))))

(define (d:device thing indent)
  (let* ((dev (decoder-device-description thing))
         (meta (device-meta dev))
         (name (assq-ref meta #:name))
         (man (assq-ref meta #:manufacturer))
         (man-name (and man (manufacturer-name man)))
         (man-hp (and man (manufacturer-homepage man)))
         (man-wp (and man (manufacturer-wikipedia man)))
         (hp (assq-ref meta #:homepage))
         (ds (assq-ref meta #:datasheet))
         (kw (assq-ref meta #:keywords)))
    (list (if name
              (cat (d:device:highlight:title "Decoding Device ")
                   (d:device:highlight:name (maybe-string name))
                   ": ")
              (d:device:highlight:title "Decoding Device:"))
          (if man
              (list
               (maybe-add man-name
                          (thunk (d:device:key/value "Manufacturer"
                                                     man-name indent)))
               (maybe-add man-hp
                          (thunk (d:device:key/value "Homepage"
                                                     man-hp (+ 2 indent))))
               (maybe-add man-wp
                          (thunk (d:device:key/value "Wikipedia"
                                                     man-wp (+ 2 indent)))))
              '())
          (maybe-add hp (thunk (d:device:key/value "Device Page" hp indent)))
          (maybe-add ds (thunk (d:device:key/value "Datasheet" ds indent)))
          (maybe-add kw (thunk (d:device:key/value "Keywords" kw indent))))))

(define* (trace-?? name indent thing #:key (force? #f))
  (when (or debug? force?)
    (format #t "debug(~a, ~a):~%~a~%" name indent thing)))

(define (??+ thing indent)
  (?? thing #:indent (+ 2 indent)))

(define* (?? thing #:key (indent 0))
  (cond ((decoder-item? thing)
         (trace-?? 'item indent thing)
         (d:item-value thing indent))

        ((decoder-register? thing)
         (trace-?? 'register indent thing)
         (list (d:register-value thing indent)
               (??+ (decoder-register-items thing) indent)))

        ((decoder-register-map? thing)
         (trace-?? 'register-map indent thing)
         (map (lambda (reg)
                (list (d:register-map (car reg) (cdr reg) indent)
                      (??+ (cdr reg) indent)))
              (decoder-register-map-registers thing)))

        ((decoder-page-map? thing)
         (trace-?? 'page-map indent thing)
         (map (lambda (rm)
                (list (d:page-map (car rm) (cdr rm) indent)
                      (??+ (cdr rm) indent)))
              (decoder-page-map-register-maps thing)))

        ((decoder-device? thing)
         (trace-?? 'device indent thing)
         (list (d:device thing indent)
               (??+ (decoder-device-page-map thing) indent)))

        ((list? thing)
         (trace-?? 'list indent thing)
         (map (lambda (x) (?? x #:indent indent)) thing))

        ((pair? thing)
         (trace-?? 'pair indent thing)
         (cons (?? (car thing) #:indent indent)
               (?? (cdr thing) #:indent indent)))

        (else (trace-?? 'else indent thing)
              #:ignore)))

(define (wat description value)
  (map (lambda (s)
         (unless (eq? s #:ignore) (display s)
                 (newline)))
       (flatten (?? (decode* description value))))
  #t)
