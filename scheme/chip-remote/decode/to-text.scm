;; Copyright (c) 2014-2018 chip-remote workers, All rights reserved.
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
  #:use-module (chip-remote register-window)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote device)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:export (decode-to-text))

(define debug? #f)

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

(define* (make-highlighter colour #:key (bold? #f))
  (lambda (str)
    (let ((in (esc-seq (if bold?
                           bold-seq
                           "")
                       (assq-ref colour-map colour)))
          (out (esc-seq (assq-ref colour-map 'reset))))
      (cat in str out))))

(define (width->digits base width)
  (let* ((digits (/ width (log2 base)))
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

(define double-quote (make-wrapper "\"" "\""))
(define square-backets (make-wrapper "[" "]"))

(define (maybe-string x)
  (format #f "~a" (if (symbol? x)
                      (symbol->string x)
                      x)))

(define (make-indent n)
  (make-string n #\space))

(define d:item:highlight:name (make-highlighter 'yellow))
(define d:item:highlight:decoded (make-highlighter 'red #:bold? #t))
(define d:item:highlight:semantics (make-highlighter 'magenta #:bold? #t))
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

(define (d:register-like indent title tl v w more)
  (list (cat (make-indent indent)
             title
             (d:reg:decimal v w))
        (cat more
             (d:reg:hex v w))
        (cat more
             (d:reg:octal v w))
        (cat more
             (d:reg:binary v w))))

(define (d:register-value r indent)
  (let* ((v (decoder-register-raw r))
         (w (register-width (decoder-register-description r)))
         (title "Raw Register Value: ")
         (tl (string-length title))
         (more (make-indent (+ tl indent))))
    (d:register-like indent title tl v w more)))

(define (d:broken-window indent w type)
  (let* ((item (if (eq? type 'LSI)
                   (first (window-items w))
                   (last (window-items w))))
         (start (item-offset item))
         (end (+ start (- (item-width item) 1)))
         (limit (if (eq? type 'LSI)
                    (window-offset w)
                    (+ (window-offset w) (- (window-width w) 1)))))
    (list (cat (make-indent indent)
               (format #f "Incomplete ~a ~a: start: ~a, end ~a, limit: ~a"
                       type (item-name item) start end limit)))))

(define (d:register-window-value rw indent)
  (let* ((window (decoder-register-window-description rw))
         (w (window-width window))
         (o (window-offset window))
         (v (ash (decoder-register-window-raw rw)
                 (* -1 (window-offset window))))
         (title "Raw Register-Window Value: ")
         (tl (string-length title))
         (more (make-indent (+ tl indent))))
    (append (d:register-like indent title tl v w more)
            (list (format #f "Window Offset: ~a, Width: ~a" o w))
            (if (lsi-complete? window) '()
                (list (d:broken-window indent window 'LSI)))
            (if (msi-complete? window) '()
                (list (d:broken-window indent window 'MSI))))))

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
         (v (decoder-item-raw item))
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
         " - offset: "
         (d:item:offset+width (number->string o))
         ", width: "
         (d:item:offset+width (number->string w)))))

(define (d:item-decoded-value item indent)
  (let ((v (decoder-item-decoded item)))
    (cat (make-indent indent)
         "Decoded: "
         (d:item:highlight:decoded (maybe-string v)))))

(define (d:item-value item indent)
  (let ((sem (item-semantics (decoder-item-description item))))
    (list (cat (make-indent indent)
               "Item "
               (double-quote
                (d:item:highlight:name
                 (maybe-string (item-name (decoder-item-description item)))))
               " (semantics: "
               (d:item:highlight:semantics (if (semantics? sem)
                                               (maybe-string (semantics-type sem))
                                               "*unknown-semantics*"))
               "):")
          (d:item-raw-value item (+ 2 indent))
          (d:item-decoded-value item (+ 2 indent)))))

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

        ((decoder-register-window? thing)
         (trace-?? 'register-window indent thing)
         (list (d:register-window-value thing indent)
               (??+ (decoder-register-window-items thing) indent)))

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

(define (decode-to-text description value)
  (let ((lst (flatten (?? (decode* description value)))))
    (for-each (lambda (s)
                (unless (eq? s #:ignore)
                  (display s)
                  (newline)))
              lst)
    (length lst)))
