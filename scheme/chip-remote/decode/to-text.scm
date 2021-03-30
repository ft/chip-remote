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
  #:use-module (ice-9 pretty-print)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote manufacturer)
  #:use-module (chip-remote combination)
  #:use-module (chip-remote decode)
  #:use-module (chip-remote decode types)
  #:use-module (chip-remote item)
  #:use-module (chip-remote named-value)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register-window)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote device)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:export (decode-to-text))

(define (string-ends-in-newline s)
  (char=? #\newline (string-ref s (1- (string-length s)))))

(define (string-strip-newlines s)
  (cond ((string-null? s) s)
        ((string-ends-in-newline s)
         (string-strip-newlines (substring s 0 (1- (string-length s)))))
        (else s)))

(define (pp-to-list obj indent)
  (string-split
   (string-strip-newlines
    (with-output-to-string
      (lambda ()
        (pretty-print obj
                      #:display? #t
                      #:per-line-prefix (make-indent indent)
                      #:width 128
                      #:max-expr-width 96))))
   #\newline))

(define (pp:combination-spec spec)
  (cond ((null? spec) spec)
        ((list? spec) (map pp:combination-spec spec))
        ((pair? spec) (cons (pp:combination-spec (car (spec)))
                            (pp:combination-spec (cdr (spec)))))
        ((semantics? spec)
         (format #f "#<semantics ~a ~a>"
                 (semantics-type spec)
                 (semantics-name spec)))
        (else spec)))

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

(define (d:indent state thing)
  (* (length (ps-level state)) 2))

(define (make-indent n)
  (make-string n #\space))

(define d:item:highlight:name (make-highlighter 'yellow))
(define d:item:highlight:decoded (make-highlighter 'red #:bold? #t))
(define d:item:highlight:semantics (make-highlighter 'magenta #:bold? #t))
(define d:regmap:highlight:name (make-highlighter 'yellow))
(define d:pagemap:highlight:name (make-highlighter 'white #:bold? #t))
(define d:combination:highlight:name (make-highlighter 'yellow))
(define d:combinations:highlight:title (make-highlighter 'white #:bold? #t))
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

(define (d:register-value proc state d:reg)
  (let* ((v (decoder-register-raw d:reg))
         (w (register-width (decoder-register-description d:reg)))
         (lvl (d:indent state 'register))
         (title "Raw Register Value: ")
         (tl (string-length title))
         (more (make-indent (+ tl lvl))))
    (d:register-like lvl title tl v w more)))

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

(define (d:register-window-value proc state d:win win items lsi msi)
  (let* ((w (window-width win))
         (o (window-offset win))
         (v (ash (decoder-register-window-raw d:win)
                 (* -1 (window-offset win))))
         (title "Raw Register-Window Value: ")
         (tl (string-length title))
         (lvl (d:indent state 'register-window))
         (more (make-indent (+ tl lvl))))
    (append (d:register-like lvl title tl v w more)
            (list (format #f "Window Offset: ~a, Width: ~a" o w))
            (if (lsi-complete? win) '()
                (list (d:broken-window lvl win 'LSI)))
            (if (msi-complete? win) '()
                (list (d:broken-window lvl win 'MSI)))
            (ps-content state))))

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

(define (pp:semantics sem)
  (d:item:highlight:semantics
   (if (semantics? sem)
       (let ((type (semantics-type sem))
             (sname (semantics-name sem)))
         (if (eq? 'table-lookup type)
             (let* ((data (semantics-data sem))
                    (tname (if (named-value? data)
                               (value-name data)
                               "*unnamed-table*")))
               (string-concatenate
                (list (maybe-string type)
                      ": "
                      (or sname (maybe-string tname)))))
             (maybe-string (or sname "*unnamed-semantics*"))))
       "*unknown-semantics*")))

(define (d:item-value proc state d:item)
  (let ((sem (item-semantics (decoder-item-description d:item)))
        (lvl (d:indent state 'item)))
    (list (cat (make-indent lvl)
               "Item "
               (double-quote
                (d:item:highlight:name
                 (maybe-string (item-name (decoder-item-description d:item)))))
               " (semantics: " (pp:semantics sem) "):")
          (d:item-raw-value d:item (+ 2 lvl))
          (d:item-decoded-value d:item (+ 2 lvl)))))

(define (d:regmap:num b v)
  (number->terminal v #:base b #:prefix? #f #:decorate? #t #:zeropad? #f
                    #:highlight (make-highlighter 'magenta #:bold? #t)))

(define (d:regmap:hex v) (d:regmap:num 16 v))
(define (d:regmap:octal v) (d:regmap:num 8 v))
(define (d:regmap:binary v) (d:regmap:num 2 v))
(define (d:regmap:decimal v) (d:regmap:num 10 v))

(define (d:register-map proc state d:rm)
  (list (let ((addr (ps-address state))
              (lvl (d:indent state 'register-map)))
          (if (integer? addr)
              (cat (make-indent lvl)
                   (d:regmap:highlight:name "Decoding Register-Map at address: ")
                   (d:regmap:decimal addr)
                   ", "
                   (d:regmap:hex addr)
                   ", "
                   (d:regmap:octal addr)
                   ", "
                   (d:regmap:binary addr))
              (cat (make-indent lvl)
                   (d:regmap:highlight:name "Decoding Register-Map: "))))
        (ps-content state)))

(define (d:register proc state d:reg)
  (list (let ((addr (ps-address state))
              (lvl (d:indent state 'register)))
          (if (integer? addr)
              (cat (make-indent lvl)
                   (d:regmap:highlight:name "Decoding Register at address: ")
                   (d:regmap:decimal addr)
                   ", "
                   (d:regmap:hex addr)
                   ", "
                   (d:regmap:octal addr)
                   ", "
                   (d:regmap:binary addr))
              (cat (make-indent lvl)
                   (d:regmap:highlight:name "Decoding Register: "))))
        (d:register-value proc state d:reg)
        (ps-content state)))

(define (d:pagemap:num b v)
  (number->terminal v #:base b #:prefix? #f #:decorate? #t #:zeropad? #f
                    #:highlight (make-highlighter 'magenta #:bold? #t)))

(define (d:pagemap:hex v) (d:pagemap:num 16 v))
(define (d:pagemap:octal v) (d:pagemap:num 8 v))
(define (d:pagemap:binary v) (d:pagemap:num 2 v))
(define (d:pagemap:decimal v) (d:pagemap:num 10 v))

(define (d:page-map proc state d:pm)
  (list (cat (make-indent (d:indent state 'page-map))
             (d:pagemap:highlight:name "Decoding Page Map: "))
        (ps-content state)))

(define-syntax-rule (thunk body* ...)
  (lambda () body* ...))

(define (maybe-add thing thunk)
  (if thing (thunk) '()))

(define (d:device:key/value k v indent)
  (cat (make-indent indent)
       (d:device:highlight:key (maybe-string k))
       ": "
       (d:device:highlight:value (maybe-string v))))

(define (d:combinations proc state value)
  (let ((c (ps-content state))
        (level (d:indent state 'combination)))
    (if (null? c)
        c
        (cons (cat (make-indent level)
                   (d:combinations:highlight:title "Decoding combined items:"))
              c))))

(define (d:combination-raw-value combination indent)
  (let* ((c (decoder-combination-data combination))
         (v (c:raw c))
         (w (c:width c)))
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
         (d:item:offset+width (number->string w)))))

(define (d:combination-decoded-value combination indent)
  (let ((v (decoder-combination-decoded combination)))
    (cat (make-indent indent)
         "Decoded: "
         (d:item:highlight:decoded (maybe-string v)))))

(define (d:combination proc state value)
  (let  ((name (decoder-combination-name value))
         (sem (c:semantics (decoder-combination-data value)))
         (spec (c:spec (decoder-combination-data value)))
         (decoded (decoder-combination-decoded value))
         (level (d:indent state 'combination)))
    (list (cat (make-indent level)
               "Combination "
               (double-quote (d:combination:highlight:name (maybe-string name)))
               " (semantics: " (pp:semantics sem) "):")
          (d:combination-raw-value value (+ 2 level))
          (d:combination-decoded-value value (+ 2 level))
          (cat (make-indent (+ 2 level))
               "Specification:")
          (pp-to-list (pp:combination-spec spec) (+ 4 level)))))

(define (d:device proc state d:device)
  (let* ((dev (decoder-device-description d:device))
         (meta (device-meta dev))
         (name (assq-ref meta #:name))
         (man (assq-ref meta #:manufacturer))
         (man-name (and man (manufacturer-name man)))
         (man-hp (and man (manufacturer-homepage man)))
         (man-wp (and man (manufacturer-wikipedia man)))
         (level (d:indent state 'device))
         (hp (assq-ref meta #:homepage))
         (ds (assq-ref meta #:datasheet))
         (kw (assq-ref meta #:keywords)))
    (append
     (list (if name
               (cat (d:device:highlight:title "Decoding Device ")
                    (d:device:highlight:name (maybe-string name))
                    ": ")
               (d:device:highlight:title "Decoding Device:"))
           (if man
               (list
                (maybe-add man-name
                           (thunk (d:device:key/value "Manufacturer"
                                                      man-name level)))
                (maybe-add man-hp
                           (thunk (d:device:key/value "Homepage"
                                                      man-hp (+ 2 level))))
                (maybe-add man-wp
                           (thunk (d:device:key/value "Wikipedia"
                                                      man-wp (+ 2 level)))))
               '())
           (maybe-add hp (thunk (d:device:key/value "Device Page" hp level)))
           (maybe-add ds (thunk (d:device:key/value "Datasheet" ds level)))
           (maybe-add kw (thunk (d:device:key/value "Keywords" kw level))))
     (ps-content state))))

(define (decode-to-text desc value)
  (let ((lst (flatten (process (make-processor #:item d:item-value
                                               #:register d:register
                                               #:window d:register-window-value
                                               #:register-map d:register-map
                                               #:page-map d:page-map
                                               #:combinations d:combinations
                                               #:combination d:combination
                                               #:device d:device)
                               (make-processor-state #:debug? #f)
                               (decode* desc value)))))
    (for-each (lambda (s)
                (unless (eq? s #:ignore)
                  (display s)
                  (newline)))
              lst)
    (length lst)))
