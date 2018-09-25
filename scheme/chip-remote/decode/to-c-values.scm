;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (chip-remote decode to-c-values)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote decode)
  #:use-module (chip-remote decode types)
  #:use-module (chip-remote device)
  #:use-module (chip-remote item)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote utilities)
  #:export (decode-to-values decode-to-c-values))

(define (d:item-value proc state d:item)
  (let ((name (item-name (decoder-item-description d:item))))
    `(comment ,(format #f "~a: ~a" name (decoder-item-decoded d:item)))))

(define (d:register proc state d:reg)
  (append `((register-address ,(ps-address state)))
          (ps-content state)
          `((width ,(register-width (decoder-register-description d:reg))))
          `((value ,(decoder-register-raw d:reg)))))

(define (d:register-window-value proc state d:win)
  '((comment "register-window export not implemented yet")))

(define (d:register-map proc state d:rm)
  (append `((register-map-address ,(ps-address state))) (ps-content state)))

(define (d:page-map proc state d:pm)
  (ps-content state))

(define (d:device proc state d:dev)
  (append `((device ,(device-name (decoder-device-description d:dev))))
          (ps-content state)))

(define (decode-to-values description value)
  (process (make-processor #:item d:item-value
                           #:register d:register
                           #:window d:register-window-value
                           #:register-map d:register-map
                           #:page-map d:page-map
                           #:device d:device)
           (make-processor-state #:debug? #f)
           (decode* description value)))

(define-immutable-record-type <generate-c-state>
  (make-generate-c-state last? mode name width type)
  generate-c-state?
  (last? gs-last? new-last?)
  (mode gs-mode new-mode)
  (name gs-name new-name)
  (width gs-width new-width)
  (type gs-type new-type))

(define (generate-c-headers state lst)
  (list "#include <stdint.h>"))

(define (generate-c-types state lst)
  (match lst
    ((('device n) . rest)
     (list (generate-c-types (new-name state n) (car rest))
           ""
           (format #f "struct device_~a {" n)
           "    unsigned int rmn;"
           (format #f "    struct regmap_~a *maps;" n)
           "};"))
    ((('register-map-address a) . rest)
     (list (generate-c-types state (car rest))
           ""
           (format #f "struct regmap_~a {" (gs-name state))
           "    unsigned int address;"
           "    unsigned int regn;"
           (format #f "    struct register_~a *registers;" (gs-name state))
           "};"))
    ((('register-address a) . rest)
     (list (format #f "struct register_~a {" (gs-name state))
           "    unsigned int address;"
           "    uint32_t value;"
           "};"))
    (else (list (format #f "/* Huh. What's this? ~a */" lst)))))

(define (generate-c-source state lst)
  (match lst
    ((('device n) . rest)
     (list (map (lambda (x)
                  (generate-c-source (new-mode (new-name state n) 'regs) x))
                rest)
           ""
           (format #f "struct regmap_~a ~a_maps[] = {" n n)
           (map/last (lambda (last? x)
                       (generate-c-source (new-last? (new-name state n)
                                                     last?)
                                          x))
                     rest)
           "};"
           ""
           (format #f "struct device_~a ~a = {" n n)
           (format #f "    .rmn = ~au," (length rest))
           (format #f "    .maps = ~a_maps" n)
           "};"))
    ((('register-map-address a) . rest)
     (if (eq? 'regs (gs-mode state))
         (list (format #f "struct register_~a ~a_rm~a_registers[] = {"
                       (gs-name state) (gs-name state) (if (integer? a) a 0))
               (map/last (lambda (last? x)
                           (generate-c-source (new-last? state last?) x)) rest)
               "};")
         (list (format #f "    { .address = ~au," (if (integer? a) a 0))
               (format #f "      .regn = ~au," (length rest))
               (format #f "      .registers = ~a_rm~a_registers }~a"
                       (gs-name state)
                       (if (integer? a) a 0)
                       (if (gs-last? state) "" ",")))))
    ((('register-address a) . rest)
     (list (format #f "    { .address = ~au," a)
           (format #f "      .value = 0x~v,'0xul }~a"
                   (/ (gs-width state) 4)
                   (cadr (assq 'value rest))
                   (if (gs-last? state) "" ","))))
    (else (list (format #f "/* Huh. What's this? ~a */" lst)))))

(define (next-power-of-two n)
  (2e (inexact->exact (ceiling (log2 n)))))

(define (grep x lst)
  (cond ((null? lst) '())
        ((not (pair? lst)) '())
        ((eq? x (car lst)) (cdr lst))
        (else (append (grep x (car lst))
                      (grep x (cdr lst))))))

(define (find-max-width lst)
  (apply max (grep 'width lst)))

(define (decode-to-c-values description value)
  (let* ((lst (decode-to-values description value))
         (width (next-power-of-two (find-max-width lst)))
         (type (format #f "uint~a_t" width))
         (state (make-generate-c-state #f #f #f width type)))
    (for-each (lambda (s) (display s) (newline))
              (flatten (list (generate-c-headers state lst)
                             ""
                             (generate-c-types state lst)
                             ""
                             (generate-c-source state lst))))))
