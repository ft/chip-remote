;; Copyright (c) 2025 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote pretty-print)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (chip-remote bit-operations)
  #:use-module (chip-remote device)
  #:use-module (chip-remote item)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register-map)
  #:use-module (data-structures sized-stack)
  #:export (cr-pretty-print))

;; The default pretty printers for the types in chip-remote produce compact
;; views of the data contained within the objects:
;;
;;    #<device name: dw1000 pages: 39 states: 1>
;;    #<register-map name: digital-diagnostics registers: 15>
;;    #<register name: event-ctrl items: 3>
;;    #<item name: event-counter-enable
;;           offset: 0
;;           width: 1
;;           semantics: boolean
;;           default: #f>
;;
;; Which is excellent for REPL usage, where you do not want to clutter the
;; display with pages and pages of output just because something returned a
;; complex device instance.
;;
;; Sometimes it is useful to see the full details of an object, and here a re-
;; cursive pretty-printer is useful. And this is what this module implements:
;;
;;   #<device name: dw1000 states: 1
;;     #<page-map name: #f pages: 39
;;       [#x00 #<register-map name: device-id registers: 2
;;               {#x0 #<register name: device-type items: 3
;;                      #<item name: revision offset: 0 width: 4 …>
;;                      #<item name: version offset: 4 width: 4 …>
;;                      #<item name: model offset: 8 width: 8 …>>}
;;               {#x2 #<register name: id items: 1
;;                      #<item name: register-id offset: 0 width: 16 …>>}>]
;;       [#x01 #<register-map name: ieee-euid registers: 2
;;               {#x0 #<register name: euid-device items: 1
;;                      #<item name: euid-device offset: 0 width: 40 …>>}
;;               {#x5 #<register name: euid-manufacturer items: 1
;;                      #<item name: euid-manufacturer offset: 0 width: 24 …>>}>]
;;       …
;;       [#x36 #<register-map name: power-management-ctrl registers: 7
;;               {#x00 #<register name: pmsc-ctrl-0 items: 16
;;                       #<item name: system-clock-select offset: 0 width: 2 …>
;;                       …
;;                       #<item name: soft-reset offset: 28 width: 4 …>>}
;;               …
;;               {#x28 #<register name: pmsc-led-ctrl items: 5
;;                       #<item name: led-blink-time offset: 0 width: 8 …>
;;                       #<item name: led-blink-enable offset: 8 width: 1 …>
;;                       #<item name: reserved offset: 9 width: 7 …>
;;                       #<item name: led-blink-now-mask offset: 16 width: 4 …>
;;                       #<item name: reserved offset: 20 width: 12 …>>}>]>>

(define-immutable-record-type <crp-state>
  (make-crp-state* indentation per-line-prefix port)
  crp-state?
  (indentation     crp:indentation crp:new-indentation)
  (per-line-prefix crp:per-line-prefix)
  (port            crp:port))

(define (line-init state)
  (let ((port (crp:port state))
        (str (crp:per-line-prefix state)))
    (and str (display str port))
    (display (make-string (crp:indentation state) #\space) port)))

(define (line-combine state)
  (newline)
  (line-init state))

(define* (make-crp-state #:key
                         (indentation 0)
                         (per-line-prefix #f)
                         (port (current-output-port)))
  (make-crp-state* indentation per-line-prefix port))

(define (new-indent state modification)
  (crp:new-indentation state (+ (crp:indentation state) modification)))

(define* (crp obj #:optional (state (make-crp-state)))
  (cond ((register?     obj) (crp:register     state obj))
        ((register-map? obj) (crp:register-map state obj))
        ((page-map?     obj) (crp:page-map     state obj))
        ((device?       obj) (crp:device       state obj))
        (else (format (crp:port state) "~a" obj))))

(define* (cr-pretty-print obj #:key
                          (port (current-output-port))
                          per-line-prefix)
  (let ((state (make-crp-state #:port port #:per-line-prefix per-line-prefix)))
    (line-init state)
    (crp obj state)
    (newline)))

(define (crp:item state obj)
  (crp obj state))

(define (crp:register state obj)
  (let ((port (crp:port state))
        (name (register-name obj))
        (items (register-items obj)))
    (format port "#<register name: ~a items: ~a" name (length items))
    (let ((state* (new-indent state 2)))
      (for-each (lambda (item)
                  (line-combine state*)
                  (crp:item state* item))
                items))
    (display ">" port)))

(define (crp:tablish state obj str:open str:close get-table next show-type)
  (let* ((port (crp:port state))
         (rt (get-table obj))
         (registers (length rt))
         (final (and (not (zero? registers))
                     (car (last rt))))
         (awidth (if final
                     (hex-width final)
                     0)))
    (show-type port obj)
    (let ((state* (new-indent state 2)))
      (for-each (lambda (thing)
                  (let ((idx (car thing))
                        (data (cdr thing)))
                    (line-combine state*)
                    (if (integer? idx)
                        (format #t "~a#x~v,'0x " str:open awidth idx)
                        (format #t "~a#f " str:open))
                    (next (new-indent state (+ awidth 6)) data)
                    (display str:close port)))
                rt))
    (display ">" port)))

(define (crp:register-map state obj)
  (crp:tablish state obj "{" "}" register-map-table:sorted crp:register
               (lambda (port obj)
                 (format port "#<register-map name: ~a registers: ~a"
                         (register-map-name obj)
                         (length (register-map-table obj))))))

(define (crp:page-map state obj)
  (crp:tablish state obj "[" "]" page-map-table:sorted crp:register-map
               (lambda (port obj)
                 (format port "#<page-map name: ~a pages: ~a"
                         (page-map-name obj)
                         (length (page-map-table obj))))))

(define (crp:device state obj)
  (let* ((port (crp:port state))
         (name (device-name obj))
         (states (sized-stack-used (device-state obj))))
    (format port "#<device name: ~a states: ~a" name states)
    (let ((state* (new-indent state 2)))
      (line-combine state*)
      (crp:page-map state* (device-page-map obj)))
    (display ">" port)))
