(use-modules (ice-9 control)
             (ice-9 match)
             (ice-9 pretty-print)
             (srfi srfi-1))

(define *double-buffered* '())

(define (push-db! expr)
  (match expr
    ((def name . alist)
     (set! *double-buffered*
           (append! *double-buffered*
                    `((,name . ,(car (assq-ref alist 'address)))))))))

(define (pp obj)
  (pretty-print obj #:max-expr-width 78))

(define (flatten lst)
  "Flatten deep tree to a shallow list"
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append (flatten (car lst))
                      (flatten (cdr lst))))))

(define (plist->alist plist)
  (if (null? plist)
      '()
      (begin
        (unless (keyword? (car plist))
          (throw 'not-a-plist plist))
        (let loop ((alist '())
                   (current (list (keyword->symbol (car plist))))
                   (rest (cdr plist)))
          (if (null? rest)
              (if (null? current)
                  alist
                  (append alist (list current)))
              (let ((hd (car rest)))
                (cond ((keyword? hd)
                       (loop (append alist (list current))
                             (list (keyword->symbol hd))
                             (cdr rest)))
                      (else (loop alist
                                  (append current (list hd))
                                  (cdr rest))))))))))

(define (cleanup-item i)
  (match i
    ((n o w . rest) (append (list n `(offset ,o) `(width ,w))
                            (plist->alist rest)))))

(define (cleanup-contents x)
  (match x
    (('contents . items) (cons 'contents (map cleanup-item items)))
    (else x)))

(define (cleanup-register expr)
  (catch 'not-a-plist
    (lambda ()
      (match expr
        ((def name . plist)
         (cons* def name (map cleanup-contents (plist->alist plist))))))
    (lambda (k . a)
      (format #t "Not a plist in ~s~%" expr)
      (format #t "→ ~s~%" a)
      (quit 1))))

(define (rewrite-semantics alist expr)
  (let* ((def* (assq 'default alist))
         (def (and def* (cadr def*))))
    (match expr
      (('lookup table)
       `((tbl ,table . ,(if def* `(#:default ,def) '()))))
      (_ expr))))

(define (post-rewrite-item-cleanup* expr)
  expr)

(define (post-rewrite-item-cleanup expr)
  ;; This is not super precise, but it's good enough.
  (match expr
    (('‣ n o w . alist)
     `(‣ ,n ,o ,w . ,(let* ((def* (assq 'default alist))
                            (def (and def* (cadr def*)))
                            (sem* (assq 'semantics alist))
                            (sem (and sem* (cdr sem*))))
                       (if (and sem* def* (memq #:default (flatten sem)))
                           (filter (lambda (x) (not (eq? 'default (car x))))
                                   alist)
                           alist))))))

(define (make-process-property alist)
  (lambda (expr stack)
    (match expr
      (((or 'offset 'width) . rest) stack)
      (((or 'semantics 'semantics*) . rest)
       (cons (cons 'semantics (rewrite-semantics alist rest))
             stack))
      (('validate 'range ('>= a) ('<= b))
       (cons `(semantics
               (semantics (inherit unsigned-integer)
                          (range (lambda (s w) '(range ,a ,b)))))
             stack))
      (_ (cons expr stack)))))

(define (rewrite-item expr)
  (post-rewrite-item-cleanup
   (let* ((name (car expr))
          (alist (cdr expr))
          (offset (car (assq-ref alist 'offset)))
          (width (car (assq-ref alist 'width))))
     `(‣ ,name ,offset ,width . ,(fold-right (make-process-property alist)
                                             '() alist)))))

(define (make-process-register reg)
  (lambda (expr stack)
    (match expr
      (((or 'double-buffer 'double-buffer?) v)
       (when v (push-db! reg))
       stack)
      (('register-width rest) (cons (list 'width rest) stack))
      (('contents . items)
       (cons `(items ,(cons 'list (map rewrite-item items))) stack))
      (_ (cons expr stack)))))

(define (rewrite-name name)
  (string->symbol (substring (symbol->string name) 4)))

(define (rewrite-register expr)
  (match expr
    ((def name . rest)
     (list 'define name
           `(register
             (name (quote ,(rewrite-name name))) .
             ,(fold-right (make-process-register expr) '() rest))))))

(define handle-register (compose rewrite-register cleanup-register))

(define (new-module kind)
  (case kind
    ((dw1000)
     `(define-module (chip-remote devices decawave dw1000 registers)
        #:use-module (chip-remote codecs)
        #:use-module (chip-remote item)
        #:use-module (chip-remote item builder)
        #:use-module (chip-remote register)
        #:use-module (chip-remote semantics)
        #:use-module (chip-remote devices decawave dw1000 tables)
        #:export (reg:device-id
                  reg:ieee-eui
                  reg:pan-id/short-address
                  reg:system-cfg
                  reg:system-time
                  reg:tx-frame-ctrl
                  reg:tx-buffer
                  reg:delayed-tx/rx-time
                  reg:rx-frame-wait-timeout
                  reg:system-ctrl
                  reg:system-event-mask
                  reg:system-status
                  reg:rx-frame-info
                  reg:rx-buffer
                  reg:rx-frame-quality-info
                  reg:rx-time-track-interval
                  reg:rx-time-track-offset
                  reg:rx-time-of-arrival
                  reg:tx-time-of-sending
                  reg:tx-antenna-delay
                  reg:system-state
                  reg:ack-time/response-time
                  reg:rx-sniff-mode-cfg
                  reg:tx-power-ctrl
                  reg:channel-ctrl
                  reg:user-sfd-sequences
                  reg:agc-ctrl
                  reg:external-sync-ctrl
                  reg:accumulator-memory
                  reg:gpio-ctrl
                  reg:digital-rx-cfg
                  reg:analog-rx-cfg
                  reg:tx-calibration-cfg
                  reg:frequency-synthesizer-ctrl
                  reg:always-on-ctrl
                  reg:otp-interface
                  reg:leading-edge-detect-ctrl
                  reg:digital-diagnostics
                  reg:power-management-ctrl)))
    ((dw3000)
     `(define-module (chip-remote devices decawave dw3000 registers)
        #:use-module (chip-remote codecs)
        #:use-module (chip-remote item)
        #:use-module (chip-remote item builder)
        #:use-module (chip-remote register)
        #:use-module (chip-remote semantics)
        #:use-module (chip-remote devices decawave dw3000 tables)
        #:export (reg:general-cfg
                  reg:general-cfg-and-aes
                  reg:sts-cfg
                  reg:rx-tune
                  reg:ext-sync
                  reg:gpio-ctrl
                  reg:digital-rx-cfg
                  reg:analog-rf-cfg
                  reg:tx-calibration
                  reg:freq-synth-ctrl
                  reg:always-on-system-control
                  reg:otp-interface
                  reg:cia-0
                  reg:cia-1
                  reg:cia-2-and-rx-antenna-delay
                  reg:digital-diag
                  reg:pmsc
                  reg:rx-buffer-0
                  reg:rx-buffer-1
                  reg:tx-buffer
                  reg:acc-mem
                  reg:scratch-ram
                  reg:aes-ram
                  reg:set-1/set-2
                  reg:indirect-ptr-a
                  reg:indirect-ptr-b
                  reg:in-ptr-cfg)))))

(with-input-from-file "registers.scm.old"
  (lambda ()
    (call/ec
     (lambda (return)
       (let loop ((datum (read)))
         (when (eof-object? datum)
           (return))
         (newline)
         (match datum
           (('define . rest)          (pp datum))
           (('define-module . rest)   (pp (new-module 'dw3000)))
           (('define-register . rest) (pp (handle-register datum)))
           (_ (format #t "(unhandled) ~s~%" datum)))
         (loop (read)))))))

(newline)
(pp (append '(define double-buffered-registers)
            `((quote ,*double-buffered*))))
