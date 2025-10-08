(define-module (chip-remote device)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 pretty-print)
  #:use-module (chip-remote decode)
  #:use-module (chip-remote device access)
  #:use-module (chip-remote item)
  #:use-module (chip-remote modify)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote utilities)
  #:use-module (data-structures sized-stack)
  #:use-module (data-structures records)
  #:use-module (data-structures records utilities)
  #:export (device                   ;; Device creation
            make-device
            define-device
            device?                  ;; Device type API
            device-name
            device-manufacturer
            device-homepage
            device-datasheet
            device-keywords
            device-register-width
            device-page-map
            device-combinations
            device-access
            device-state
            current-device-state     ;; Device utilities
            make-default-device-state
            push-device-state
            reset-device-state
            device-value-suitable?
            device-default
            device-diff
            device-history
            device-registers
            device-item-names
            device-canonical
            device-ref
            device-address
            device-address:register
            device-address-map
            device-extract
            device-setup!            ;; Actual interaction
            device-xfer!
            device-read!
            device-write!))

(define-record-type* <device>
  device make-device device? this-device
  (name           device-name (default #f))
  (manufacturer   device-manufacturer (default #f))
  (homepage       device-homepage (default #f))
  (datasheet      device-datasheet (default #f))
  (keywords       device-keywords (default '()))
  (register-width device-register-width (default #f))
  (page-map       device-page-map (sanitize (need 'page-map page-map?)))
  (combinations   device-combinations (default '()))
  (access         device-access (default access-not-implemented))
  (state          device-state (thunked)
                  (default (make-default-device-state this-device))))

(new-record-definer define-device device)

(set-record-type-printer! <device>
  (lambda (dev port)
    (simple-format port "#<device name: ~a maps: ~a states: ~a>"
                   (device-name dev)
                   (length (page-map-table (device-page-map dev)))
                   (sized-stack-used (device-state dev)))))

(define* (make-default-device-state dev #:key (stack-size 1024))
  (make-sized-stack stack-size #:preload `((init . ,(device-default dev)))))

(define (current-device-state dev)
  (let ((state (device-state dev)))
    (if (sized-stack-empty? state)
        (device-default dev)
        (cdr (sized-stack-peek state)))))

(define (push-device-state dev annotation value)
  (device (inherit dev)
          (state (sized-stack-push (device-state dev)
                                   (cons annotation value)))))

(define (reset-device-state dev)
  (push-device-state dev 'default (device-default dev)))

(define (device-default dev)
  (page-map-default (device-page-map dev)))

(define (device-item-names dev)
  (page-map-item-names (device-page-map dev)))

(define (device-registers dev)
  (map cdr
       (apply append (map register-map-table
                          (map cdr
                               (page-map-table (device-page-map dev)))))))

(define (device-address-map dev)
  (sort (page-map-fold
         (lambda (page-addr page acc)
           (cons (cons page-addr
                       (sort (register-map-fold
                              (lambda (reg-addr reg acc)
                                (cons reg-addr acc))
                              '() page)
                             index<))
                 acc))
         '() (device-page-map dev))
        (lambda (a b)
          (index< (car a) (car b)))))

(define (device-canonical d . item-address)
  (apply page-map-canonical (cons (device-page-map d) item-address)))

(define (device-address d . item-address)
  (apply page-map-address (cons (device-page-map d) item-address)))

(define (device-address:register d . item-address)
  (apply page-map-address:register (cons (device-page-map d) item-address)))

(define (device-ref d . args)
  (apply page-map-ref (cons (device-page-map d) args)))

(define (device-value-suitable? d v)
  "Test whether ‘v’ is a suitable value for the device ‘d’."
  (structurally-equal? v (device-default d)))

(define (fe0 lst)
  (filter (compose not null?) lst))

(define (fe1 lst)
  (filter (lambda (x) (or (and (pair? x)
                               (diff? (cdr x)))
                          (> (length x) 1)))
          lst))

(define (minimise-diff/item item)
  (match item
    ((name . value) (if (diff? value)
                        item
                        '()))))

(define (minimise-diff/register reg)
  (match reg
    ((addr items ...) (cons addr (fe0 (map minimise-diff/item items))))
    ((addr . value)   (if (diff? value)
                          (cons addr value)
                          '()))))

(define (minimise-diff/page page)
  (match page
    (('combinations kv ...) (cons 'combinations
                                  (fe0 (map minimise-diff/item kv))))
    ((addr kv ...) (cons addr
                         (fe1 (map minimise-diff/register kv))))))

(define (minimise-diff/device dev)
  (fe1 (map minimise-diff/page dev)))

(define (minimise-diff data)
  (match data
    ((mark . rest) (cons mark (minimise-diff/device rest)))))

(define (xdiff a b)
  (let ((ra (cdr a))
        (rb (cdr b)))
    (cons (car b)
          (let-values (((diff? v) (diff ra rb)))
            v))))

(define* (device-history dev #:optional count #:key (decode? #t))
  (let* ((dec (lambda (x) (cons (car x) (decode dev (cdr x)))))
         (st (device-state dev))
         (count* (and count (min (1+ count) (sized-stack-used st))))
         (memory (sized-stack-memory st))
         (data (reverse (if count* (take memory count*) memory))))
    (if decode? (map dec data) data)))

(define* (device-diff dev #:optional count #:key (decode? #t))
  (map minimise-diff
       (pair-combine xdiff
                     (device-history dev count #:decode? decode?))))

(define (device-extract dev value addr)
  (let* ((faddr (apply device-canonical
                       (cons dev (if (list? addr) addr (list addr)))))
         (part (apply device-ref (cons dev faddr)))
         (pv (apply value-at-address (cons value faddr))))
    `((address . ,faddr)
      (part . ,part)
      (value . ,pv)
      (item . ,(if (item? part) (item-get part pv) pv)))))

;; The following are talking through actual devices via the chip-remote
;; firmware.

(define (device-setup! c bus dev)
  (let* ((ops (device-access dev))
         (make-setup (device-operations-make-setup ops))
         (setup (device-bus-setup bus))
         (ifc (device-bus-interface bus)))
    (apply setup c ifc (make-setup dev))))

(define (device-xfer! c bus data)
  ((device-bus-xfer bus) c (device-bus-interface bus) data))

(define (device-read! c bus dev addr)
  (let* ((ops (device-access dev))
         (read (device-operations-read ops))
         (parse (device-operations-read-parse ops)))
    (let-values (((data meta) (read dev addr)))
      (parse dev addr (device-xfer! c bus data) meta))))

(define (device-write! c bus dev addr value)
  (let* ((ops (device-access dev))
         (write (device-operations-write ops))
         (parse (device-operations-write-parse ops)))
    (let-values (((data meta) (write dev addr value)))
      (parse dev addr value (device-xfer! c bus data) meta))))
