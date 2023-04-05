;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (protocol saleae-spi)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (chip-remote utilities)
  #:use-module (protocol csv)
  #:export (log->frames read-frames read-log pick-frames))

;; Takes a port, reads csv data from it, puts it into scheme data.
(define parse-protocol
  (make-csv-reader #\, #:have-row (lambda (r rs) (cons (reverse r) rs))))

;; The first line of saleae csv logs is a headline that names the columns found
;; in the file. We'll use this to form dictionaries of data from raw csv logs.
(define (parse-headline raw)
  (map string->symbol (car raw)))

;; Make CS delimited frames from raw SPI data

(define (maybe-data lst key)
  (let ((datum (assq-ref lst key)))
    (cond ((list? datum) datum)
          ((not datum) '())
          (else (list datum)))))

(define (spi-error part)
  `((error (name     . ,(assq-ref part 'name))
           (time     . ,(assq-ref part 'start_time))
           (duration . ,(assq-ref part 'duration)))))

(define (more-spi-data frame part)
  (let ((mosi-old (maybe-data frame 'mosi))
        (mosi-new (maybe-data part  'mosi))
        (miso-old (maybe-data frame 'miso))
        (miso-new (maybe-data part  'miso)))
    (assq-change (assq-change frame 'mosi (append mosi-old mosi-new))
                 'miso (append miso-old miso-new))))

(define (spi-frame part with-timestamps?)
  (if with-timestamps?
      (list (cons 'time (assq-ref part 'start_time)))
      (list)))

(define (cs-framing part log with-timestamps?)
  ;;(format #t "part: ~a~%" part)
  (let* ((current (car log))
         (name (assq-ref part 'name))
         (type (assq-ref part 'type))
         (frame (or (assq-ref current name) '())))
    ;; Feed frame part into log. The log's first entry is a name to data-list
    ;; mapping, that tracks currently active frames. Only data after CS enable
    ;; and before disable events are considered. Everything else is discarded.
    (case type
      ((enable) (cons (assq-change current name
                                   (spi-frame part with-timestamps?))
                      (cdr log)))
      ((disable) (cons (assq-change current name '())
                       (append (cdr log) (list (cons name frame)))))
      ((result) (cons (assq-change current name (more-spi-data frame part))
                      (cdr log)))
      ((error) (cons current (append (cdr log) (spi-error part))))
      (else (format #t "# Unknown Part Type: ~a ~a~%" type part)
            log))))

;; Basic string conversion. Data numbers are hexadecimals in these.

(define (spi->number str)
  (let ((hex (if (string-prefix? "0x" str)
                 (substring str 2)
                 str)))
    (string->number hex 16)))

(define (convert-data x)
  (let ((key (car x))
        (value (cdr x)))
    (cons key
          (cond ((member key '(name type )) (string->symbol value))
                ((member key '(mosi miso)) (spi->number value))
                (else (catch #t
                        (lambda () (or (string->number value 10) value))
                        (lambda _ value)))))))

;; Frontends

(define* (read-log #:key file)
  (define (parse-from-stdin)
    (parse-protocol (current-input-port)))
  (let* ((raw (if file
                  (with-input-from-file file parse-from-stdin)
                  (parse-from-stdin)))
         (headline (parse-headline raw)))
    (map (lambda (x) (map convert-data (zip2 headline x)))
         (cdr raw))))

(define* (log->frames log #:key (with-timestamps? #t))
  (define (framer p l)
    (cs-framing p l with-timestamps?))
  (cdr (fold framer '(()) log)))

(define* (read-frames #:key file (with-timestamps? #t))
  (log->frames (read-log #:file file)
               #:with-timestamps? with-timestamps?))

(define (pick-frames log key)
  (filter (lambda (e) (eq? (car e) key)) log))
