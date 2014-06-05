;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti ads4149 registers)
  #:use-module (chip-remote bit-decoders)
  #:use-module (chip-remote devices ti ads4149 tables)
  #:use-module (chip-remote register-map)
  #:export (register-width
            valid-register-address?))

(define (valid-register-address? a)
  (member a (map car ads4149-register-map)))

(define (posn-decode width value get cmos lvds)
  (let ((v (get value)))
    (list (cons 'lvds-mode (reverse-lookup lvds value))
          (cons 'cmos-mode (reverse-lookup cmos value)))))

(define (rise-posn-decode name offset width value)
  (posn-decode width
               value
               get-clkout-rise-posn-bits
               clkout-pos-rise-cmos
               clkout-pos-rise-lvds))

(define (fall-posn-decode name offset width value)
  (posn-decode width
               value
               get-clkout-fall-posn-bits
               clkout-pos-fall-cmos
               clkout-pos-fall-lvds))

;; ‘sym-or-picosecs’ is a little more involved, since the decoders, that are
;; implemented above return lists, representing the different possible decoded
;; values. The actual value depends on some other item in another register. So,
;; if we see a list as the decoded value, hand down a function that decides
;; wether or not to append a unit when the list of different possibilities is
;; processed. That's what ‘sym-or-picosecs*’ is for.
(define (sym-or-picosecs* value)
  (cond ((symbol? value) #f)
        (else 'ps)))

(define (sym-or-picosecs data)
  (let ((value (assq-ref data 'decoded)))
    (if (list? value)
        sym-or-picosecs*
        (sym-or-picosecs* value))))

;; The unit-functions using ‘maybe-unit’ here are much simpler. They are used
;; in cases in which the decoded value is either a symbol or a numeric scalar.
;; If it's the symbol, don't add the requested unit, otherwise do add it.
(define (maybe-unit data unit)
  (let ((value (assq-ref data 'decoded)))
    (if (symbol? value) #f unit)))

(define (maybe-dB data)
  (maybe-unit data 'dB))

(define (maybe-M data)
  (maybe-unit data 'M))

;; The width of every register:
(define register-width 8)

;; Here is the actual register map:
(define-register-map ads4149
  (#x0 (default-value #x0)
       (contents (readout 0 1 => logic-active-high)
                 (reset 1 1 => logic-active-high)))
  (#x1 (default-value #x0)
       (contents (lvds-swing 2 6 => lvds-swing-map 'mV)))
  (#x3 (default-value #x0)
       (contents (high-performance-mode-1 0 2 => high-performace-mode-map)))
  (#x25 (default-value #x0)
        (contents (test-pattern 0 3 => test-pattern-map)
                  (disable-gain 3 1 => logic-active-high)
                  (gain 4 4 => gain-map maybe-dB)))
  (#x26 (default-value #x0)
        (contents (lvds-data-strength 0 1 => lvds-strength-map)
                  (lvds-clkout-strength 1 1 => lvds-strength-map)))
  (#x3d (default-value #x0)
        (contents (enable-offset-correction 5 1 => logic-active-high)
                  (data-format 6 2 => data-format-map)))
  (#x3f (default-value #x0)
        (contents (custom-pattern-high 0 8)))
  (#x40 (default-value #x0)
        (contents (custom-pattern-low 2 6)))
  (#x41 (default-value #x0)
        (contents (enable-clkout-fall 0 1 => logic-active-high)
                  (clkout-rise-posn 1 2 => rise-posn-decode sym-or-picosecs)
                  (enable-clkout-rise 3 1 => logic-active-high)
                  (cmos-clkout-strength 4 2 => lvds-strength-map)
                  (lvds-cmos 6 2 => lvds-cmos-select-map)))
  (#x42 (default-value #x0)
        (contents (standby 2 1 => logic-active-high)
                  (disable-low-latency 3 1 => logic-active-high)
                  (clkout-fall-posn 6 2 => fall-posn-decode sym-or-picosecs)))
  (#x43 (default-value #x0)
        (contents (enable-lvds-swing 0 2 => lvds-swing-control-map)
                  (power-down-outputs 4 1 => logic-active-high)
                  (power-down-global 6 1 => logic-active-high)))
  (#x4a (default-value #x0)
        (contents (high-performance-mode-2 0 1 => logic-active-high)))
  (#xbf (default-value #x0)
        (contents (offset-pedestal 2 6 => twos-complement)))
  (#xcf (default-value #x0)
        (contents (offset-correction-time-constant
                   2 4 => correction-time-map maybe-M)
                  (freeze-offset-correction 7 1 => logic-active-high)))
  (#xdf (default-value #x0)
        (contents (low-speed 4 2 => low-speed-map))))

;; When the third-level API decodes the entire device, it needs to know how
;; some of the entries from the register-map are connected in order to make a
;; final decoding.

(define (decode-hpm name combined)
  (if (= combined #b111)
      #t
      #f))

(define (combine-hpm x)
  (let ((a (car (assq-ref x 'high-performance-mode-1)))
        (b (car (assq-ref x 'high-performance-mode-2))))
    (logior (ash b 2) a)))

(define (decode-custom-pattern name values)
  (literal-binary 'custom-pattern 0 (+ 8 6) values))

(define (combine-custom-pattern x)
  (let ((low (car (assq-ref x 'custom-pattern-low)))
        (high (car (assq-ref x 'custom-pattern-high))))
    (logior (ash high 6) low)))

(define (decode-clkout-fall-posn name bits raw decoded)
  (let ((type (assq-ref decoded 'lvds-cmos)))
    (cond ((eq? type 'dfs-pin) 'dfs-pin-decides)
          ((eq? type 'lvds) (list (reverse-lookup clkout-pos-fall-lvds bits)
                                  'ps))
          (else (list (reverse-lookup clkout-pos-fall-cmos bits) 'ps)))))

(define (decode-clkout-rise-posn name bits raw decoded)
  (let* ((type (assq-ref decoded 'lvds-cmos))
         (v (cond ((eq? type 'dfs-pin) 'dfs-pin-decides)
                  ((eq? type 'lvds) (reverse-lookup clkout-pos-rise-lvds bits))
                  (else (list (reverse-lookup clkout-pos-rise-cmos bits) 'ps)))))
    (if (number? v)
        (list v 'ps)
        v)))

(define-register-interconns ads4149
  (combine (high-performance-mode-1 high-performance-mode-2)
           #:into high-performance-mode
           #:combine combine-hpm
           #:logic boolean
           #:finally decode-hpm)
  (combine (custom-pattern-high custom-pattern-low)
           #:into custom-pattern
           #:combine combine-custom-pattern
           #:finally decode-custom-pattern)
  (depends clkout-rise-posn
           #:on lvds-cmos
           #:finally decode-clkout-rise-posn)
  (depends clkout-fall-posn
           #:on lvds-cmos
           #:finally decode-clkout-fall-posn))
