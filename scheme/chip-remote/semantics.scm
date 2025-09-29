;; Copyright (c) 2017-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

;; Semantics are the way in chip-remote to specify how a datum in the human
;; readable world must be translated into a bit-sequence encoding for a given
;; item.
;;
;; Semantics can have an optional name. They have an encoder function and a
;; decoder function, which can be evaluation objects from the intpreter module,
;; as well as scheme functions. They have a range and a default value. The
;; latter is the only non-optional argument to the constructor ‘semantics’.
;;
;; The ‘name’ slot needs to be a symbol, and its default value is ‘none’.
;;
;; The ‘default’ slot must be a function that takes a semantics dataum and an
;; item width and produces the appropriate default value.
;;
;; The ‘range’ slot must contain a function, that takes two arguments: The
;; semantics datum itself and an item width, that the semantics are supposed to
;; be applied to. It must produce an encoding of the range of values that are
;; valid for the implemented semantics.
;;
;; See the documentation of the ‘semantics-range’ function for a description of
;; this function's return value. It is consumed by ‘semantics-in-range?’.
;;
;; If unspecified, this is a function that just returns `'(none)`, which encodes
;; that any value is applicable for the semantics. This is the least useful
;; range and real semantics values should specify a more appropriate range.
;;
;; The ‘encode’ and ‘decode’ slots contain the functions that perform the
;; generation and parsing of a bitstream into a human readable value. They are
;; at the core of the semantics' behaviour. Both functions must take two
;; arguments: A field width, and a value to be converted.
;;
;; The default decoder/encoder pair is able to look at range (by calling the
;; function in the ‘range’ slot), and if that specifies a table lookup, use
;; that table to do their work. Note that, while this helps, for most
;; convenience, use the ‘tbl’ function for generating semantics from lookup
;; tables.
;;
;; The ‘allow’ slot contains another function, that takes a width and a value.
;; It is used by the high level encoding process to further limit values from
;; the data yielded by the ‘range’ process. This can be useful to filter things
;; like "reserved" values from lookup table keys.
;;
;; Note that except for ‘semantics-name’, it is usually not needed to call any
;; of the other slot accessors. Instead the higher level API should be used.
;;
;; The high level API consists of these functions:
;;
;;   - `(semantics-decode SEMANTICS WIDTH VALUE)`
;;   - `(semantics-encode SEMANTICS WIDTH VALUE)`
;;   - `(semantics-default SEMANTICS WIDTH)`
;;   - `(semantics-range SEMANTICS WIDTH)`
;;   - `(semantics-in-range? SEMANTICS WIDTH VALUE)`
;;   - `(semantics-compose FUNCTION FUNCTION)`
;;   - `(table-lookup ASSOC-LIST-VALUE)`
;;   - `(tbl ASSOC-LIST-VALUE [#:default CONST-DEFAULT #:without LIST)`
;;
;; See their API documentation for details. For example implementations of the
;; semantic behaviour based on this module see the `(chip-remote codecs)`
;; module, as well as the device specifications of chip-remote, and also the
;; unit tests for this module.

(define-module (chip-remote semantics)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote interpreter)
  #:use-module (chip-remote named-value)
  #:use-module (chip-remote utilities)
  #:use-module (data-structures records)
  #:use-module (data-structures records utilities)
  #:export (semantics                   ; Data-type API
            make-semantics
            define-semantics
            semantics?
            semantics-name
            semantics-default*
            semantics-range*
            semantics-decoder
            semantics-encoder
            semantics-allow
            semantics-default           ; High Level API
            semantics-range
            semantics-compose
            semantics-decode
            semantics-encode
            semantics-in-range?
            table-lookup
            tbl))

(define-record-type* <semantics>
  semantics make-semantics semantics? this-semantics
  (name    semantics-name (default 'none) (sanitize (need 'name symbol?)))
  (range   semantics-range* (default (lambda (s w) '(none))))
  (default semantics-default*)
  (decode  semantics-decoder (thunked)
           (default (lambda (w v)
                     (default-decode this-semantics w v))))
  (encode  semantics-encoder (thunked)
           (default (lambda (w v)
                     (default-encode this-semantics w v))))
  (allow   semantics-allow (default (lambda (w v) #t))))

(new-record-definer define-semantics semantics)

;; High Level API

(define (semantics-decode s w v)
  "Invoke a semantics' decoder function.

Given a semantics datum S, a field width W, and a bit-string value V (as an
unsigned integer) that is corresponds to width W, this produces a human
readable value according to the behaviour encoded in the provided semantics.

Example:

    (semantics-decode unsigned-integer 8 255) → 255

The `unsigned-integer` semantics is defined in (chip-remote codecs)."
  (codec semantics-decoder s w v))

(define (semantics-encode s w v)
  "Invoke a semantics' encoder function.

Given a semantics datum S, a field with W, and a human-readable value V, that
is in the domain of the encoder function, this produces an unsigned integer
that encodes a bit-string of width W according to the behaviour encoded in the
provided semantics.

Example:

    (semantics-encode twos-complement 8 -2) → 254

The `twos-complement` semantics is defined in (chip-remote codecs)."
  (unless ((semantics-allow s) w v)
    (throw 'invalid-semantics-encoder-input v w s))
  (codec semantics-encoder s w v))

(define (semantics-range s w)
  "Return the range (or the domain) of the encoder function.

Given a semantics datum S and a field width W, this returns a value in one of
these forms:

  - `(none)`
  - `(range <minimum> <maximum>)`
  - `(enumeration <list-of-values>)`
  - `(table <association-list/named-value>)`

The `(none)` value encodes unlimited range. The `range` variant encodes a range
with a lower and an upper bound. These bounds are inclusive. The `enumeration`
variant names all possible input values into a function. And the `table`
variant allows the use of all keys into its provided lookup table as input
values.

Example:

The `twos-complement` semantics is defined in (chip-remote codecs)."
  ((semantics-range* s) s w))

(define (semantics-default s w)
  "Return the default value a sementics S, given a field width W.

Example:

    (semantics-default twos-complement 4) → 0

The `twos-complement` semantics is defined in (chip-remote codecs)."
  ((semantics-default* s) s w))

(define (semantics-in-range? s w v)
  "Test if V is in the range of the encoder of semantics S and field width W.

Examples:

    (semantics-in-range? twos-complement 4 -8) → #t
    (semantics-in-range? twos-complement 4 -9) → #f
    (semantics-in-range? twos-complement 4  7) → #t
    (semantics-in-range? twos-complement 4  8) → #f

The `twos-complement` semantics is defined in (chip-remote codecs)."
  (match (semantics-range s w)
    (('none) #t)
    (('range minimum maximum) (and (>= v minimum)
                                   (<= v maximum)))
    (('table table) (!! (memq v (map car (table-value table)))))
    (('enumeration . lst) (!! (memq v lst)))
    (_ #f)))

(define (semantics-compose a b)
  "Produce the composition of two encoder or decoder functions.

Normal function composition works on unary functions. Encoder and decoder
functions take two arguments, a width and a value. The width needs to be the
same in both composed function calls. So it is applied to both and the value
argument is chained from function to function.

Example:

    (define (invert w x) (logxorx 1))
    (define id (semantics-compose invert invert))
    (id 8 123) → 123"
  (lambda (w x)
    (a w (b w x))))

(define (table-lookup table)
  "Return a function, that returns ‘table’ range based on TABLE.

The return value of this function can be used to initialise the ‘range’ slot in
a semantics datum. If you need a full semantics datum based on a lookup table,
use the ‘tbl’ helper function instead."
  (lambda (s w) (list 'table table)))

(define* (tbl t #:key default without)
  "Return a semantics datum based on a lookup table T.

Given a lookup table T, return a semantics that encodes a key from this table
into a bit-string and decodes a bit-string back to one of those keys.

The `#:default` parameter can be used to name any key from the table as a the
default behaviour of the semantics datum. Note that this value does not have to
be wrapped in `(const ...)`. This is done internally. If this parameter is not
used, the semantics will use the first key in the lookup table as the default.

The `#:without` parameter can be used to supply a list of keys from the lookup
table that the encoding process should reject. This may be useful to weed out
uses of \"reserved\" keys and the like."
  (semantics (range (table-lookup t))
             (allow (if without
                        (lambda (w v) (not (member v without)))
                        (lambda (w v) #t)))
             (default (if default
                          (const default)
                          (const (caar (table-value t)))))))

;; Utilities

(define (table-value t)
  "Unwrap a named value if needed

In chip-remote many lookup tables are specifies as named-value, to facilitate
code-generation. The machinery itself also allows plain assoc lists as lookup
tables, however. This function can be used when both kinds of data are allowed
as input and only assoc lists are needed downstream."
  (if (named-value? t) (value-data t) t))

(define (decode-with-table table value)
  (let loop ((rest (table-value table)))
    (if (null? rest)
        'chip-remote:undefined
        (let ((k (caar rest))
              (v (cdar rest)))
          (if (= value v)
              k
              (loop (cdr rest)))))))

(define (encode-with-table table key)
  (let ((value (assoc key (table-value table))))
    (if value
        (cdr value)
        (throw 'cr/undefined key table))))

(define (default-codec f s w x)
  "Backend for default-encode and default-decode.

This function dispatches to the table based encoders and decoders depending in
the supplied semantics' range. If this is not the case, the encode/decode pair
implemented by this function is the identity function."
  (let ((range (semantics-range s w)))
    (match range
      (('table table) (f table x))
      (_ x))))

(define (default-encode s w x)
  "This is the default encoder function for semantics objects.

The the semantics wraps a lookup table, this implements encoding based on that
table. Otherwise the encoding process is analogous to the identity function."
  (default-codec encode-with-table s w x))

(define (default-decode s w x)
  "This is the default decoder function for semantics objects.

The the semantics wraps a lookup table, this implements decoding based on that
table. Otherwise the decoding process is analogous to the identity function."
  (default-codec decode-with-table s w x))

(define (codec a s w v)
  "This is the backend to semantics-encode and semantics-decode.

The A parameter is an accessor to an semantics object S. In this case it needs
to be either semantics-decoder or semantics-encoder. These will yield the
function that implements the required conversion. This function is then applied
to the remaining values, namely a field width W and in input value V."
  (let ((f (a s)))
    (cond ((procedure? f) (f w v))
          ((evaluation? f) ((evaluation-value f) w v))
          (else (throw 'cr/unknown-semantics-type f)))))
