;; Copyright (c) 2019-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote register common)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote item)
  #:use-module (chip-remote register)
  #:export (generate-simple-register
            define-simple-register
            generate-u8-register
            define-u8-register
            generate-u16-register
            define-u16-register
            generate-u32-register
            define-u32-register
            generate-u64-register
            define-u64-register
            generate-s8-register
            define-s8-register
            generate-s16-register
            define-s16-register
            generate-s32-register
            define-s32-register
            generate-s64-register
            define-s64-register
            generate-f32-register
            define-f32-register
            generate-f64-register
            define-f64-register))

(define-syntax-rule (generate-simple-register name* width* rest ...)
  († (item (name 'name*) (offset 0) (width width*) rest ...)))

(define-syntax-rule (define-simple-register name width rest ...)
  (define name (generate-simple-register name width rest ...)))

(define-syntax-rule (generate-u8-register name rest ...)
  (generate-simple-register name 8 (semantics unsigned-integer) rest ...))

(define-syntax-rule (define-u8-register name rest ...)
  (define name (generate-u8-register name rest ...)))

(define-syntax-rule (generate-u16-register name rest ...)
  (generate-simple-register name 16 (semantics unsigned-integer) rest ...))

(define-syntax-rule (define-u16-register name rest ...)
  (define name (generate-u16-register name rest ...)))

(define-syntax-rule (generate-u32-register name rest ...)
  (generate-simple-register name 32 (semantics unsigned-integer) rest ...))

(define-syntax-rule (define-u32-register name rest ...)
  (define name (generate-u32-register name rest ...)))

(define-syntax-rule (generate-u64-register name rest ...)
  (generate-simple-register name 64 (semantics unsigned-integer) rest ...))

(define-syntax-rule (define-u64-register name rest ...)
  (define name (generate-u64-register name rest ...)))

(define-syntax-rule (generate-s8-register name rest ...)
  (generate-simple-register name 8 (semantics twos-complement) rest ...))

(define-syntax-rule (define-s8-register name rest ...)
  (define name (generate-s8-register name rest ...)))

(define-syntax-rule (generate-s16-register name rest ...)
  (generate-simple-register name 16 (semantics twos-complement) rest ...))

(define-syntax-rule (define-s16-register name rest ...)
  (define name (generate-s16-register name rest ...)))

(define-syntax-rule (generate-s32-register name rest ...)
  (generate-simple-register name 32 (semantics twos-complement) rest ...))

(define-syntax-rule (define-s32-register name rest ...)
  (define name (generate-s32-register name rest ...)))

(define-syntax-rule (generate-s64-register name rest ...)
  (generate-simple-register name 64 (semantics twos-complement) rest ...))

(define-syntax-rule (define-s64-register name rest ...)
  (define name (generate-s64-register name rest ...)))

(define-syntax-rule (generate-f32-register name rest ...)
  (generate-simple-register name 32 (semantics ieee-754-single) rest ...))

(define-syntax-rule (define-f32-register name rest ...)
  (define name (generate-f32-register name rest ...)))

(define-syntax-rule (generate-f64-register name rest ...)
  (generate-simple-register name 64 (semantics ieee-754-double) rest ...))

(define-syntax-rule (define-f64-register name rest ...)
  (define name (generate-f64-register name rest ...)))
