;; -*- scheme -*-

;; Copyright (c) 2011-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap))
(primitive-load "tests/test-tap-cfg.scm")

(define er (@@ (chip-remote protocol) expect-read))

(define working-expects '(("VERSION 2 7 ca"
                           ("VERSION" int int int)
                           ("VERSION" 2 7 202))
                          ("LINES 17 FIXED"
                           ("LINES" int opt "FIXED")
                           ("LINES" 23 "FIXED"))
                          ("LINES 17"
                           ("LINES" int opt "FIXED")
                           ("LINES" 23))))

(define failing-expects '(("VERSION 2 7z ca"
                           protocol-unexpected-data
                           ("VERSION" int int int)
                           ;; This last entry is currently not used by the
                           ;; tests below. However, it may become useful, as
                           ;; soon as scm-tap-test supports inspecting the
                           ;; arguments of an exception.
                           ("7z" . int))
                          ("LINES 17 FOXED"
                           protocol-unexpected-data
                           ("LINES" int opt "FIXED")
                           ("FOXED" . "FIXED"))
                          ("LINES 17 FIXED REALLY"
                           protocol-number-of-words-mismatch
                           ("LINES" int opt "FIXED")
                           ("REALLY" . too-much))
                          ("LINES"
                           protocol-number-of-words-mismatch
                           ("LINES" int opt "FIXED")
                           ("" . not-enough-words))))

(with-fs-test-bundle
 (plan (+ (length working-expects)
          (length failing-expects)))

 (map (lambda (x)
        (let* ((input (car x))
               (spec (cadr x))
               (expected (caddr x))
               (result (er input spec)))
          (define-test (format #f "expect-read succeeds: ~s" input)
            (pass-if-equal? result expected))))
      working-expects)

 (map (lambda (x)
        (let* ((input (car x))
               (excp (cadr x))
               (spec (caddr x))
               (failure (cadddr x)))
          (define-test (format #f "expect-read fails: ~s" input)
            (pass-if-exception excp (er input spec)))))
      failing-expects))
