;; This is the reader part of https://github.com/NalaGinrut/guile-csv:

;; guile-csv
;; Copyright (C) 2008, 2012, 2013
;; Andy Wingo <wingo at pobox dot com>
;; Nala Ginrut <nalaginrut@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program; if not, contact:
;;
;; Free Software Foundation, Inc.     Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org

(define-module (protocol csv)
  #:use-module (ice-9 optargs)
  #:export (make-csv-reader))

(define (csv-read-row port delimiter have-cell init-seed)
  (define (!)
    (let ((c (read-char port)))
      c))
  (define (finish-cell b seed)
    (have-cell (list->string (reverse b)) seed))
  (define (next-cell b seed)
    (state-init (!) (finish-cell b seed)))
  (define (state-init c seed)
    (cond ((eqv? c delimiter) (state-init (!) (have-cell "" seed)))
          ((eqv? c #\") (state-string (!) '() seed))
          ((eqv? c #\newline) seed)
          ((eof-object? c) seed)
          (else (state-any c '() seed))))
  (define (state-string c b seed)
    (cond ((eqv? c #\") (state-string-quote (!) b seed))
          ((eof-object? c) (error "Open double-quoted string"
                                  (list->string (reverse b))))
          (else (state-string (!) (cons c b) seed))))
  (define (state-string-quote c b seed)
    (cond ((eqv? c #\") (state-string (!) (cons c b) seed))
          ((eqv? c delimiter) (next-cell b seed))
          ((eqv? c #\newline) (finish-cell b seed))
          ((eof-object? c) (finish-cell b seed))
          (else (error "Single double quote at unexpected place." c b))))
  (define (state-any c b seed)
    (cond ((eqv? c delimiter) (next-cell b seed))
          ((eqv? c #\newline) (finish-cell b seed))
          ((eof-object? c) (finish-cell b seed))
          (else (state-any (!) (cons c b) seed))))
  (state-init (!) init-seed))

(define (csv-read port delimiter new-row have-cell have-row init-seed)
  (let lp ((seed init-seed))
    (cond
     ((eof-object? (peek-char port)) seed)
     (else (lp (have-row (csv-read-row port delimiter have-cell (new-row seed))
                         seed))))))

(define* (make-csv-reader delimiter #:key
                          (new-row (lambda (rows) '()))
                          (have-cell (lambda (cell row)
                                       (cons cell row)))
                          (have-row (lambda (row rows)
                                      (cons (list->vector (reverse row)) rows)))
                          (init-seed '()))
  (lambda (port)
    (reverse
     (csv-read port delimiter new-row have-cell have-row init-seed))))
