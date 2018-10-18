;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices microchip mcp43xx registers)
  #:use-module (chip-remote item)
  #:use-module (chip-remote item builder)
  #:use-module (chip-remote register)
  #:use-module (chip-remote semantics)
  #:use-module (chip-remote utilities)
  #:export (reg:wiper-0
            reg:wiper-1
            reg:terminal-ctrl-0
            reg:wiper-2
            reg:wiper-3
            reg:terminal-ctrl-1))

(define-register reg:wiper-0
  #:width 9
  #:contents (wiper-0 0 9 #:default #x040))

(define-register reg:wiper-1
  #:width 9
  #:contents (wiper-1 0 9 #:default #x040))

(define-register reg:wiper-2
  #:width 9
  #:contents (wiper-2 0 9 #:default #x040))

(define-register reg:wiper-3
  #:width 9
  #:contents (wiper-3 0 9 #:default #x040))

(define* (make-terminal-pin #:key offset idx terminal)
  (generate-item #:name (symbol-append 'res- (number->symbol idx)
                                       '-term- terminal '-connect?)
                 #:offset offset
                 #:width 1
                 #:semantics boolean
                 #:default #t))

(define* (make-wiper-pin #:key offset idx)
  (generate-item #:name (symbol-append 'res- (number->symbol idx)
                                       '-wiper-connect?)
                 #:offset offset
                 #:width 1
                 #:semantics boolean
                 #:default #t))

(define* (make-enabled-bit #:key offset idx)
  (generate-item #:name (symbol-append 'res- (number->symbol idx) '-enabled?)
                 #:offset offset
                 #:width 1
                 #:semantics boolean
                 #:default #t))

(define-register reg:terminal-ctrl-0
  #:width 9
  #:contents*
  (make-terminal-pin #:offset 0 #:idx 0 #:terminal 'b)
  (make-wiper-pin    #:offset 1 #:idx 0)
  (make-terminal-pin #:offset 2 #:idx 0 #:terminal 'a)
  (make-enabled-bit  #:offset 3 #:idx 0)
  (make-terminal-pin #:offset 4 #:idx 1 #:terminal 'b)
  (make-wiper-pin    #:offset 5 #:idx 1)
  (make-terminal-pin #:offset 6 #:idx 1 #:terminal 'a)
  (make-enabled-bit  #:offset 7 #:idx 1)
  (reserved 8 1 #:default #t))

(define-register reg:terminal-ctrl-1
  #:width 9
  #:contents*
  (make-terminal-pin #:offset 0 #:idx 2 #:terminal 'b)
  (make-wiper-pin    #:offset 1 #:idx 2)
  (make-terminal-pin #:offset 2 #:idx 2 #:terminal 'a)
  (make-enabled-bit  #:offset 3 #:idx 2)
  (make-terminal-pin #:offset 4 #:idx 3 #:terminal 'b)
  (make-wiper-pin    #:offset 5 #:idx 3)
  (make-terminal-pin #:offset 6 #:idx 3 #:terminal 'a)
  (make-enabled-bit  #:offset 7 #:idx 3)
  (reserved 8 1 #:default #t))
