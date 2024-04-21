;; -*- scheme -*-

(define (ft thing)
  (cons thing (format #f "https://github.com/ft/~a.git" thing)))

(map ft '(guile-tap guile-termios makemehappy test-dispatch))
