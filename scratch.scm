(use-modules (chip-remote device)
             (chip-remote item)
             (chip-remote register)
             (chip-remote devices linear-technology ltc6603)
             (chip-remote devices decawave dw1000)
             (chip-remote pretty-print)
             (chip-remote semantics)
             (ice-9 match)
             (ice-9 pretty-print))

(pretty-print ltc6603)
;;(pretty-print dw1000)

;;(pp-eval (current-output-port)
;;         (pp-semantics (item-semantics (device-address ltc6603 #f #f 0))))

;;(pretty-print (item-semantics (device-address ltc6603 #f #f 0)))


;;(pp-eval (current-output-port)
;;         '(wrap "#<" ">"
;;                (type item) (newline)
;;                (indent complex
;;                        (key name) (space enable-output?) (newline)
;;                        (key offset) (space 0) (newline)
;;                        (key width) (space 1) (newline)
;;                        (key semantics) (newline)
;;                        (indent complex
;;                                (wrap "#<" ">"
;;                                      (type semantics) (newline)
;;                                      (indent complex
;;                                              (key name) (space #f) (newline)
;;                                              (key type) (space boolean) (newline)
;;                                              (key data) (space #f) (newline)
;;                                              (key encode) (space ...) (newline)
;;                                              (key decode) (space ...))))
;;                        (key access) (space ...) (newline)
;;                        (key meta) (space ()) (newline)
;;                        (key get) (space ...) (newline)
;;                        (key set) (space ...))))
