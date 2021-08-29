;; Copyright (c) 2011-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote protocol)
  #:use-module (chip-remote io)
  #:use-module (srfi srfi-1)
  #:export (address
            bye
            capabilities
            client-version
            focus
            firmware-version
            hi
            init
            line
            lines
            modes
            port
            ports
            protocol-version
            set
            transmit
            update-capabilities))

;; Protocol version of this implementation
(define client-version '((major . 3)
                         (minor . 0)
                         (micro . 0)))

(define (protocol-tokenize string)
  "Take ‘string’ and return a list of tokens."
  (string-tokenize string protocol-char-set))

(define (multi-tokenize string)
  (string-split string #\;))

(define (protocol-read conn)
  "Read a reply from the connection ‘conn’ and return it.

This function throws exceptions for the common failure the protocol specifies."
  (let ((reply (io-read conn)))
    (if (or (string-prefix? "wtf" reply)
            (string-prefix? "malformed-command" reply)
            (string-prefix? "broken-value" reply)
            (string-prefix? "value-out-of-range" reply))
        (let* ((tokens (protocol-tokenize reply))
               (cause (car tokens))
               (len (+ 1 (string-length cause)))
               (rest (if (< len (string-length reply))
                         (substring reply len)
                         ""))
               (excp (symbol-append 'protocol-
                                    (string->symbol (car tokens)))))
          (throw excp reply rest tokens)))
    reply))

(define (zip2 la lb)
  "This is like ‘zip’ from `(srfi srfi-1)`, except that it returns a list of
pairs instead of a list of lists with two elements in them.

    (zip2 '(a c e) '(b d f)) => ((a . b) (c . d) (e . f))"
  (let next ((a la)
             (b lb)
             (acc '()))
    (cond ((any null? (list a b))
           (reverse acc))
          (else
           (next (cdr a) (cdr b)
                 (cons (cons (car a) (car b))
                       acc))))))

;; Words in the protocol may contain letters, a dash and digits from ASCII.
(define protocol-char-set
  (char-set-union
   (char-set-diff+intersection char-set:ascii
                               char-set:letter+digit)
   (char-set #\+ #\- #\:)))

(define (hexstring->int str)
  "Turn a hexadecimal string ‘str’ into an integer. Returns ‘#f’ in case of an
error."
  (string->number str 16))

(define (int->hexstring int)
  "Turn an integer ‘int’ into a string of hexadecimal digits."
  (number->string int 16))

(define (verify-and-convert pair)
  "Take ‘pair’ that looks like this: `(VALUE . SPEC)`

- If the ‘SPEC’ is a string, the ‘VALUE’ must be the same string.

- If the ‘VALUE’ is an integer, the first must by a valid hexstring.

Returns a pair: `(BOOLEAN . VALUE)`

‘BOOLEAN’ is either ‘#t’ or ‘#f’ depending on whether the verification
succeeded. If ‘BOOLEAN’ is ‘#f’, ‘VALUE’ is the unchanged input data.
Otherwise, ‘VALUE’ is the the possibly converted data (for example an
integer)."
  (let ((got (car pair))
        (want (cdr pair)))
    (cond ((string? want)
           (cons (string=? want got)
                 got))
          ((eq? want 'int)
           (let* ((val (hexstring->int got))
                  (success (integer? val)))
             (cons success
                   (if success val got))))
          ((eq? want 'string)
           (cons #t got))
          (else
           (cons #f got)))))

(define (expect-read string spec)
  "Check whether ‘string’ meets the specification in ‘spec’.

‘string’ is a raw string from the protocol, that gets tokenised and then
checked by ‘verify-and-convert’ with the corresponding specification item from
‘spec’.

‘spec’ supports fixed length of input tokens as well as optional tokens. For
example, with the \"VERSION\" reply ‘spec’ looks like this:

    (\"VERSION\" int int int)

That means, that ‘string’ has to contain exactly four tokens, the first of
which has to be the string \"VERSION\" and the next three tokens have to be
strings of hexadecimal digits. Thus:

    (expect-read \"VERSION a b c\" '(\"VERSION\" int int int))
    => (\"VERSION\" 10 11 12)

To specify optional tokens you put the symbol ‘opt’ before the first optional
token in SPEC. For example with (\"LINES\" int opt \"FIXED\"), ‘string’ has to
contain either two or three tokens. The first has to be the string \"LINES\",
the second has to be a string of hexadecimal digits and the third (if it
exists) has to be the string \"FIXED\". So:

    (expect-read \"lines c\" '(\"lines\" int opt \"fixed\"))
    => (\"lines\" 12)

    (expect-read \"lines c fixed\" '(\"lines\" int opt \"fixed\"))
    => (\"lines\" 12 \"fixed\")

The function throws exceptions in the following cases:

- protocol-number-of-words-mismatch: In case the number of tokens in ‘string’
  and SPEC don't match up.

- protocol-unexpected-data: Thrown in case ‘verify-and-convert’ for a token
  failed."
  (let ((tokens (protocol-tokenize string)))
    (let ((opt-pos (list-index (lambda (x) (eq? x 'opt)) spec))
          (lt (length tokens))
          (lw (length spec)))
      (unless (or (and opt-pos (>= lt opt-pos) (< lt lw))
                  (and (not opt-pos) (eq? lw lt)))
        (throw 'protocol-number-of-words-mismatch string tokens spec)))
    (reverse
     (fold (lambda (new acc)
             (let ((v (verify-and-convert new)))
               (unless (eq? (car v) #t)
                 (throw 'protocol-unexpected-data new))
               (cons (cdr v) acc)))
           '()
           (zip2 (protocol-tokenize string)
                 (filter (lambda (x) (not (eq? x 'opt))) spec))))))

(define (push-capability-and-return conn key value)
  "Set the data for the item named ‘key’ to ‘value’ in the data cache for the
connection ‘conn’."
  (set-cr-capability! conn key value)
  value)

;; Read from the device, save the reply and run code in case the read was
;; successful. Return ‘#f’ otherwise.
(define-syntax with-read-raw-string
  (lambda (x)
    (syntax-case x ()
      ((_ (c r) code ...)
       #'(let ((r (protocol-read c)))
           code ...)))))

(define (hi conn)
  "Initiate an `RCCEP` conversation on ‘conn’. In case the reply doesn't match
the protocol specification, the function throws ‘protocol-hi-failed’.
Otherwise, #t is returned."
  (io-write conn "hi")
  (with-read-raw-string (conn reply)
    (unless (string=? reply "Hi there, stranger.")
      (throw 'protocol-hi-failed reply))
    #t))

(define (bye conn)
  "End an `RCCEP` conversation on ‘conn’. In case the reply doesn't match the
protocol specification, the function throws ‘protocol-bye-failed’. Otherwise,
#t is returned."
  (io-write conn "bye")
  (with-read-raw-string (conn reply)
    (unless (string=? reply "Have a nice day.")
      (throw 'protocol-bye-failed reply))
    #t))

(define (capabilities-parse tokens)
  (define keys '(rx-buffer-size maximum-arguments))
  (let ((fst (string->symbol (car tokens))))
    (if (memq fst keys)
        (cons fst (map hexstring->int (cdr tokens)))
        fst)))

(define (capability-categories lst)
  (let loop ((rest lst) (kv '()) (ext '()))
    (if (null? rest)
        (cons (cons 'extensions (sort ext (lambda (a b)
                                            (string< (symbol->string a)
                                                     (symbol->string b)))))
              (sort kv (lambda (a b)
                         (string< (symbol->string (car a))
                                  (symbol->string (car b))))))
        (let  ((entry (car rest)))
          (if (list? entry)
              (loop (cdr rest) (cons entry kv) ext)
              (loop (cdr rest) kv (cons entry ext)))))))

(define (capabilities conn)
  (io-write conn "capabilities")
  (for-each (lambda (x) (set-cr-capability! conn (car x) (cdr x)))
            (capability-categories
             (map (compose capabilities-parse protocol-tokenize)
                  (multi-tokenize (protocol-read conn)))))
  (cr-capabilities conn))

;; Query protocol version from the board.
(define (protocol-version conn)
  "Issue the ‘version’ request via ‘conn’ and return an alist, that looks like
this:

    ((major . 23)
     (minor . 42)
     (micro . 666))"
  (io-write conn "version")
  (push-capability-and-return
   conn 'protocol-version
   (zip2 '(major minor micro)
         (cdr (with-read-raw-string (conn reply)
                (expect-read reply '("VERSION" int int int)))))))

(define (firmware-version conn)
  "Issue the ‘+version’ request via ‘conn’ and return an alist, that looks like
this:

    ((major . 23)
     (minor . 42)
     (micro . 666))

Note that all requests prefixed by a ‘+’ are extensions to the RCCEP protocol.
The ‘+version’ is an extension that returns the firmware's version in terms of
the semantic versioning scheme (https://semver.org/).

The reply should contain three tokens, all integer, reflecting the firmware's
version."
  (io-write conn "+version")
  (push-capability-and-return
   conn 'firmware-version
   (zip2 '(major minor micro)
         (with-read-raw-string (conn reply)
           (expect-read reply '(int int int))))))

(define (request->list-of-symbols conn request)
  "Apply ‘string->symbol’ to a list of strings and return a list of the
resulting symbols."
  (io-write conn request)
  (map string->symbol (multi-tokenize (protocol-read conn))))

(define (string+int->pair s)
  "Take a string (‘s’)with two tokens (a string and an integer represented as a
string of hexadecimal digits) and turn it into a pair of a lower-cased symbol
and an integer: (string+int->pair \"foo e\") => (foo . 14)"
  (let ((l (expect-read s '(string int))))
    (cond ((not (list? l)) l)
          (else (cons (string->symbol (car l))
                      (cadr l))))))

(define (ports conn)
  "Via ‘conn’, query the remote controller for a list of ports it contains."
  (io-write conn "ports")
  (push-capability-and-return
   conn 'ports (map string+int->pair (multi-tokenize (protocol-read conn)))))

(define (modes conn)
  "Via ‘conn’, query the remote controller for a list of modes it implements."
  (push-capability-and-return conn 'modes
                              (request->list-of-symbols conn "modes")))

(define (transmit conn data)
  "Instruct the remote controller (connected to via ‘conn’) to transmit the
word ‘data’ (an integer) to using its currently focused port. If the controller
receives data from the slave device while transmitting the ‘data’ word, the
function returns the received data. If nothing is reveived, 0 is returned."
  (protocol-transmit conn (int->hexstring data)))

(define (protocol-transmit conn string)
  "Like ‘transmit’, only that ‘string’ is a string representation of the data to
be transmitted (encoded as an string of hexadecimal digits)."
  (io-write conn (string-concatenate (list "transmit " string)))
  (with-read-raw-string (conn reply)
    (car (expect-read reply '(int)))))

(define (request-expects-ok conn request)
  "Send a request to a controller via ‘conn’.

Throw ‘protocol-expected-ok’ in case the reply from the controller was not the
fixed string \"ok\". Otherwise, return #t."
  (io-write conn request)
  (let ((reply (io-read conn)))
    (unless (string=? reply "ok")
      (throw 'protocol-expected-ok request reply))
    #t))

(define (request-with-index request index)
  "Assemble a request string that consists of ‘request’ and the properly
represented integer ‘index’ separated by an `ASCII` space."
  (string-concatenate (list request " " (int->hexstring index))))

(define (request-with-index-to-ok conn request index)
  "Place a request to a remote controller connected to via ‘conn’, that
consists of ‘request’ and ‘index’. The controller is expected to reply \"ok\"."
  (request-expects-ok conn (request-with-index request index)))

(define request-with-integer request-with-index)
(define request-with-integer-to-ok request-with-index-to-ok)

(define (address conn addr)
  (request-with-integer-to-ok conn "address" addr))

(define (focus conn index)
  "Instruct (via ‘conn’) a remote controller to focus the port indexed by
‘index’."
  (request-with-index-to-ok conn "focus" index))

(define (init conn index)
  "Instruct (via ‘conn’) a remote controller to initialise the port indexed by
‘index’."
  (request-with-index-to-ok conn "init" index))

(define (update-capabilities conn)
  "Gather as much information from a remote controller, connected to via
‘conn’."
  (protocol-version conn)
  (modes conn)
  (ports conn)
  #t)

(define (zip-apply lf ld)
  "Like ‘zip’, but instead of constructing a resulting list, use elements from
‘lf’ as functions and apply them to the corresponding element in ‘ld’. Return a
list of return values from those function applications.

    (zip-apply (list (lambda (x) (* x x))
                     (lambda (x) (+ x x))
                     (lambda (x) (- (* x x)
                                    (+ x x))))
               (list 5 4 3))
    => (25 8 3)"
  (map (lambda (x)
         (let ((fnc (car x))
               (dat (cdr x)))
           (if fnc
               (fnc dat)
               dat)))
       (zip2 lf ld)))

(define (parse-line-symbol str)
  "Take a line-role token and turn it into a pair:

    (parse-line-symbol \"foo\") => (foo . 0)
    (parse-line-symbol \"foo:0\") => (foo . 0)
    (parse-line-symbol \"foo:3\") => (foo . 3)"
  (let* ((data (string-split str #\:))
         (len (length data)))
    (cond ((= len 1) (cons (string->symbol (car data)) 0))
          ((= len 2) (cons (string->symbol (car data))
                           (let ((int (hexstring->int (cadr data))))
                             (if int int
                                 (throw 'protocol-line-index-not-integer
                                        (cadr data))))))
          (else (throw 'protocol-invalid-lines-reply str data)))))

(define (parse-lines reply)
  "Take a reply to the ‘lines’ request and turn it into scheme data:

    (parse-lines (list \"0 clk fixed\"))
    => (0 (clk . 0) fixed)

    (parse-lines (list \"1 cs\"))
    => (0 (clk . 0))

    (parse-lines (list \"2 cs:1\"))
    => (0 (clk . 1))"
  (zip-apply (list #f parse-line-symbol string->symbol)
             (expect-read reply '(int string opt "fixed"))))

(define (lines conn index)
  "Query the line setup of the port indexed by ‘index’ from a remote controller
connected to via ‘conn’."
  (io-write conn (request-with-index "lines" index))
  (map parse-lines (multi-tokenize (protocol-read conn))))

(define (fixed->symbol string)
  "Turn ‘string’ into a symbol and make sure it is the ‘fixed’ symbol."
  (let ((sym (string->symbol string)))
    (unless (eq? sym 'fixed)
      (throw 'protocol-unallowed-keyword string sym 'fixed))
    sym))

(define (return-allowed-symbol ls str)
  "Turn the string ‘str’ into a symbol and make sure it is a member of the list
of allowed symbols supplied as ‘ls’."
  (let ((sym (string->symbol str)))
    (if (memq sym ls)
        sym
        (throw 'protocol-unallowed-keyword str sym ls))))

(define (port-bit-order str)
  "Make sure that ‘str’ is an allowed setting for the `bit-order` setting."
  (return-allowed-symbol '(msb-first lsb-first) str))

(define (port-clk-polarity str)
  "Make sure that ‘str’ is an allowed setting for the `clk-polarity` setting."
  (return-allowed-symbol '(rising-edge falling-edge) str))

(define (port-cs-polarity str)
  "Make sure that ‘str’ is an allowed setting for the `cs-polarity` setting."
  (return-allowed-symbol '(active-high active-low) str))

(define (port-mode str)
  "Make sure that ‘str’ is an allowed setting for the `mode` setting."
  (return-allowed-symbol '(spi) str))

(define (reply->boolean str)
  "Make sure that ‘str’ is a boolean."
  (let ((reply (string->symbol str)))
    (case reply
      ((true on) #t)
      ((false off) #f)
      (else (throw 'protocol-not-a-boolean str reply)))))

(define parse-port-table
  `((bit-order . ,port-bit-order)
    (clk-phase-delay . ,reply->boolean)
    (clk-polarity . ,port-clk-polarity)
    (cs-lines . ,hexstring->int)
    (cs-polarity . ,port-cs-polarity)
    (frame-length . ,hexstring->int)
    (lines . ,hexstring->int)
    (mode . ,port-mode)
    (rate . ,hexstring->int)))

(define (parse-port reply)
  "Turn a reply to the `port` request to scheme data."
  (let* ((orig (protocol-tokenize reply))
         (data (cons (string->symbol (car orig)) (cdr orig))))
    (zip-apply (list #f
                     (assq-ref parse-port-table (car data))
                     fixed->symbol)
               data)))

(define (port conn index)
  "Query the port setup of the port indexed by ‘index’ from a remote controller
connected to via ‘conn’."
  (io-write conn (request-with-index "port" index))
  (map parse-port (multi-tokenize (protocol-read conn))))

(define (boolean->protocol-string bool)
  (if bool "true" "false"))

(define (set conn pidx key value)
  "Issue a `set` request to a remote controller connected to via ‘conn’.

‘pidx’ is the index of the port the setting named by ‘key’ should be changed
in. The value for ‘key’ will be set to ‘value’."
  (let ((key-str (symbol->string key))
        (idx-str (int->hexstring pidx))
        (val (cond ((symbol? value) (symbol->string value))
                   ((boolean? value) (boolean->protocol-string value))
                   ((integer? value) (int->hexstring value))
                   ((string? value) value)
                   (else (throw 'protocol-set-unknown-value-type value)))))
    (request-expects-ok conn (string-join (list "set" idx-str key-str val)
                                          " "))))

(define (line conn pidx lidx role)
  "Issue a ‘line’ request to a remote controller connected to via ‘conn’.

‘pidx’ is the index of the port that contains the line to be changed. ‘lidx’ is
the index of the line within port ‘pidx’. ‘role’ is the role the line will be
assigned in the next port initialisation."
  (let ((role-str (symbol->string role))
        (pidx-str (int->hexstring pidx))
        (lidx-str (int->hexstring lidx)))
    (request-expects-ok conn (string-join (list "line"
                                                pidx-str
                                                lidx-str
                                                role-str)
                                          " "))))
