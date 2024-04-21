(use-modules (ice-9 getopt-long)
             (ice-9 pretty-print)
             (rnrs io ports)
             (srfi srfi-1))

;; --- Manifest ---

;; The manifest is a list of records. Each record is a list of which the car is
;; a symbol that should be unique in the manifest; the cdr is an association
;; list, that has symbols as keys. Supported keys are:
;;
;;   type: This specifies the type of record. The value should be a symbol,
;;         that is either scheme or program.
;;
;;   scheme: When the type is scheme, this specifies the subdirectory that
;;           holds the scheme source files of this library. This value will be
;;           added to GUILE_LOAD_PATH and GUILE_LOAD_COMPILED_PATH.
;;
;;   path: This adds the named directory to PATH.
;;
;;   program: This defines a program in PATH that, if it is exists, skips the
;;            instantiation of the record in standalone/. This is used with
;;            records of program type.
;;
;;   module: With scheme record types, this tests for the availability of the
;;           named module in Guile's load-path. If the module already exists,
;;           instantiation is in standalone/ is skipped.
;;
;;   build: This is a list either lists of strings, or thunks that produce
;;          lists of strings. The lists of strings are passed to system* in
;;          order to build the project that is being instantiated in the
;;          standalone/ directory.
;;
;;   git: A string, that names the git repository a record's project can be
;;        cloned from. By default, the URL for cloning is derived from the
;;        remote of the master branch of the chip-remote clone, we are working
;;        from. This can be adjusted by supplying a mapping file via the
;;        --mapping (-M) option of the program.
;;
;; A mapping file is a file of which the last expression evaluates to an
;; association list, that maps a record name to a git repository URL. If, for
;; instance, you would like to clone the guile-tap git repository from
;; https://foobar.tld/p/guile-tap.git, you can make a file that contains:
;;
;; '((guile-tap . "https://foobar.tld/p/guile-tap.git"))
;;
;; …and pass that to the --mapping option of this program.
;;
;; The tools/standalone-github.scm file is an example that maps all projects to
;; the author's github account.

(define (concat . lst) (string-concatenate lst))

(define manifest
  `((guile-tap
     (type    . scheme)
     (scheme  . "scheme")
     (path    . "bin")
     (git     . "guile-tap.git")
     (module  . (test tap))
     (build   . (("make"))))
    (guile-termios
     (type    . scheme)
     (scheme  . "scheme")
     (git     . "guile-termios.git")
     (module  . (termios))
     (build   . (("make" "CC=gcc"))))
    (makemehappy
     (type    . program)
     (path    . ".")
     (git     . "makemehappy.git")
     (program . "mmh")
     (build   . (,(lambda ()
                    (let ((cwd (getcwd)))
                      (list "./configure"
                            (concat "--datadir=" cwd "/data")
                            (concat "--etcdir=" cwd "/etc")))))))
    (test-dispatch
     (type    . program)
     (path    . ".")
     (git     . "test-dispatch.git")
     (program . "run-tests"))))

;; --- Utilities ---

(define *name*    'make-standalone)
(define *version* '((major . 0)
                    (minor . 1)
                    (patch . 0)))

(define (pp-version v)
  (string-join (map (compose number->string cdr) v) "."))

(define option-spec
  '((help      (single-char #\h))
    (version   (single-char #\V))
    (force     (single-char #\f))
    (manifest  (single-char #\m))
    (directory (single-char #\d) (value #t))
    (mapping   (single-char #\M) (value #t))
    (shellcode (single-char #\o) (value #t))))

(define (opt o)
  (option-ref opts o #f))

(define opts (getopt-long (command-line) option-spec
                          #:stop-at-first-non-option #t))

(when (opt 'help)
  (format #t "usage: make-standalone [OPTION(s)...]~%")
  (quit 0))

(when (opt 'version)
  (format #t "~a version ~a~%" *name* (pp-version *version*))
  (quit 0))

(when (opt 'manifest)
  (pretty-print manifest)
  (quit 0))

(define *force* (opt 'force))
(define *standalone* (option-ref opts 'directory "standalone"))
(define *mapping* (let ((arg (opt 'mapping)))
                    (if arg
                        (primitive-load arg)
                        '())))
(define *shellcode* (option-ref opts 'shellcode "standalone/environ.sh"))
(define shellcode #f)

(define (standalone . rest)
  (define (string x) (if (symbol? x) (symbol->string x) x))
  (string-join (map string (cons *standalone* rest)) "/" 'infix))

(define (extend-environ variable value)
  (format #t "Extending ~a by ~s~%" variable value)
  (format shellcode
          "~a=\"${PWD}/~a${~a:+:}$~a\"~%"
          variable value variable variable))

(define (directory-exists? name)
  (and (file-exists? name)
       (eq? 'directory (stat:type (stat name)))))

(define (executable-file? name)
  (and (file-exists? name)
       (let ((st (stat name)))
         (and (eq? 'regular (stat:type st))
              (not (zero? (logand #o100 (stat:perms st))))))))

(define (program? str)
  (let ((path (string-split (getenv "PATH") #\:)))
    (let loop ((rest path))
      (if (null? rest)
          #f
          (let ((dir (car rest)))
            (if (executable-file? (concat dir "/" str))
                #t
                (loop (cdr rest))))))))

(define (available-module? lst)
  (string? (module-filename (resolve-module lst))))

(define (with-filesys-excursion! dir code)
  (let* ((old-cwd (getcwd))
         (return (lambda () (chdir old-cwd))))
    (chdir dir)
    (catch #t code
      (lambda (k . a)
        (return)
        (apply throw k a)))
    (return)))

(define (handle-status rc cmd)
  (when (or (not (zero? (status:exit-val rc)))
            (status:stop-sig rc)
            (status:term-sig rc))
    (throw 'command-failure rc cmd)))

(define (first-line text)
  (car (string-split text #\newline)))

(define (command->output . lst)
  (let* ((io (pipe))
         (pid (spawn (car lst ) lst
                     #:output (cdr io))))
    (close-port (cdr io))
    (let ((data (get-string-all (car io))))
      (close-port (car io))
      (let ((rc (cdr (waitpid pid))))
        (handle-status rc lst))
      (first-line data))))

(define (git-get-remote!)
  (command->output "git" "config" "--get" "branch.master.remote"))

(define (git-get-url! remote)
  (command->output "git" "remote" "get-url" remote))

(define (make-git-url-from-parent parent tail)
  (concat (dirname parent) "/" tail))

(define (default-url name param)
  (let* ((tail (assq-ref param 'git))
         (remote (git-get-remote!))
         (url (git-get-url! remote)))
    (make-git-url-from-parent url tail)))

(define (make-url name param)
  (or (assq-ref *mapping* name)
      (default-url name param)))

(define (git-clone! name param)
  (let* ((cmd (list "git" "clone"
                    (make-url name param)
                    (symbol->string name)))
         (rc (apply system* cmd)))
    (handle-status rc cmd)))

(define (ensure:git! name param)
  (let ((destination (standalone name)))
    (if (directory-exists? destination)
        (format #t "Destination exists (~a). Skipping git clone.~%" destination)
        (with-filesys-excursion! *standalone*
                                 (lambda () (git-clone! name param))))))

(define (ensure:scheme-chunk! name param)
  (let ((value (standalone name (assq-ref param 'scheme))))
    (extend-environ 'GUILE_LOAD_PATH value)
    (extend-environ 'GUILE_LOAD_COMPILED_PATH value))
  (let ((path? (assq-ref param 'path)))
    (when path?
      (extend-environ 'PATH (standalone name path?)))))

(define (ensure:program-chunk! name param)
  (extend-environ 'PATH (standalone name (assq-ref param 'path))))

(define (ensure:scheme! name param)
  (format #t "Ensuring availability of scheme module ~a...~%" name)
  (if (and (not *force*)
           (available-module? (assq-ref param ' module)))
      (format #t "Module found in execution environment. Skipping.~%")
      (begin
        (ensure:git! name param)
        (ensure:scheme-chunk! name param))))

(define (ensure:program! name param)
  (format #t "Ensuring availability of program ~a...~%" name)
  (if (and (not *force*)
           (program? (assq-ref param 'program)))
      (format #t "Program found in execution environment. Skipping.~%")
      (begin
        (ensure:git! name param)
        (ensure:program-chunk! name param))))

(define (ensure! spec)
  (let* ((name (car spec))
         (param (cdr spec))
         (build (assq-ref param 'build)))
    (case (assq-ref param 'type)
      ((scheme)  (ensure:scheme!  name param))
      ((program) (ensure:program! name param))
      (else (begin
              (format #t "Invalid manifest type: ~a~%" spec))))
    (when (and build (directory-exists? (standalone name)))
      (with-filesys-excursion!
       (standalone name)
       (lambda ()
         (for-each (lambda (step)
                     (let* ((cmd (if (procedure? step)
                                     (step)
                                     step))
                            (rc (apply system* cmd)))
                       (handle-status rc cmd)))
                   build))))))

;; --- Main Program ---

(unless (directory-exists? *standalone*)
  (mkdir *standalone*))

(when (file-exists? *shellcode*)
  (truncate-file *shellcode* 0))
(set! shellcode (open-output-file *shellcode*))
(format shellcode "#!/bin/sh~%~%")
(for-each ensure! manifest)
(close-port shellcode)
