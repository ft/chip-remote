((nil
  (meta:project-author  . "chip-remote workers")
  (meta:project-licence . "LICENCE")
  (meta:project-code "scheme"
                     "firmware/application"
                     "firmware/libraries"))
 (scheme-mode
  (eval . (put 'for-each-test            'scheme-indent-function 1))
  (eval . (put 'with-test-bundle         'scheme-indent-function 1))
  (eval . (put 'with-fs-test-bundle      'scheme-indent-function 0))
  (eval . (put 'with-fw-test-bundle      'scheme-indent-function 2))
  (eval . (put 'with-ellipsis            'scheme-indent-function 1))
  (eval . (put 'set-record-type-printer! 'scheme-indent-function 1))
  (eval . (put 'define-test              'scheme-indent-function 1))))
