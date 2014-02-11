;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(let ((root (format #f "~a/tests" (getcwd))))
  ;(format #t "# fs-root: [~a]~%" root)
  (tap/set-option 'fs-root root))
(tap/set-option 'fs-suffix "-scm.t")
(tap/set-option 'fs-prefix "")
