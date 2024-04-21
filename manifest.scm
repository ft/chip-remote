;; -*- scheme -*-

;; This is a manifest file for GNU Guix. You can use it, for example with guix
;; shell:
;;
;;   % guix shell --pure -m manifest.scm
;;   % guix shell -m manifest.scm
;;
;; See the GNU Guix manual for details.

(use-modules (gnu packages base)
             (gnu packages bash)
             (gnu packages certs)
             (gnu packages cmake)
             (gnu packages gcc)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages less)
             (gnu packages ninja)
             (gnu packages perl)
             (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages version-control))

;; Packages needed for general operation
;;
;; In order to be able to use shell with --pure, these packages contain some of
;; the basic utilities you require, like ls or find.
(define *base-packages*
  (list coreutils
        findutils
        gnu-make))

;; These are not absolutely required, but useful for driving standalone/, for
;; instance. The gcc toolchain packages are required if you'd like to have
;; guile-termios in standalone.
(define *extension-packages*
  (list bash
        binutils
        gcc
        git
        glibc
        less
        nss-certs))

;; Packages needed to compile and test the Scheme library of the project
;;
;; These packages are required for compiling all Scheme modules in scheme/, as
;; well as running most of the test-suite in tests/.
(define *library-packages*
  (list guile-3.0
        guile-tap
        guile-termios
        perl
        sed))

;; The firmware is based on Zephyr RTOS. These are needed at a minimum.
(define *firmware-packages*
  (list cmake
        python-packaging
        python-pyelftools
        python-pykwalify
        ninja))

;; MakeMeHappy is a build orchestration tools for embedded systems. If your
;; host cannot provide it, we have support to get it into standalone. These
;; packages are required to run it. We're doing this instead of python venv.
(define *mmh-dependencies*
  (list python
        python-logbook
        python-mako
        python-pyyaml))

(concatenate-manifests
 (map packages->manifest
      (list *base-packages*
            *extension-packages*
            *library-packages*
            *firmware-packages*
            *mmh-dependencies*)))
