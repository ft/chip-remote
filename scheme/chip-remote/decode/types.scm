(define-module (chip-remote decode types)
  #:use-module (srfi srfi-9)
  #:export (make-item/decoder
            decoder-item?
            decoder-item-decoded
            decoder-item-raw
            decoder-item-description
            make-register/decoder
            decoder-register?
            decoder-register-raw
            decoder-register-items
            decoder-register-description
            make-register-map/decoder
            decoder-register-map?
            decoder-register-map-raw
            decoder-register-map-registers
            decoder-register-map-description
            make-page-map/decoder
            decoder-page-map?
            decoder-page-map-raw
            decoder-page-map-register-maps
            decoder-page-map-description
            make-device/decoder
            decoder-device?
            decoder-device-raw
            decoder-device-page-map
            decoder-device-description))

;; The decoder front end produces decoded values of raw values of things in
;; chip-remote (like items, registers, etc.) and attaches additional informa-
;; tion to them. The reason here is to allow backends to have all the informa-
;; they need in order to produce the most informative human-readable data pos-
;; sible. This is what the decode* function returns.
;;
;; The decode function returns a structure, that's like the one returned by
;; decode*, but with all the additional information taken away, which may argu-
;; ably be more useful in a REPL.

(define-record-type <decoder-item>
  (make-item/decoder decoded raw description)
  decoder-item?
  (decoded decoder-item-decoded)
  (raw decoder-item-raw)
  (description decoder-item-description))

(define-record-type <decoder-register>
  (make-register/decoder raw items description)
  decoder-register?
  (raw decoder-register-raw)
  (items decoder-register-items)
  (description decoder-register-description))

(define-record-type <decoder-register-map>
  (make-register-map/decoder raw registers description)
  decoder-register-map?
  (raw decoder-register-map-raw)
  (registers decoder-register-map-registers)
  (description decoder-register-map-description))

(define-record-type <decoder-page-map>
  (make-page-map/decoder raw register-maps description)
  decoder-page-map?
  (raw decoder-page-map-raw)
  (register-maps decoder-page-map-register-maps)
  (description decoder-page-map-description))

(define-record-type <decoder-device>
  (make-device/decoder raw page-map description)
  decoder-device?
  (raw decoder-device-raw)
  (page-map decoder-device-page-map)
  (description decoder-device-description))
