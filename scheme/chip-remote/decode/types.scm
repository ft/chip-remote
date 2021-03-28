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
            make-register-window/decoder
            decoder-register-window?
            decoder-register-window-raw
            decoder-register-window-items
            decoder-register-window-description
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
            make-combination/decoder
            decoder-combination?
            decoder-combination-name
            decoder-combination-data
            decoder-combination-description
            make-combinations/decoder
            decoder-combinations?
            decoder-combinations-raw
            decoder-combinations
            decoder-combinations-description
            make-device/decoder
            decoder-device?
            decoder-device-raw
            decoder-device-page-map
            decoder-device-combinations
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

(define-record-type <decoder-register-window>
  (make-register-window/decoder raw items description)
  decoder-register-window?
  (raw decoder-register-window-raw)
  (items decoder-register-window-items)
  (description decoder-register-window-description))

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

(define-record-type <decoder-combination>
  (make-combination/decoder name data description)
  decoder-combination?
  (name decoder-combination-name)
  (data decoder-combination-data)
  (description decoder-combination-description))

(define-record-type <decoder-combinations>
  (make-combinations/decoder combinations description)
  decoder-combinations?
  (combinations decoder-combinations)
  (description decoder-combinations-description))

(define-record-type <decoder-device>
  (make-device/decoder raw page-map combinations description)
  decoder-device?
  (raw decoder-device-raw)
  (page-map decoder-device-page-map)
  (combinations decoder-device-combinations)
  (description decoder-device-description))
