;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices bosch bno055)
  #:use-module (chip-remote device)
  #:use-module (chip-remote manufacturer bosch)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-map)
  #:export (bno055))

;; PAGE-0:

(define-register-map bno055-page-0
  #:table
  (#x0 (#:contents (bno055-chip-id 0 8)))
  (#x1 (#:contents (acc-chip-id 0 8)))
  (#x2 (#:contents (mag-chip-id 0 8)))
  (#x3 (#:contents (gyr-chip-id 0 8)))
  (#x4 (#:contents (software-revision-lsb 0 8)))
  (#x5 (#:contents (software-revision-msb 0 8)))
  (#x6 (#:contents (bootloader-revision 0 8)))
  ;; Interesting thing about register 7: This is the page-id register, that's
  ;; present in both memory pages. We'll figure out how to deal with this.
  (#x7 (#:contents (page-id 0 8)))
  (#x8 (#:contents (acc-data-x-lsb 0 8)))
  (#x9 (#:contents (acc-data-x-msb 0 8)))
  (#xa (#:contents (acc-data-y-lsb 0 8)))
  (#xb (#:contents (acc-data-y-msb 0 8)))
  (#xc (#:contents (acc-data-z-lsb 0 8)))
  (#xd (#:contents (acc-data-z-msb 0 8)))
  (#xe (#:contents (mag-data-x-lsb 0 8)))
  (#xf (#:contents (mag-data-x-msb 0 8)))
  (#x10 (#:contents (mag-data-y-lsb 0 8)))
  (#x11 (#:contents (mag-data-y-msb 0 8)))
  (#x12 (#:contents (mag-data-z-lsb 0 8)))
  (#x13 (#:contents (mag-data-z-msb 0 8)))
  (#x14 (#:contents (gyr-data-x-lsb 0 8)))
  (#x15 (#:contents (gyr-data-x-msb 0 8)))
  (#x16 (#:contents (gyr-data-y-lsb 0 8)))
  (#x17 (#:contents (gyr-data-y-msb 0 8)))
  (#x18 (#:contents (gyr-data-z-lsb 0 8)))
  (#x19 (#:contents (gyr-data-z-msb 0 8)))
  (#x1a (#:contents (euler-heading-lsb 0 8)))
  (#x1b (#:contents (euler-heading-msb 0 8)))
  (#x1c (#:contents (euler-roll-lsb 0 8)))
  (#x1d (#:contents (euler-roll-msb 0 8)))
  (#x1e (#:contents (euler-pitch-lsb 0 8)))
  (#x1f (#:contents (euler-pitch-msb 0 8)))
  (#x20 (#:contents (quaternion-w-lsb 0 8)))
  (#x21 (#:contents (quaternion-w-msb 0 8)))
  (#x22 (#:contents (quaternion-x-lsb 0 8)))
  (#x23 (#:contents (quaternion-x-msb 0 8)))
  (#x24 (#:contents (quaternion-y-lsb 0 8)))
  (#x25 (#:contents (quaternion-y-msb 0 8)))
  (#x26 (#:contents (quaternion-z-lsb 0 8)))
  (#x27 (#:contents (quaternion-z-msb 0 8)))
  (#x28 (#:contents (linear-accel-data-x-lsb 0 8)))
  (#x29 (#:contents (linear-accel-data-x-msb 0 8)))
  (#x2a (#:contents (linear-accel-data-y-lsb 0 8)))
  (#x2b (#:contents (linear-accel-data-y-msb 0 8)))
  (#x2c (#:contents (linear-accel-data-z-lsb 0 8)))
  (#x2d (#:contents (linear-accel-data-z-msb 0 8)))
  (#x2e (#:contents (gravity-data-x-lsb 0 8)))
  (#x2f (#:contents (gravity-data-x-msb 0 8)))
  (#x30 (#:contents (gravity-data-y-lsb 0 8)))
  (#x31 (#:contents (gravity-data-y-msb 0 8)))
  (#x32 (#:contents (gravity-data-z-lsb 0 8)))
  (#x33 (#:contents (gravity-data-z-msb 0 8)))
  (#x34 (#:contents (temperature 0 8)))
  (#x35 (#:contents (mag-calibration-status 0 2)
                    (acc-calibration-status 2 2)
                    (gyr-calibration-status 4 2)
                    (system-calibration-status 6 2)))
  (#x36 (#:contents (acc-self-test-result 0 1)
                    (mag-self-test-result 1 1)
                    (gyr-self-test-result 2 1)
                    (mcu-self-test-result 3 1)
                    (reserved 4 4)))
  (#x37 (#:contents (reserved 0 2)
                    (irq-gyr-any-motion 2 1)
                    (irq-gyr-high-rate 3 1)
                    (reserved 4 1)
                    (irq-acc-high-accel 5 1)
                    (irq-acc-any-motion 6 1)
                    (irq-acc-no-motion 7 1)))
  (#x38 (#:contents (sys-clock-cfg-unlock 0 1)
                    (reserved 1 7)))
  (#x39 (#:contents (system-status-code 0 8)))
  (#x3a (#:contents (system-error-code 0 8)))
  (#x3b (#:contents (acc-unit-select 0 1)
                    (gyr-unit-select 1 1)
                    (euler-unit-select 2 1)
                    (reserved 3 1)
                    (temperature-unit-select 4 1)
                    (reserved 5 2)
                    (orientation-mode-select 7 1)))
  ;; #x3c is completely reserved.
  (#x3d (#:contents (operation-mode 0 4)
                    (reserved 4 4)))
  (#x3e (#:contents (power-mode 0 2)
                    (reserved 2 6)))
  (#x3f (#:contents (sys-trigger-self-test 0 1)
                    (reserved 1 4)
                    (sys-trigger-reset 5 1)
                    (sys-trigger-irq-reset 6 1)
                    (sys-clock-select 7 1)))
  (#x40 (#:contents (sys-temperature-src-select 0 2)
                    (reserved 2 6)))
  (#x41 (#:contents (axis-map-x 0 2)
                    (axis-map-y 2 2)
                    (axis-map-z 4 2)
                    (reserved 6 2)))
  (#x42 (#:contents (axis-map-x-sign 0 1)
                    (axis-map-y-sign 1 1)
                    (axis-map-z-sign 2 1)
                    (reserved 3 5)))
  ;; #x43 .. #x54 are completely reserved.
  (#x55 (#:contents (acc-offset-x-lsb 0 8)))
  (#x56 (#:contents (acc-offset-x-msb 0 8)))
  (#x57 (#:contents (acc-offset-y-lsb 0 8)))
  (#x58 (#:contents (acc-offset-y-msb 0 8)))
  (#x59 (#:contents (acc-offset-z-lsb 0 8)))
  (#x5a (#:contents (acc-offset-z-msb 0 8)))
  (#x5b (#:contents (mag-offset-x-lsb 0 8)))
  (#x5c (#:contents (mag-offset-x-msb 0 8)))
  (#x5d (#:contents (mag-offset-y-lsb 0 8)))
  (#x5e (#:contents (mag-offset-y-msb 0 8)))
  (#x5f (#:contents (mag-offset-z-lsb 0 8)))
  (#x60 (#:contents (mag-offset-z-msb 0 8)))
  (#x61 (#:contents (gyr-offset-x-lsb 0 8)))
  (#x62 (#:contents (gyr-offset-x-msb 0 8)))
  (#x63 (#:contents (gyr-offset-y-lsb 0 8)))
  (#x64 (#:contents (gyr-offset-y-msb 0 8)))
  (#x65 (#:contents (gyr-offset-z-lsb 0 8)))
  (#x66 (#:contents (gyr-offset-z-msb 0 8)))
  (#x67 (#:contents (acc-radius-lsb 0 8)))
  (#x67 (#:contents (acc-radius-msb 0 8)))
  (#x69 (#:contents (mag-radius-lsb 0 8)))
  (#x6a (#:contents (mag-radius-msb 0 8))))

(define-register-map bno055-page-1
  #:table
  ;; #x0 .. #x6 is completely reserved.
  (#x7 (#:contents (page-id 0 8)))
  ;; TODO: The data sheet is contradicting itself here. Range could be 3 or two
  ;; bits; so can the bandwidth. Need to figure this out later.
  (#x8 (#:contents (acc-range 0 2)
                   (acc-bandwidth 2 3)
                   (acc-power-mode 5 3)))
  (#x9 (#:contents (mag-data-output-rate 0 3)
                   (mag-operation-mode 3 2)
                   (mag-power-mode 5 2)
                   (reserved 7 1)))
  (#xa (#:contents (gyr-range 0 3)
                   (gyr-bandwidth 3 3)
                   (reserved 6 2)))
  (#xb (#:contents (gyr-power-mode 0 3)
                   (reserved 3 5)))
  (#xc (#:contents (acc-sleep-mode 0 1)
                   (acc-sleep-duration 1 4)
                   (reserved 5 3)))
  (#xd (#:contents (gyr-sleep-duration 0 3)
                   (gyr-auto-sleep-duration 0 3)))
  ;; #xe is completely reserved.
  (#xf (#:contents (reserved 0 2)
                   (irq-mask-gyr-any-motion 2 1)
                   (irq-mask-gyr-high-rate 3 1)
                   (reserved 4 1)
                   (irq-mask-acc-high-accel 5 1)
                   (irq-mask-acc-any-motion 6 1)
                   (irq-mask-acc-no-motion 7 1)))
  (#x10 (#:contents (reserved 0 2)
                    (irq-enable-gyr-any-motion 2 1)
                    (irq-enable-gyr-high-rate 3 1)
                    (reserved 4 1)
                    (irq-enable-acc-high-accel 5 1)
                    (irq-enable-acc-any-motion 6 1)
                    (irq-enable-acc-no-motion 7 1)))
  (#x11 (#:contents (acc-any-motion-threshold 0 8)))
  (#x12 (#:contents (acc-any-motion-duration 0 2)
                    (acc-any/no-motion-x-axis 2 1)
                    (acc-any/no-motion-y-axis 3 1)
                    (acc-any/no-motion-z-axis 4 1)
                    (acc-high-accel-x-axis 5 1)
                    (acc-high-accel-y-axis 6 1)
                    (acc-high-accel-z-axis 7 1)))
  (#x13 (#:contents (acc-high-accel-duration 0 8)))
  (#x14 (#:contents (acc-high-accel-threshold 0 8)))
  (#x15 (#:contents (acc-slow/no-motion-threshold 0 8)))
  (#x16 (#:contents (acc-slow/no-motion-select 0 1)
                    (acc-slow/no-motion-duration 1 6)
                    (reserved 7 1)))
  (#x17 (#:contents (gyr-any-motion-x-axis 0 1)
                    (gyr-any-motion-y-axis 1 1)
                    (gyr-any-motion-z-axis 2 1)
                    (gyr-high-rate-z-axis 3 1)
                    (gyr-high-rate-z-axis 4 1)
                    (gyr-high-rate-z-axis 5 1)
                    (gyr-any-motion-select-filtered 6 1)
                    (gyr-high-rate-select-filtered 7 1)))
  (#x18 (#:contents (gyr-high-rate-x-threshold 0 5)
                    (gyr-high-rate-x-threshold-hysteresis 5 2)
                    (reserved 7 1)))
  (#x19 (#:contents (gyr-high-rate-x-duration 0 8)))
  (#x1a (#:contents (gyr-high-rate-y-threshold 0 5)
                    (gyr-high-rate-y-threshold-hysteresis 5 2)
                    (reserved 7 1)))
  (#x1b (#:contents (gyr-high-rate-y-duration 0 8)))
  (#x1c (#:contents (gyr-high-rate-z-threshold 0 5)
                    (gyr-high-rate-z-threshold-hysteresis 5 2)
                    (reserved 7 1)))
  (#x1d (#:contents (gyr-high-rate-z-duration 0 8)))
  (#x1e (#:contents (gyr-any-motion-threshold 0 7)
                    (reserved 7 1)))
  (#x1f (#:contents (gyr-any-motion-slope-samples 0 2)
                    (gyr-any-motion-awake-duration 2 2)
                    (reserved 4 4)))
  ;; #x20 .. #x4f are completely reserved.
  ;;
  ;; 16 bytes of unique ID:
  ;;   OOppNNmmLLkkJJiiHHggFFeeDDccBBaa
  (#x50 (#:contents (bno055-unique-id-a 0 8)))
  (#x51 (#:contents (bno055-unique-id-b 0 8)))
  (#x52 (#:contents (bno055-unique-id-c 0 8)))
  (#x53 (#:contents (bno055-unique-id-d 0 8)))
  (#x54 (#:contents (bno055-unique-id-e 0 8)))
  (#x55 (#:contents (bno055-unique-id-f 0 8)))
  (#x56 (#:contents (bno055-unique-id-g 0 8)))
  (#x57 (#:contents (bno055-unique-id-h 0 8)))
  (#x58 (#:contents (bno055-unique-id-i 0 8)))
  (#x59 (#:contents (bno055-unique-id-j 0 8)))
  (#x5a (#:contents (bno055-unique-id-k 0 8)))
  (#x5b (#:contents (bno055-unique-id-l 0 8)))
  (#x5c (#:contents (bno055-unique-id-m 0 8)))
  (#x5d (#:contents (bno055-unique-id-n 0 8)))
  (#x5e (#:contents (bno055-unique-id-o 0 8)))
  (#x5f (#:contents (bno055-unique-id-p 0 8))))

(define-page-map bno055-page-map
  (0 bno055-page-0)
  (1 bno055-page-1))

(define-device bno055
  #:manufacturer bosch
  #:homepage "https://www.bosch-sensortec.com/bst/products/all_products/bno055"
  #:datasheet "https://ae-bst.resource.bosch.com/media/_tech/media/datasheets/BST_BNO055_DS000_14.pdf"
  #:keywords '(nine axis absolute orientation sensor motion unit)
  #:register-width 8
  #:page-map* bno055-page-map)