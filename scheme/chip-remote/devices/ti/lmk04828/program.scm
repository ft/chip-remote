;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti lmk04828 program)
  #:use-module (chip-remote assemble)
  #:use-module (chip-remote devices ti lmk04828 registers)
  #:use-module (chip-remote devices ti lmk04828 tables)
  #:use-module ((chip-remote devices ti lmk04828 conversions)
                #:renamer (symbol-prefix-proc 'conv:))
  #:export (disable-clear-pll1-ld-lost
            disable-clkout0-1-idl
            disable-clkout0-1-odl
            disable-clkout0-1-powerdown
            disable-clkout10-11-idl
            disable-clkout10-11-odl
            disable-clkout10-11-powerdown
            disable-clkout12-13-idl
            disable-clkout12-13-odl
            disable-clkout12-13-powerdown
            disable-clkout2-3-idl
            disable-clkout2-3-odl
            disable-clkout2-3-powerdown
            disable-clkout4-5-idl
            disable-clkout4-5-odl
            disable-clkout4-5-powerdown
            disable-clkout6-7-odl
            disable-clkout6-7-powerdown
            disable-clkout6-8-idl
            disable-clkout8-9-idl
            disable-clkout8-9-odl
            disable-clkout8-9-powerdown
            disable-clr-pll2-ld-lost
            disable-dclkout0-adly-mux
            disable-dclkout0-adly-powerdown
            disable-dclkout0-adlyg-powerdown
            disable-dclkout0-ddly-powerdown
            disable-dclkout0-hs
            disable-dclkout0-hsg-powerdown
            disable-dclkout10-adly-mux
            disable-dclkout10-adly-powerdown
            disable-dclkout10-ddly-powerdown
            disable-dclkout10-hs
            disable-dclkout10-hsg-powerdown
            disable-dclkout12-adly-mux
            disable-dclkout12-adly-powerdown
            disable-dclkout12-adlyg-powerdown
            disable-dclkout12-ddly-powerdown
            disable-dclkout12-hs
            disable-dclkout12-hsg-powerdown
            disable-dclkout2-adly-mux
            disable-dclkout2-adly-powerdown
            disable-dclkout2-adlyg-powerdown
            disable-dclkout2-ddly-powerdown
            disable-dclkout2-hs
            disable-dclkout2-hsg-powerdown
            disable-dclkout4-adly-mux
            disable-dclkout4-adly-powerdown
            disable-dclkout4-adlyg-powerdown
            disable-dclkout4-ddly-powerdown
            disable-dclkout4-hs
            disable-dclkout4-hsg-powerdown
            disable-dclkout6-adly-mux
            disable-dclkout6-adly-powerdown
            disable-dclkout6-adlyg-powerdown
            disable-dclkout6-ddly-powerdown
            disable-dclkout6-hs
            disable-dclkout6-hsg-powerdown
            disable-dclkout8-adly-mux
            disable-dclkout8-adly-powerdown
            disable-dclkout8-adlyg-powerdown
            disable-dclkout8-ddly-powerdown
            disable-dclkout8-hs
            disable-dclkout8-hsg-powerdown
            enable-sync-0
            enable-sync-10
            enable-sync-12
            enable-sync-2
            enable-sync-4
            enable-sync-6
            enable-sync-8
            enable-sync-sysref
            disable-dlclkout10-adlyg-powerdown
            disable-clkin-0-auto-mode
            disable-clkin-1-auto-mode
            disable-clkin-2-auto-mode
            disable-ddlyd-sysref
            disable-ddlyd0
            disable-ddlyd10
            disable-ddlyd12
            disable-ddlyd2
            disable-ddlyd4
            disable-ddlyd6
            disable-ddlyd7
            disable-fb-mux
            disable-holdover
            disable-los
            disable-manual-dac
            disable-pll2-freq-calibration
            disable-pll2-ref-2x
            disable-pll2-xtal
            disable-sdcklout11-adly
            disable-sdclkout1-adly
            disable-sdclkout13-adly
            disable-sdclkout3-adly
            disable-sdclkout5-adly
            disable-sdclkout7-adly
            disable-sdclkout9-adly
            disable-spi-three-wire
            disable-sync
            disable-sync-1shot
            disable-track
            disable-holdover-force
            disable-holdover-hitless-switch
            disable-holdover-los-detect
            disable-holdover-pll1-detect
            disable-holdover-vtune-detect
            disable-oscin-powerdown
            disable-pll1-cp-tri-state
            disable-pll1-powerdown
            disable-pll2-cp-tri-state
            disable-pll2-powerdown
            disable-pll2-prescaler-powerdown
            disable-powerdown
            disable-reset
            disable-sdclkout1-hs
            disable-sdclkout1-powerdown
            disable-sdclkout11-hs
            disable-sdclkout11-powerdown
            disable-sdclkout13-hs
            disable-sdclkout13-powerdown
            disable-sdclkout3-hs
            disable-sdclkout3-powerdown
            disable-sdclkout5-hs
            disable-sdclkout5-powerdown
            disable-sdclkout7-hs
            disable-sdclkout7-powerdown
            disable-sdclkout9-hs
            disable-sdclkout9-powerdown
            disable-sync-pll1-dld
            disable-sync-pll2-dld
            disable-sysref-ddly-clr
            disable-sysref-ddly-powerdown
            disable-sysref-global-powerdown
            disable-sysref-plsr-powerdown
            disable-sysref-powerdown
            disable-vco-ldo-powerdown
            disable-vco-powerdown
            enable-clear-pll1-ld-lost
            enable-clkout0-1-idl
            enable-clkout0-1-odl
            enable-clkout0-1-powerdown
            enable-clkout10-11-idl
            enable-clkout10-11-odl
            enable-clkout10-11-powerdown
            enable-clkout12-13-idl
            enable-clkout12-13-odl
            enable-clkout12-13-powerdown
            enable-clkout2-3-idl
            enable-clkout2-3-odl
            enable-clkout2-3-powerdown
            enable-clkout4-5-idl
            enable-clkout4-5-odl
            enable-clkout4-5-powerdown
            enable-clkout6-7-odl
            enable-clkout6-7-powerdown
            enable-clkout6-8-idl
            enable-clkout8-9-idl
            enable-clkout8-9-odl
            enable-clkout8-9-powerdown
            enable-clr-pll2-ld-lost
            enable-dclkout0-adly-mux
            enable-dclkout0-adly-powerdown
            enable-dclkout0-adlyg-powerdown
            enable-dclkout0-ddly-powerdown
            enable-dclkout0-hs
            enable-dclkout0-hsg-powerdown
            enable-dclkout10-adly-mux
            enable-dclkout10-adly-powerdown
            enable-dclkout10-ddly-powerdown
            enable-dclkout10-hs
            enable-dclkout10-hsg-powerdown
            enable-dclkout12-adly-mux
            enable-dclkout12-adly-powerdown
            enable-dclkout12-adlyg-powerdown
            enable-dclkout12-ddly-powerdown
            enable-dclkout12-hs
            enable-dclkout12-hsg-powerdown
            enable-dclkout2-adly-mux
            enable-dclkout2-adly-powerdown
            enable-dclkout2-adlyg-powerdown
            enable-dclkout2-ddly-powerdown
            enable-dclkout2-hs
            enable-dclkout2-hsg-powerdown
            enable-dclkout4-adly-mux
            enable-dclkout4-adly-powerdown
            enable-dclkout4-adlyg-powerdown
            enable-dclkout4-ddly-powerdown
            enable-dclkout4-hs
            enable-dclkout4-hsg-powerdown
            enable-dclkout6-adly-mux
            enable-dclkout6-adly-powerdown
            enable-dclkout6-adlyg-powerdown
            enable-dclkout6-ddly-powerdown
            enable-dclkout6-hs
            enable-dclkout6-hsg-powerdown
            enable-dclkout8-adly-mux
            enable-dclkout8-adly-powerdown
            enable-dclkout8-adlyg-powerdown
            enable-dclkout8-ddly-powerdown
            enable-dclkout8-hs
            enable-dclkout8-hsg-powerdown
            disable-sync-0
            disable-sync-10
            disable-sync-12
            disable-sync-2
            disable-sync-4
            disable-sync-6
            disable-sync-8
            disable-sync-sysref
            enable-dlclkout10-adlyg-powerdown
            enable-clkin-0-auto-mode
            enable-clkin-1-auto-mode
            enable-clkin-2-auto-mode
            enable-ddlyd-sysref
            enable-ddlyd0
            enable-ddlyd10
            enable-ddlyd12
            enable-ddlyd2
            enable-ddlyd4
            enable-ddlyd6
            enable-ddlyd7
            enable-fb-mux
            enable-holdover
            enable-los
            enable-manual-dac
            enable-pll2-freq-calibration
            enable-pll2-ref-2x
            enable-pll2-xtal
            enable-sdcklout11-adly
            enable-sdclkout1-adly
            enable-sdclkout13-adly
            enable-sdclkout3-adly
            enable-sdclkout5-adly
            enable-sdclkout7-adly
            enable-sdclkout9-adly
            enable-spi-three-wire
            enable-sync
            enable-sync-1shot
            enable-track
            enable-holdover-force
            enable-holdover-hitless-switch
            enable-holdover-los-detect
            enable-holdover-pll1-detect
            enable-holdover-vtune-detect
            enable-oscin-powerdown
            enable-pll1-cp-tri-state
            enable-pll1-powerdown
            enable-pll2-cp-tri-state
            enable-pll2-powerdown
            enable-pll2-prescaler-powerdown
            enable-powerdown
            enable-reset
            enable-sdclkout1-hs
            enable-sdclkout1-powerdown
            enable-sdclkout11-hs
            enable-sdclkout11-powerdown
            enable-sdclkout13-hs
            enable-sdclkout13-powerdown
            enable-sdclkout3-hs
            enable-sdclkout3-powerdown
            enable-sdclkout5-hs
            enable-sdclkout5-powerdown
            enable-sdclkout7-hs
            enable-sdclkout7-powerdown
            enable-sdclkout9-hs
            enable-sdclkout9-powerdown
            enable-sync-pll1-dld
            enable-sync-pll2-dld
            enable-sysref-ddly-clr
            enable-sysref-ddly-powerdown
            enable-sysref-global-powerdown
            enable-sysref-plsr-powerdown
            enable-sysref-powerdown
            enable-vco-ldo-powerdown
            enable-vco-powerdown
            set-clkin-0-type
            set-clkin-1-type
            set-clkin-2-type
            set-clkin-sel-mode
            set-clkin-sel-polarity
            set-clkin-sel0-mux
            set-clkin-sel0-type
            set-clkin-sel1-mux
            set-clkin-sel1-type
            set-clkin0-out-mux
            set-clkin0-r-divider-high
            set-clkin0-r-divider-low
            set-clkin1-out-mux
            set-clkin1-r-divider-high
            set-clkin1-r-divider-low
            set-clkin2-r-divider-high
            set-clkin2-r-divider-low
            set-clkout0-format
            set-clkout1-format
            set-clkout10-format
            set-clkout11-format
            set-clkout12-format
            set-clkout13-format
            set-clkout2-format
            set-clkout3-format
            set-clkout4-format
            set-clkout5-format
            set-clkout6-format
            set-clkout7-format
            set-clkout8-format
            set-clkout9-format
            set-dac-clk-cntr
            set-dac-clk-mult
            set-dac-trip-high
            set-dac-trip-low
            set-dclkout0-adly
            set-dclkout0-ddly-cnt-high
            set-dclkout0-ddly-cnt-low
            set-dclkout0-div
            set-dclkout0-mux
            set-dclkout0-polarity
            set-dclkout10-adly
            set-dclkout10-ddly-cnt-high
            set-dclkout10-ddly-cnt-low
            set-dclkout10-div
            set-dclkout10-mux
            set-dclkout10-polarity
            set-dclkout12-adly
            set-dclkout12-ddly-cnt-high
            set-dclkout12-ddly-cnt-low
            set-dclkout12-div
            set-dclkout12-mux
            set-dclkout12-polarity
            set-dclkout2-adly
            set-dclkout2-ddly-cnt-high
            set-dclkout2-ddly-cnt-low
            set-dclkout2-div
            set-dclkout2-mux
            set-dclkout2-polarity
            set-dclkout4-adly
            set-dclkout4-ddly-cnt-high
            set-dclkout4-ddly-cnt-low
            set-dclkout4-div
            set-dclkout4-mux
            set-dclkout4-polarity
            set-dclkout6-adly
            set-dclkout6-ddly-cnt-high
            set-dclkout6-ddly-cnt-low
            set-dclkout6-div
            set-dclkout6-mux
            set-dclkout6-polarity
            set-dclkout8-adly
            set-dclkout8-ddly-cnt-high
            set-dclkout8-ddly-cnt-low
            set-dclkout8-div
            set-dclkout8-mux
            set-dclkout8-polarity
            set-ddlyd-step-cnt
            set-enable-sysref-req
            set-fb-mux
            set-fixed-reg:0145
            set-holdover-dld-cnt-high
            set-holdover-dld-cnt-low
            set-id-device-type
            set-id-maskrev
            set-id-product-high
            set-id-product-low
            set-id-vendor-high
            set-id-vendor-low
            set-los-timeout
            set-manual-dac-high
            set-manual-dac-low
            set-opt-reg-1
            set-opt-reg-2
            set-oscin-freq
            set-oscout-format
            set-oscout-mux
            set-pll1-cp-gain
            set-pll1-cp-polarity
            set-pll1-dld-cnt-high
            set-pll1-dld-cnt-low
            set-pll1-ld-mux
            set-pll1-ld-type
            set-pll1-n-divider-dly
            set-pll1-n-divider-high
            set-pll1-n-divider-low
            set-pll1-nclk-mux
            set-pll1-r-divider-dly
            set-pll1-window-size
            set-pll2-cp-gain
            set-pll2-cp-polarity
            set-pll2-dld-cnt-high
            set-pll2-dld-cnt-low
            set-pll2-ld-mux
            set-pll2-ld-type
            set-pll2-loopfilter-c3
            set-pll2-loopfilter-c4
            set-pll2-loopfilter-r3
            set-pll2-loopfilter-r4
            set-pll2-n-calibration-phase-high
            set-pll2-n-calibration-phase-low
            set-pll2-n-calibration-phase-mid
            set-pll2-n-divider-high
            set-pll2-n-divider-low
            set-pll2-n-divider-mid
            set-pll2-nclk-mux
            set-pll2-prescaler
            set-pll2-r-divider-high
            set-pll2-r-divider-low
            set-pll2-window-size
            set-reset-mux
            set-reset-type
            set-sdclkout1-adly
            set-sdclkout1-ddly
            set-sdclkout1-disable-mode
            set-sdclkout1-mux
            set-sdclkout1-polarity
            set-sdclkout11-adly
            set-sdclkout11-ddly
            set-sdclkout11-disable-mode
            set-sdclkout11-mux
            set-sdclkout11-polarity
            set-sdclkout13-adly
            set-sdclkout13-ddly
            set-sdclkout13-disable-mode
            set-sdclkout13-mux
            set-sdclkout13-polarity
            set-sdclkout3-adly
            set-sdclkout3-ddly
            set-sdclkout3-disable-mode
            set-sdclkout3-mux
            set-sdclkout3-polarity
            set-sdclkout5-adly
            set-sdclkout5-ddly
            set-sdclkout5-disable-mode
            set-sdclkout5-mux
            set-sdclkout5-polarity
            set-sdclkout7-adly
            set-sdclkout7-ddly
            set-sdclkout7-disable-mode
            set-sdclkout7-mux
            set-sdclkout7-polarity
            set-sdclkout9-adly
            set-sdclkout9-ddly
            set-sdclkout9-disable-mode
            set-sdclkout9-mux
            set-sdclkout9-polarity
            set-sdio-readback-type
            enable-spi-lock-high
            disable-spi-lock-high
            enable-spi-lock-mid
            disable-spi-lock-mid
            enable-spi-lock-low
            disable-spi-lock-low
            set-sync-mode
            set-sync-polarity
            set-sysref-ddly-high
            set-sysref-ddly-low
            set-sysref-div-high
            set-sysref-div-low
            set-sysref-mux
            set-sysref-pulse-cnt
            set-vco-mux))

(define (disable-clear-pll1-ld-lost regval)
  (unset-logic-active-high set-clear-pll1-ld-lost-bits regval))

(define (disable-clkout0-1-idl regval)
  (unset-logic-active-high set-clkout0-1-idl-bits regval))

(define (disable-clkout0-1-odl regval)
  (unset-logic-active-high set-clkout0-1-odl-bits regval))

(define (disable-clkout0-1-powerdown regval)
  (unset-logic-active-high set-clkout0-1-powerdown-bits regval))

(define (disable-clkout10-11-idl regval)
  (unset-logic-active-high set-clkout10-11-idl-bits regval))

(define (disable-clkout10-11-odl regval)
  (unset-logic-active-high set-clkout10-11-odl-bits regval))

(define (disable-clkout10-11-powerdown regval)
  (unset-logic-active-high set-clkout10-11-powerdown-bits regval))

(define (disable-clkout12-13-idl regval)
  (unset-logic-active-high set-clkout12-13-idl-bits regval))

(define (disable-clkout12-13-odl regval)
  (unset-logic-active-high set-clkout12-13-odl-bits regval))

(define (disable-clkout12-13-powerdown regval)
  (unset-logic-active-high set-clkout12-13-powerdown-bits regval))

(define (disable-clkout2-3-idl regval)
  (unset-logic-active-high set-clkout2-3-idl-bits regval))

(define (disable-clkout2-3-odl regval)
  (unset-logic-active-high set-clkout2-3-odl-bits regval))

(define (disable-clkout2-3-powerdown regval)
  (unset-logic-active-high set-clkout2-3-powerdown-bits regval))

(define (disable-clkout4-5-idl regval)
  (unset-logic-active-high set-clkout4-5-idl-bits regval))

(define (disable-clkout4-5-odl regval)
  (unset-logic-active-high set-clkout4-5-odl-bits regval))

(define (disable-clkout4-5-powerdown regval)
  (unset-logic-active-high set-clkout4-5-powerdown-bits regval))

(define (disable-clkout6-7-odl regval)
  (unset-logic-active-high set-clkout6-7-odl-bits regval))

(define (disable-clkout6-7-powerdown regval)
  (unset-logic-active-high set-clkout6-7-powerdown-bits regval))

(define (disable-clkout6-8-idl regval)
  (unset-logic-active-high set-clkout6-8-idl-bits regval))

(define (disable-clkout8-9-idl regval)
  (unset-logic-active-high set-clkout8-9-idl-bits regval))

(define (disable-clkout8-9-odl regval)
  (unset-logic-active-high set-clkout8-9-odl-bits regval))

(define (disable-clkout8-9-powerdown regval)
  (unset-logic-active-high set-clkout8-9-powerdown-bits regval))

(define (disable-clr-pll2-ld-lost regval)
  (unset-logic-active-high set-clr-pll2-ld-lost-bits regval))

(define (disable-dclkout0-adly-mux regval)
  (unset-logic-active-high set-dclkout0-adly-mux-bits regval))

(define (disable-dclkout0-adly-powerdown regval)
  (unset-logic-active-high set-dclkout0-adly-powerdown-bits regval))

(define (disable-dclkout0-adlyg-powerdown regval)
  (unset-logic-active-high set-dclkout0-adlyg-powerdown-bits regval))

(define (disable-dclkout0-ddly-powerdown regval)
  (unset-logic-active-high set-dclkout0-ddly-powerdown-bits regval))

(define (disable-dclkout0-hs regval)
  (unset-logic-active-high set-dclkout0-hs-bits regval))

(define (disable-dclkout0-hsg-powerdown regval)
  (unset-logic-active-high set-dclkout0-hsg-powerdown-bits regval))

(define (disable-dclkout10-adly-mux regval)
  (unset-logic-active-high set-dclkout10-adly-mux-bits regval))

(define (disable-dclkout10-adly-powerdown regval)
  (unset-logic-active-high set-dclkout10-adly-powerdown-bits regval))

(define (disable-dclkout10-ddly-powerdown regval)
  (unset-logic-active-high set-dclkout10-ddly-powerdown-bits regval))

(define (disable-dclkout10-hs regval)
  (unset-logic-active-high set-dclkout10-hs-bits regval))

(define (disable-dclkout10-hsg-powerdown regval)
  (unset-logic-active-high set-dclkout10-hsg-powerdown-bits regval))

(define (disable-dclkout12-adly-mux regval)
  (unset-logic-active-high set-dclkout12-adly-mux-bits regval))

(define (disable-dclkout12-adly-powerdown regval)
  (unset-logic-active-high set-dclkout12-adly-powerdown-bits regval))

(define (disable-dclkout12-adlyg-powerdown regval)
  (unset-logic-active-high set-dclkout12-adlyg-powerdown-bits regval))

(define (disable-dclkout12-ddly-powerdown regval)
  (unset-logic-active-high set-dclkout12-ddly-powerdown-bits regval))

(define (disable-dclkout12-hs regval)
  (unset-logic-active-high set-dclkout12-hs-bits regval))

(define (disable-dclkout12-hsg-powerdown regval)
  (unset-logic-active-high set-dclkout12-hsg-powerdown-bits regval))

(define (disable-dclkout2-adly-mux regval)
  (unset-logic-active-high set-dclkout2-adly-mux-bits regval))

(define (disable-dclkout2-adly-powerdown regval)
  (unset-logic-active-high set-dclkout2-adly-powerdown-bits regval))

(define (disable-dclkout2-adlyg-powerdown regval)
  (unset-logic-active-high set-dclkout2-adlyg-powerdown-bits regval))

(define (disable-dclkout2-ddly-powerdown regval)
  (unset-logic-active-high set-dclkout2-ddly-powerdown-bits regval))

(define (disable-dclkout2-hs regval)
  (unset-logic-active-high set-dclkout2-hs-bits regval))

(define (disable-dclkout2-hsg-powerdown regval)
  (unset-logic-active-high set-dclkout2-hsg-powerdown-bits regval))

(define (disable-dclkout4-adly-mux regval)
  (unset-logic-active-high set-dclkout4-adly-mux-bits regval))

(define (disable-dclkout4-adly-powerdown regval)
  (unset-logic-active-high set-dclkout4-adly-powerdown-bits regval))

(define (disable-dclkout4-adlyg-powerdown regval)
  (unset-logic-active-high set-dclkout4-adlyg-powerdown-bits regval))

(define (disable-dclkout4-ddly-powerdown regval)
  (unset-logic-active-high set-dclkout4-ddly-powerdown-bits regval))

(define (disable-dclkout4-hs regval)
  (unset-logic-active-high set-dclkout4-hs-bits regval))

(define (disable-dclkout4-hsg-powerdown regval)
  (unset-logic-active-high set-dclkout4-hsg-powerdown-bits regval))

(define (disable-dclkout6-adly-mux regval)
  (unset-logic-active-high set-dclkout6-adly-mux-bits regval))

(define (disable-dclkout6-adly-powerdown regval)
  (unset-logic-active-high set-dclkout6-adly-powerdown-bits regval))

(define (disable-dclkout6-adlyg-powerdown regval)
  (unset-logic-active-high set-dclkout6-adlyg-powerdown-bits regval))

(define (disable-dclkout6-ddly-powerdown regval)
  (unset-logic-active-high set-dclkout6-ddly-powerdown-bits regval))

(define (disable-dclkout6-hs regval)
  (unset-logic-active-high set-dclkout6-hs-bits regval))

(define (disable-dclkout6-hsg-powerdown regval)
  (unset-logic-active-high set-dclkout6-hsg-powerdown-bits regval))

(define (disable-dclkout8-adly-mux regval)
  (unset-logic-active-high set-dclkout8-adly-mux-bits regval))

(define (disable-dclkout8-adly-powerdown regval)
  (unset-logic-active-high set-dclkout8-adly-powerdown-bits regval))

(define (disable-dclkout8-adlyg-powerdown regval)
  (unset-logic-active-high set-dclkout8-adlyg-powerdown-bits regval))

(define (disable-dclkout8-ddly-powerdown regval)
  (unset-logic-active-high set-dclkout8-ddly-powerdown-bits regval))

(define (disable-dclkout8-hs regval)
  (unset-logic-active-high set-dclkout8-hs-bits regval))

(define (disable-dclkout8-hsg-powerdown regval)
  (unset-logic-active-high set-dclkout8-hsg-powerdown-bits regval))

(define (enable-sync-0 regval)
  (unset-logic-active-high set-disable-sync-0-bits regval))

(define (enable-sync-10 regval)
  (unset-logic-active-high set-disable-sync-10-bits regval))

(define (enable-sync-12 regval)
  (unset-logic-active-high set-disable-sync-12-bits regval))

(define (enable-sync-2 regval)
  (unset-logic-active-high set-disable-sync-2-bits regval))

(define (enable-sync-4 regval)
  (unset-logic-active-high set-disable-sync-4-bits regval))

(define (enable-sync-6 regval)
  (unset-logic-active-high set-disable-sync-6-bits regval))

(define (enable-sync-8 regval)
  (unset-logic-active-high set-disable-sync-8-bits regval))

(define (enable-sync-sysref regval)
  (unset-logic-active-high set-disable-sync-sysref-bits regval))

(define (disable-dlclkout10-adlyg-powerdown regval)
  (unset-logic-active-high set-dlclkout10-adlyg-powerdown-bits regval))

(define (disable-clkin-0-auto-mode regval)
  (unset-logic-active-high set-enable-clkin-0-auto-mode-bits regval))

(define (disable-clkin-1-auto-mode regval)
  (unset-logic-active-high set-enable-clkin-1-auto-mode-bits regval))

(define (disable-clkin-2-auto-mode regval)
  (unset-logic-active-high set-enable-clkin-2-auto-mode-bits regval))

(define (disable-ddlyd-sysref regval)
  (unset-logic-active-high set-enable-ddlyd-sysref-bits regval))

(define (disable-ddlyd0 regval)
  (unset-logic-active-high set-enable-ddlyd0-bits regval))

(define (disable-ddlyd10 regval)
  (unset-logic-active-high set-enable-ddlyd10-bits regval))

(define (disable-ddlyd12 regval)
  (unset-logic-active-high set-enable-ddlyd12-bits regval))

(define (disable-ddlyd2 regval)
  (unset-logic-active-high set-enable-ddlyd2-bits regval))

(define (disable-ddlyd4 regval)
  (unset-logic-active-high set-enable-ddlyd4-bits regval))

(define (disable-ddlyd6 regval)
  (unset-logic-active-high set-enable-ddlyd6-bits regval))

(define (disable-ddlyd7 regval)
  (unset-logic-active-high set-enable-ddlyd7-bits regval))

(define (disable-fb-mux regval)
  (unset-logic-active-high set-enable-fb-mux-bits regval))

(define (disable-holdover regval)
  (unset-logic-active-high set-enable-holdover-bits regval))

(define (disable-los regval)
  (unset-logic-active-high set-enable-los-bits regval))

(define (disable-manual-dac regval)
  (unset-logic-active-high set-enable-manual-dac-bits regval))

(define (disable-pll2-freq-calibration regval)
  (unset-logic-active-low set-enable-pll2-freq-calibration-bits regval))

(define (disable-pll2-ref-2x regval)
  (unset-logic-active-high set-enable-pll2-ref-2x-bits regval))

(define (disable-pll2-xtal regval)
  (unset-logic-active-high set-enable-pll2-xtal-bits regval))

(define (disable-sdcklout11-adly regval)
  (unset-logic-active-high set-enable-sdcklout11-adly-bits regval))

(define (disable-sdclkout1-adly regval)
  (unset-logic-active-high set-enable-sdclkout1-adly-bits regval))

(define (disable-sdclkout13-adly regval)
  (unset-logic-active-high set-enable-sdclkout13-adly-bits regval))

(define (disable-sdclkout3-adly regval)
  (unset-logic-active-high set-enable-sdclkout3-adly-bits regval))

(define (disable-sdclkout5-adly regval)
  (unset-logic-active-high set-enable-sdclkout5-adly-bits regval))

(define (disable-sdclkout7-adly regval)
  (unset-logic-active-high set-enable-sdclkout7-adly-bits regval))

(define (disable-sdclkout9-adly regval)
  (unset-logic-active-high set-enable-sdclkout9-adly-bits regval))

(define (disable-spi-three-wire regval)
  (unset-logic-active-low set-enable-spi-three-wire-bits regval))

(define (disable-sync regval)
  (unset-logic-active-high set-enable-sync-bits regval))

(define (disable-sync-1shot regval)
  (unset-logic-active-high set-enable-sync-1shot-bits regval))

(define (disable-track regval)
  (unset-logic-active-high set-enable-track-bits regval))

(define (disable-holdover-force regval)
  (unset-logic-active-high set-holdover-force-bits regval))

(define (disable-holdover-hitless-switch regval)
  (unset-logic-active-high set-holdover-hitless-switch-bits regval))

(define (disable-holdover-los-detect regval)
  (unset-logic-active-high set-holdover-los-detect-bits regval))

(define (disable-holdover-pll1-detect regval)
  (unset-logic-active-high set-holdover-pll1-detect-bits regval))

(define (disable-holdover-vtune-detect regval)
  (unset-logic-active-high set-holdover-vtune-detect-bits regval))

(define (disable-oscin-powerdown regval)
  (unset-logic-active-high set-oscin-powerdown-bits regval))

(define (disable-pll1-cp-tri-state regval)
  (unset-logic-active-high set-pll1-cp-tri-state-bits regval))

(define (disable-pll1-powerdown regval)
  (unset-logic-active-high set-pll1-powerdown-bits regval))

(define (disable-pll2-cp-tri-state regval)
  (unset-logic-active-high set-pll2-cp-tri-state-bits regval))

(define (disable-pll2-powerdown regval)
  (unset-logic-active-high set-pll2-powerdown-bits regval))

(define (disable-pll2-prescaler-powerdown regval)
  (unset-logic-active-high set-pll2-prescaler-powerdown-bits regval))

(define (disable-powerdown regval)
  (unset-logic-active-high set-powerdown-bits regval))

(define (disable-reset regval)
  (unset-logic-active-high set-reset-bits regval))

(define (disable-sdclkout1-hs regval)
  (unset-logic-active-high set-sdclkout1-hs-bits regval))

(define (disable-sdclkout1-powerdown regval)
  (unset-logic-active-high set-sdclkout1-powerdown-bits regval))

(define (disable-sdclkout11-hs regval)
  (unset-logic-active-high set-sdclkout11-hs-bits regval))

(define (disable-sdclkout11-powerdown regval)
  (unset-logic-active-high set-sdclkout11-powerdown-bits regval))

(define (disable-sdclkout13-hs regval)
  (unset-logic-active-high set-sdclkout13-hs-bits regval))

(define (disable-sdclkout13-powerdown regval)
  (unset-logic-active-high set-sdclkout13-powerdown-bits regval))

(define (disable-sdclkout3-hs regval)
  (unset-logic-active-high set-sdclkout3-hs-bits regval))

(define (disable-sdclkout3-powerdown regval)
  (unset-logic-active-high set-sdclkout3-powerdown-bits regval))

(define (disable-sdclkout5-hs regval)
  (unset-logic-active-high set-sdclkout5-hs-bits regval))

(define (disable-sdclkout5-powerdown regval)
  (unset-logic-active-high set-sdclkout5-powerdown-bits regval))

(define (disable-sdclkout7-hs regval)
  (unset-logic-active-high set-sdclkout7-hs-bits regval))

(define (disable-sdclkout7-powerdown regval)
  (unset-logic-active-high set-sdclkout7-powerdown-bits regval))

(define (disable-sdclkout9-hs regval)
  (unset-logic-active-high set-sdclkout9-hs-bits regval))

(define (disable-sdclkout9-powerdown regval)
  (unset-logic-active-high set-sdclkout9-powerdown-bits regval))

(define (disable-sync-pll1-dld regval)
  (unset-logic-active-high set-sync-pll1-dld-bits regval))

(define (disable-sync-pll2-dld regval)
  (unset-logic-active-high set-sync-pll2-dld-bits regval))

(define (disable-sysref-ddly-clr regval)
  (unset-logic-active-high set-sysref-ddly-clr-bits regval))

(define (disable-sysref-ddly-powerdown regval)
  (unset-logic-active-high set-sysref-ddly-powerdown-bits regval))

(define (disable-sysref-global-powerdown regval)
  (unset-logic-active-high set-sysref-global-powerdown-bits regval))

(define (disable-sysref-plsr-powerdown regval)
  (unset-logic-active-high set-sysref-plsr-powerdown-bits regval))

(define (disable-sysref-powerdown regval)
  (unset-logic-active-high set-sysref-powerdown-bits regval))

(define (disable-vco-ldo-powerdown regval)
  (unset-logic-active-high set-vco-ldo-powerdown-bits regval))

(define (disable-vco-powerdown regval)
  (unset-logic-active-high set-vco-powerdown-bits regval))

(define (enable-clear-pll1-ld-lost regval)
  (set-logic-active-high set-clear-pll1-ld-lost-bits regval))

(define (enable-clkout0-1-idl regval)
  (set-logic-active-high set-clkout0-1-idl-bits regval))

(define (enable-clkout0-1-odl regval)
  (set-logic-active-high set-clkout0-1-odl-bits regval))

(define (enable-clkout0-1-powerdown regval)
  (set-logic-active-high set-clkout0-1-powerdown-bits regval))

(define (enable-clkout10-11-idl regval)
  (set-logic-active-high set-clkout10-11-idl-bits regval))

(define (enable-clkout10-11-odl regval)
  (set-logic-active-high set-clkout10-11-odl-bits regval))

(define (enable-clkout10-11-powerdown regval)
  (set-logic-active-high set-clkout10-11-powerdown-bits regval))

(define (enable-clkout12-13-idl regval)
  (set-logic-active-high set-clkout12-13-idl-bits regval))

(define (enable-clkout12-13-odl regval)
  (set-logic-active-high set-clkout12-13-odl-bits regval))

(define (enable-clkout12-13-powerdown regval)
  (set-logic-active-high set-clkout12-13-powerdown-bits regval))

(define (enable-clkout2-3-idl regval)
  (set-logic-active-high set-clkout2-3-idl-bits regval))

(define (enable-clkout2-3-odl regval)
  (set-logic-active-high set-clkout2-3-odl-bits regval))

(define (enable-clkout2-3-powerdown regval)
  (set-logic-active-high set-clkout2-3-powerdown-bits regval))

(define (enable-clkout4-5-idl regval)
  (set-logic-active-high set-clkout4-5-idl-bits regval))

(define (enable-clkout4-5-odl regval)
  (set-logic-active-high set-clkout4-5-odl-bits regval))

(define (enable-clkout4-5-powerdown regval)
  (set-logic-active-high set-clkout4-5-powerdown-bits regval))

(define (enable-clkout6-7-odl regval)
  (set-logic-active-high set-clkout6-7-odl-bits regval))

(define (enable-clkout6-7-powerdown regval)
  (set-logic-active-high set-clkout6-7-powerdown-bits regval))

(define (enable-clkout6-8-idl regval)
  (set-logic-active-high set-clkout6-8-idl-bits regval))

(define (enable-clkout8-9-idl regval)
  (set-logic-active-high set-clkout8-9-idl-bits regval))

(define (enable-clkout8-9-odl regval)
  (set-logic-active-high set-clkout8-9-odl-bits regval))

(define (enable-clkout8-9-powerdown regval)
  (set-logic-active-high set-clkout8-9-powerdown-bits regval))

(define (enable-clr-pll2-ld-lost regval)
  (set-logic-active-high set-clr-pll2-ld-lost-bits regval))

(define (enable-dclkout0-adly-mux regval)
  (set-logic-active-high set-dclkout0-adly-mux-bits regval))

(define (enable-dclkout0-adly-powerdown regval)
  (set-logic-active-high set-dclkout0-adly-powerdown-bits regval))

(define (enable-dclkout0-adlyg-powerdown regval)
  (set-logic-active-high set-dclkout0-adlyg-powerdown-bits regval))

(define (enable-dclkout0-ddly-powerdown regval)
  (set-logic-active-high set-dclkout0-ddly-powerdown-bits regval))

(define (enable-dclkout0-hs regval)
  (set-logic-active-high set-dclkout0-hs-bits regval))

(define (enable-dclkout0-hsg-powerdown regval)
  (set-logic-active-high set-dclkout0-hsg-powerdown-bits regval))

(define (enable-dclkout10-adly-mux regval)
  (set-logic-active-high set-dclkout10-adly-mux-bits regval))

(define (enable-dclkout10-adly-powerdown regval)
  (set-logic-active-high set-dclkout10-adly-powerdown-bits regval))

(define (enable-dclkout10-ddly-powerdown regval)
  (set-logic-active-high set-dclkout10-ddly-powerdown-bits regval))

(define (enable-dclkout10-hs regval)
  (set-logic-active-high set-dclkout10-hs-bits regval))

(define (enable-dclkout10-hsg-powerdown regval)
  (set-logic-active-high set-dclkout10-hsg-powerdown-bits regval))

(define (enable-dclkout12-adly-mux regval)
  (set-logic-active-high set-dclkout12-adly-mux-bits regval))

(define (enable-dclkout12-adly-powerdown regval)
  (set-logic-active-high set-dclkout12-adly-powerdown-bits regval))

(define (enable-dclkout12-adlyg-powerdown regval)
  (set-logic-active-high set-dclkout12-adlyg-powerdown-bits regval))

(define (enable-dclkout12-ddly-powerdown regval)
  (set-logic-active-high set-dclkout12-ddly-powerdown-bits regval))

(define (enable-dclkout12-hs regval)
  (set-logic-active-high set-dclkout12-hs-bits regval))

(define (enable-dclkout12-hsg-powerdown regval)
  (set-logic-active-high set-dclkout12-hsg-powerdown-bits regval))

(define (enable-dclkout2-adly-mux regval)
  (set-logic-active-high set-dclkout2-adly-mux-bits regval))

(define (enable-dclkout2-adly-powerdown regval)
  (set-logic-active-high set-dclkout2-adly-powerdown-bits regval))

(define (enable-dclkout2-adlyg-powerdown regval)
  (set-logic-active-high set-dclkout2-adlyg-powerdown-bits regval))

(define (enable-dclkout2-ddly-powerdown regval)
  (set-logic-active-high set-dclkout2-ddly-powerdown-bits regval))

(define (enable-dclkout2-hs regval)
  (set-logic-active-high set-dclkout2-hs-bits regval))

(define (enable-dclkout2-hsg-powerdown regval)
  (set-logic-active-high set-dclkout2-hsg-powerdown-bits regval))

(define (enable-dclkout4-adly-mux regval)
  (set-logic-active-high set-dclkout4-adly-mux-bits regval))

(define (enable-dclkout4-adly-powerdown regval)
  (set-logic-active-high set-dclkout4-adly-powerdown-bits regval))

(define (enable-dclkout4-adlyg-powerdown regval)
  (set-logic-active-high set-dclkout4-adlyg-powerdown-bits regval))

(define (enable-dclkout4-ddly-powerdown regval)
  (set-logic-active-high set-dclkout4-ddly-powerdown-bits regval))

(define (enable-dclkout4-hs regval)
  (set-logic-active-high set-dclkout4-hs-bits regval))

(define (enable-dclkout4-hsg-powerdown regval)
  (set-logic-active-high set-dclkout4-hsg-powerdown-bits regval))

(define (enable-dclkout6-adly-mux regval)
  (set-logic-active-high set-dclkout6-adly-mux-bits regval))

(define (enable-dclkout6-adly-powerdown regval)
  (set-logic-active-high set-dclkout6-adly-powerdown-bits regval))

(define (enable-dclkout6-adlyg-powerdown regval)
  (set-logic-active-high set-dclkout6-adlyg-powerdown-bits regval))

(define (enable-dclkout6-ddly-powerdown regval)
  (set-logic-active-high set-dclkout6-ddly-powerdown-bits regval))

(define (enable-dclkout6-hs regval)
  (set-logic-active-high set-dclkout6-hs-bits regval))

(define (enable-dclkout6-hsg-powerdown regval)
  (set-logic-active-high set-dclkout6-hsg-powerdown-bits regval))

(define (enable-dclkout8-adly-mux regval)
  (set-logic-active-high set-dclkout8-adly-mux-bits regval))

(define (enable-dclkout8-adly-powerdown regval)
  (set-logic-active-high set-dclkout8-adly-powerdown-bits regval))

(define (enable-dclkout8-adlyg-powerdown regval)
  (set-logic-active-high set-dclkout8-adlyg-powerdown-bits regval))

(define (enable-dclkout8-ddly-powerdown regval)
  (set-logic-active-high set-dclkout8-ddly-powerdown-bits regval))

(define (enable-dclkout8-hs regval)
  (set-logic-active-high set-dclkout8-hs-bits regval))

(define (enable-dclkout8-hsg-powerdown regval)
  (set-logic-active-high set-dclkout8-hsg-powerdown-bits regval))

(define (disable-sync-0 regval)
  (set-logic-active-high set-disable-sync-0-bits regval))

(define (disable-sync-10 regval)
  (set-logic-active-high set-disable-sync-10-bits regval))

(define (disable-sync-12 regval)
  (set-logic-active-high set-disable-sync-12-bits regval))

(define (disable-sync-2 regval)
  (set-logic-active-high set-disable-sync-2-bits regval))

(define (disable-sync-4 regval)
  (set-logic-active-high set-disable-sync-4-bits regval))

(define (disable-sync-6 regval)
  (set-logic-active-high set-disable-sync-6-bits regval))

(define (disable-sync-8 regval)
  (set-logic-active-high set-disable-sync-8-bits regval))

(define (disable-sync-sysref regval)
  (set-logic-active-high set-disable-sync-sysref-bits regval))

(define (enable-dlclkout10-adlyg-powerdown regval)
  (set-logic-active-high set-dlclkout10-adlyg-powerdown-bits regval))

(define (enable-clkin-0-auto-mode regval)
  (set-logic-active-high set-enable-clkin-0-auto-mode-bits regval))

(define (enable-clkin-1-auto-mode regval)
  (set-logic-active-high set-enable-clkin-1-auto-mode-bits regval))

(define (enable-clkin-2-auto-mode regval)
  (set-logic-active-high set-enable-clkin-2-auto-mode-bits regval))

(define (enable-ddlyd-sysref regval)
  (set-logic-active-high set-enable-ddlyd-sysref-bits regval))

(define (enable-ddlyd0 regval)
  (set-logic-active-high set-enable-ddlyd0-bits regval))

(define (enable-ddlyd10 regval)
  (set-logic-active-high set-enable-ddlyd10-bits regval))

(define (enable-ddlyd12 regval)
  (set-logic-active-high set-enable-ddlyd12-bits regval))

(define (enable-ddlyd2 regval)
  (set-logic-active-high set-enable-ddlyd2-bits regval))

(define (enable-ddlyd4 regval)
  (set-logic-active-high set-enable-ddlyd4-bits regval))

(define (enable-ddlyd6 regval)
  (set-logic-active-high set-enable-ddlyd6-bits regval))

(define (enable-ddlyd7 regval)
  (set-logic-active-high set-enable-ddlyd7-bits regval))

(define (enable-fb-mux regval)
  (set-logic-active-high set-enable-fb-mux-bits regval))

(define (enable-holdover regval)
  (set-logic-active-high set-enable-holdover-bits regval))

(define (enable-los regval)
  (set-logic-active-high set-enable-los-bits regval))

(define (enable-manual-dac regval)
  (set-logic-active-high set-enable-manual-dac-bits regval))

(define (enable-pll2-freq-calibration regval)
  (set-logic-active-low set-enable-pll2-freq-calibration-bits regval))

(define (enable-pll2-ref-2x regval)
  (set-logic-active-high set-enable-pll2-ref-2x-bits regval))

(define (enable-pll2-xtal regval)
  (set-logic-active-high set-enable-pll2-xtal-bits regval))

(define (enable-sdcklout11-adly regval)
  (set-logic-active-high set-enable-sdcklout11-adly-bits regval))

(define (enable-sdclkout1-adly regval)
  (set-logic-active-high set-enable-sdclkout1-adly-bits regval))

(define (enable-sdclkout13-adly regval)
  (set-logic-active-high set-enable-sdclkout13-adly-bits regval))

(define (enable-sdclkout3-adly regval)
  (set-logic-active-high set-enable-sdclkout3-adly-bits regval))

(define (enable-sdclkout5-adly regval)
  (set-logic-active-high set-enable-sdclkout5-adly-bits regval))

(define (enable-sdclkout7-adly regval)
  (set-logic-active-high set-enable-sdclkout7-adly-bits regval))

(define (enable-sdclkout9-adly regval)
  (set-logic-active-high set-enable-sdclkout9-adly-bits regval))

(define (enable-spi-three-wire regval)
  (set-logic-active-low set-enable-spi-three-wire-bits regval))

(define (enable-sync regval)
  (set-logic-active-high set-enable-sync-bits regval))

(define (enable-sync-1shot regval)
  (set-logic-active-high set-enable-sync-1shot-bits regval))

(define (enable-track regval)
  (set-logic-active-high set-enable-track-bits regval))

(define (enable-holdover-force regval)
  (set-logic-active-high set-holdover-force-bits regval))

(define (enable-holdover-hitless-switch regval)
  (set-logic-active-high set-holdover-hitless-switch-bits regval))

(define (enable-holdover-los-detect regval)
  (set-logic-active-high set-holdover-los-detect-bits regval))

(define (enable-holdover-pll1-detect regval)
  (set-logic-active-high set-holdover-pll1-detect-bits regval))

(define (enable-holdover-vtune-detect regval)
  (set-logic-active-high set-holdover-vtune-detect-bits regval))

(define (enable-oscin-powerdown regval)
  (set-logic-active-high set-oscin-powerdown-bits regval))

(define (enable-pll1-cp-tri-state regval)
  (set-logic-active-high set-pll1-cp-tri-state-bits regval))

(define (enable-pll1-powerdown regval)
  (set-logic-active-high set-pll1-powerdown-bits regval))

(define (enable-pll2-cp-tri-state regval)
  (set-logic-active-high set-pll2-cp-tri-state-bits regval))

(define (enable-pll2-powerdown regval)
  (set-logic-active-high set-pll2-powerdown-bits regval))

(define (enable-pll2-prescaler-powerdown regval)
  (set-logic-active-high set-pll2-prescaler-powerdown-bits regval))

(define (enable-powerdown regval)
  (set-logic-active-high set-powerdown-bits regval))

(define (enable-reset regval)
  (set-logic-active-high set-reset-bits regval))

(define (enable-sdclkout1-hs regval)
  (set-logic-active-high set-sdclkout1-hs-bits regval))

(define (enable-sdclkout1-powerdown regval)
  (set-logic-active-high set-sdclkout1-powerdown-bits regval))

(define (enable-sdclkout11-hs regval)
  (set-logic-active-high set-sdclkout11-hs-bits regval))

(define (enable-sdclkout11-powerdown regval)
  (set-logic-active-high set-sdclkout11-powerdown-bits regval))

(define (enable-sdclkout13-hs regval)
  (set-logic-active-high set-sdclkout13-hs-bits regval))

(define (enable-sdclkout13-powerdown regval)
  (set-logic-active-high set-sdclkout13-powerdown-bits regval))

(define (enable-sdclkout3-hs regval)
  (set-logic-active-high set-sdclkout3-hs-bits regval))

(define (enable-sdclkout3-powerdown regval)
  (set-logic-active-high set-sdclkout3-powerdown-bits regval))

(define (enable-sdclkout5-hs regval)
  (set-logic-active-high set-sdclkout5-hs-bits regval))

(define (enable-sdclkout5-powerdown regval)
  (set-logic-active-high set-sdclkout5-powerdown-bits regval))

(define (enable-sdclkout7-hs regval)
  (set-logic-active-high set-sdclkout7-hs-bits regval))

(define (enable-sdclkout7-powerdown regval)
  (set-logic-active-high set-sdclkout7-powerdown-bits regval))

(define (enable-sdclkout9-hs regval)
  (set-logic-active-high set-sdclkout9-hs-bits regval))

(define (enable-sdclkout9-powerdown regval)
  (set-logic-active-high set-sdclkout9-powerdown-bits regval))

(define (enable-sync-pll1-dld regval)
  (set-logic-active-high set-sync-pll1-dld-bits regval))

(define (enable-sync-pll2-dld regval)
  (set-logic-active-high set-sync-pll2-dld-bits regval))

(define (enable-sysref-ddly-clr regval)
  (set-logic-active-high set-sysref-ddly-clr-bits regval))

(define (enable-sysref-ddly-powerdown regval)
  (set-logic-active-high set-sysref-ddly-powerdown-bits regval))

(define (enable-sysref-global-powerdown regval)
  (set-logic-active-high set-sysref-global-powerdown-bits regval))

(define (enable-sysref-plsr-powerdown regval)
  (set-logic-active-high set-sysref-plsr-powerdown-bits regval))

(define (enable-sysref-powerdown regval)
  (set-logic-active-high set-sysref-powerdown-bits regval))

(define (enable-vco-ldo-powerdown regval)
  (set-logic-active-high set-vco-ldo-powerdown-bits regval))

(define (enable-vco-powerdown regval)
  (set-logic-active-high set-vco-powerdown-bits regval))

(define (set-clkin-0-type regval value)
  (set-clkin-0-type-bits regval (value->bits clkin-type-map value)))

(define (set-clkin-1-type regval value)
  (set-clkin-1-type-bits regval (value->bits clkin-type-map value)))

(define (set-clkin-2-type regval value)
  (set-clkin-2-type-bits regval (value->bits clkin-type-map value)))

(define (set-clkin-sel-mode regval value)
  (set-clkin-sel-mode-bits regval (value->bits clkin-sel-mode-map value)))

(define (set-clkin-sel-polarity regval value)
  (set-clkin-sel-polarity-bits regval (value->bits clkin-sel-polarity-map value)))

(define (set-clkin-sel0-mux regval value)
  (set-clkin-sel0-mux-bits regval (value->bits clkin-sel0-mux-map value)))

(define (set-clkin-sel0-type regval value)
  (set-clkin-sel0-type-bits regval (value->bits pin-type-map value)))

(define (set-clkin-sel1-mux regval value)
  (set-clkin-sel1-mux-bits regval (value->bits clkin-sel1-mux-map value)))

(define (set-clkin-sel1-type regval value)
  (set-clkin-sel1-type-bits regval (value->bits pin-type-map value)))

(define (set-clkin0-out-mux regval value)
  (set-clkin0-out-mux-bits regval (value->bits clkin0-out-mux-map value)))

(define (set-clkin0-r-divider-high regval value)
  (with-constraints (value (>= 0) (<= #b111111))
    (set-clkin0-r-divider-high-bits regval value)))

(define (set-clkin0-r-divider-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-clkin0-r-divider-low-bits regval value)))

(define (set-clkin1-out-mux regval value)
  (set-clkin1-out-mux-bits regval (value->bits clkin1-out-mux-map value)))

(define (set-clkin1-r-divider-high regval value)
  (with-constraints (value (>= 0) (<= #b111111))
    (set-clkin1-r-divider-high-bits regval value)))

(define (set-clkin1-r-divider-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-clkin1-r-divider-low-bits regval value)))

(define (set-clkin2-r-divider-high regval value)
  (with-constraints (value (>= 0) (<= #b111111))
    (set-clkin2-r-divider-high-bits regval value)))

(define (set-clkin2-r-divider-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-clkin2-r-divider-low-bits regval value)))

(define (set-clkout0-format regval value)
  (set-clkout0-format-bits regval (value->bits output-format-map value)))

(define (set-clkout1-format regval value)
  (set-clkout1-format-bits regval (value->bits output-format-map value)))

(define (set-clkout10-format regval value)
  (set-clkout10-format-bits regval (value->bits output-format-map value)))

(define (set-clkout11-format regval value)
  (set-clkout11-format-bits regval (value->bits output-format-map value)))

(define (set-clkout12-format regval value)
  (set-clkout12-format-bits regval (value->bits output-format-map value)))

(define (set-clkout13-format regval value)
  (set-clkout13-format-bits regval (value->bits output-format-map value)))

(define (set-clkout2-format regval value)
  (set-clkout2-format-bits regval (value->bits output-format-map value)))

(define (set-clkout3-format regval value)
  (set-clkout3-format-bits regval (value->bits output-format-map value)))

(define (set-clkout4-format regval value)
  (set-clkout4-format-bits regval (value->bits output-format-map value)))

(define (set-clkout5-format regval value)
  (set-clkout5-format-bits regval (value->bits output-format-map value)))

(define (set-clkout6-format regval value)
  (set-clkout6-format-bits regval (value->bits output-format-map value)))

(define (set-clkout7-format regval value)
  (set-clkout7-format-bits regval (value->bits output-format-map value)))

(define (set-clkout8-format regval value)
  (set-clkout8-format-bits regval (value->bits output-format-map value)))

(define (set-clkout9-format regval value)
  (set-clkout9-format-bits regval (value->bits output-format-map value)))

(define (set-dac-clk-cntr regval value)
  (with-constraints (value (>= 0) (<= 255))
    (set-dac-clk-cntr-bits regval value)))

(define (set-dac-clk-mult regval value)
  (set-dac-clk-mult-bits regval (value->bits dac-clk-mult-map value)))

(define (set-dac-trip-high regval value)
  (format #t "WARNING: Unhandled second level access function!~%")
  (format #t "         `set-dac-trip-high' generated with:~%")
  (format #t "             type: function~%")
  (format #t "             var:  decode-dac-trip~%")
  (format #t "You probably want to write a special-purpose function instead!~%")
  (format #t "This function falls back to the first-level access function!~%")
  (set-dac-trip-high-bits regval value))

(define (set-dac-trip-low regval value)
  (format #t "WARNING: Unhandled second level access function!~%")
  (format #t "         `set-dac-trip-low' generated with:~%")
  (format #t "             type: function~%")
  (format #t "             var:  decode-dac-trip~%")
  (format #t "You probably want to write a special-purpose function instead!~%")
  (format #t "This function falls back to the first-level access function!~%")
  (set-dac-trip-low-bits regval value))

(define (set-dclkout0-adly regval value)
  (set-dclkout0-adly-bits regval (conv:analog-delay->bits value)))

(define (set-dclkout0-ddly-cnt-high regval value)
  (set-dclkout0-ddly-cnt-high-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout0-ddly-cnt-low regval value)
  (set-dclkout0-ddly-cnt-low-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout0-div regval value)
  (set-dclkout0-div-bits regval (conv:clkout-divider->bits value)))

(define (set-dclkout0-mux regval value)
  (set-dclkout0-mux-bits regval (value->bits clkout-mux-map value)))

(define (set-dclkout0-polarity regval value)
  (set-dclkout0-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-dclkout10-adly regval value)
  (set-dclkout10-adly-bits regval (conv:analog-delay->bits value)))

(define (set-dclkout10-ddly-cnt-high regval value)
  (set-dclkout10-ddly-cnt-high-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout10-ddly-cnt-low regval value)
  (set-dclkout10-ddly-cnt-low-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout10-div regval value)
  (set-dclkout10-div-bits regval (conv:clkout-divider->bits value)))

(define (set-dclkout10-mux regval value)
  (set-dclkout10-mux-bits regval (value->bits clkout-mux-map value)))

(define (set-dclkout10-polarity regval value)
  (set-dclkout10-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-dclkout12-adly regval value)
  (set-dclkout12-adly-bits regval (conv:analog-delay->bits value)))

(define (set-dclkout12-ddly-cnt-high regval value)
  (set-dclkout12-ddly-cnt-high-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout12-ddly-cnt-low regval value)
  (set-dclkout12-ddly-cnt-low-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout12-div regval value)
  (set-dclkout12-div-bits regval (conv:clkout-divider->bits value)))

(define (set-dclkout12-mux regval value)
  (set-dclkout12-mux-bits regval (value->bits clkout-mux-map value)))

(define (set-dclkout12-polarity regval value)
  (set-dclkout12-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-dclkout2-adly regval value)
  (set-dclkout2-adly-bits regval (conv:analog-delay->bits value)))

(define (set-dclkout2-ddly-cnt-high regval value)
  (set-dclkout2-ddly-cnt-high-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout2-ddly-cnt-low regval value)
  (set-dclkout2-ddly-cnt-low-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout2-div regval value)
  (set-dclkout2-div-bits regval (conv:clkout-divider->bits value)))

(define (set-dclkout2-mux regval value)
  (set-dclkout2-mux-bits regval (value->bits clkout-mux-map value)))

(define (set-dclkout2-polarity regval value)
  (set-dclkout2-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-dclkout4-adly regval value)
  (set-dclkout4-adly-bits regval (conv:analog-delay->bits value)))

(define (set-dclkout4-ddly-cnt-high regval value)
  (set-dclkout4-ddly-cnt-high-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout4-ddly-cnt-low regval value)
  (set-dclkout4-ddly-cnt-low-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout4-div regval value)
  (set-dclkout4-div-bits regval (conv:clkout-divider->bits value)))

(define (set-dclkout4-mux regval value)
  (set-dclkout4-mux-bits regval (value->bits clkout-mux-map value)))

(define (set-dclkout4-polarity regval value)
  (set-dclkout4-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-dclkout6-adly regval value)
  (set-dclkout6-adly-bits regval (conv:analog-delay->bits value)))

(define (set-dclkout6-ddly-cnt-high regval value)
  (set-dclkout6-ddly-cnt-high-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout6-ddly-cnt-low regval value)
  (set-dclkout6-ddly-cnt-low-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout6-div regval value)
  (set-dclkout6-div-bits regval (conv:clkout-divider->bits value)))

(define (set-dclkout6-mux regval value)
  (set-dclkout6-mux-bits regval (value->bits clkout-mux-map value)))

(define (set-dclkout6-polarity regval value)
  (set-dclkout6-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-dclkout8-adly regval value)
  (set-dclkout8-adly-bits regval (conv:analog-delay->bits value)))

(define (set-dclkout8-ddly-cnt-high regval value)
  (set-dclkout8-ddly-cnt-high-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout8-ddly-cnt-low regval value)
  (set-dclkout8-ddly-cnt-low-bits regval (conv:digital-delay-cnt->bits value)))

(define (set-dclkout8-div regval value)
  (set-dclkout8-div-bits regval (conv:clkout-divider->bits value)))

(define (set-dclkout8-mux regval value)
  (set-dclkout8-mux-bits regval (value->bits clkout-mux-map value)))

(define (set-dclkout8-polarity regval value)
  (set-dclkout8-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-ddlyd-step-cnt regval value)
  (with-constraints (value (>= 0) (<= 31))
    (set-ddlyd-step-cnt-bits regval value)))

(define (set-enable-sysref-req regval value)
  (with-constraints (value (>= 0) (<= #b1))
    (set-enable-sysref-req-bits regval value)))

(define (set-fb-mux regval value)
  (set-fb-mux-bits regval (value->bits fb-mux-map value)))

(define (set-fixed-reg:0145 regval value)
  (with-constraints (value (>= 0) (<= #b1111111))
    (set-fixed-reg:0145-bits regval value)))

(define (set-holdover-dld-cnt-high regval value)
  (with-constraints (value (>= 0) (<= #b111111))
    (set-holdover-dld-cnt-high-bits regval value)))

(define (set-holdover-dld-cnt-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-holdover-dld-cnt-low-bits regval value)))

(define (set-id-device-type regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-id-device-type-bits regval value)))

(define (set-id-maskrev regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-id-maskrev-bits regval value)))

(define (set-id-product-high regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-id-product-high-bits regval value)))

(define (set-id-product-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-id-product-low-bits regval value)))

(define (set-id-vendor-high regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-id-vendor-high-bits regval value)))

(define (set-id-vendor-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-id-vendor-low-bits regval value)))

(define (set-los-timeout regval value)
  (set-los-timeout-bits regval (value->bits los-timeout-map value)))

(define (set-manual-dac-high regval value)
  (with-constraints (value (>= 0) (<= #b11))
    (set-manual-dac-high-bits regval value)))

(define (set-manual-dac-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-manual-dac-low-bits regval value)))

(define (set-opt-reg-1 regval value)
  (with-constraints (value (>= 0) (<= 255))
    (set-opt-reg-1-bits regval value)))

(define (set-opt-reg-2 regval value)
  (with-constraints (value (>= 0) (<= 255))
    (set-opt-reg-2-bits regval value)))

(define (set-oscin-freq regval value)
  (set-oscin-freq-bits regval (value->bits oscin-freq-map value)))

(define (set-oscout-format regval value)
  (set-oscout-format-bits regval (value->bits oscout-format-map value)))

(define (set-oscout-mux regval value)
  (set-oscout-mux-bits regval (value->bits oscout-mux-map value)))

(define (set-pll1-cp-gain regval value)
  (set-pll1-cp-gain-bits regval (value->bits pll1-cp-gain-map value)))

(define (set-pll1-cp-polarity regval value)
  (set-pll1-cp-polarity-bits regval (value->bits pll-cp-polarity-map value)))

(define (set-pll1-dld-cnt-high regval value)
  (with-constraints (value (>= 0) (<= #b111111))
    (set-pll1-dld-cnt-high-bits regval value)))

(define (set-pll1-dld-cnt-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-pll1-dld-cnt-low-bits regval value)))

(define (set-pll1-ld-mux regval value)
  (set-pll1-ld-mux-bits regval (value->bits pll-ld-mux-map value)))

(define (set-pll1-ld-type regval value)
  (set-pll1-ld-type-bits regval (value->bits pll-ld-type-map value)))

(define (set-pll1-n-divider-dly regval value)
  (set-pll1-n-divider-dly-bits regval (value->bits pll1-delay-map value)))

(define (set-pll1-n-divider-high regval value)
  (with-constraints (value (>= 0) (<= #b111111))
    (set-pll1-n-divider-high-bits regval value)))

(define (set-pll1-n-divider-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-pll1-n-divider-low-bits regval value)))

(define (set-pll1-nclk-mux regval value)
  (set-pll1-nclk-mux-bits regval (value->bits pll1-nclk-mux-map value)))

(define (set-pll1-r-divider-dly regval value)
  (set-pll1-r-divider-dly-bits regval (value->bits pll1-delay-map value)))

(define (set-pll1-window-size regval value)
  (set-pll1-window-size-bits regval (value->bits pll1-window-size-map value)))

(define (set-pll2-cp-gain regval value)
  (set-pll2-cp-gain-bits regval (value->bits pll2-cp-gain-map value)))

(define (set-pll2-cp-polarity regval value)
  (set-pll2-cp-polarity-bits regval (value->bits pll-cp-polarity-map value)))

(define (set-pll2-dld-cnt-high regval value)
  (with-constraints (value (>= 0) (<= #b111111))
    (set-pll2-dld-cnt-high-bits regval value)))

(define (set-pll2-dld-cnt-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-pll2-dld-cnt-low-bits regval value)))

(define (set-pll2-ld-mux regval value)
  (set-pll2-ld-mux-bits regval (value->bits pll-ld-mux-map value)))

(define (set-pll2-ld-type regval value)
  (set-pll2-ld-type-bits regval (value->bits pll-ld-type-map value)))

(define (set-pll2-loopfilter-c3 regval value)
  (set-pll2-loopfilter-c3-bits regval (value->bits pll2-loopfilter-c3-map value)))

(define (set-pll2-loopfilter-c4 regval value)
  (set-pll2-loopfilter-c4-bits regval (value->bits pll2-loopfilter-c4-map value)))

(define (set-pll2-loopfilter-r3 regval value)
  (set-pll2-loopfilter-r3-bits regval (value->bits pll2-loopfilter-resistor-map value)))

(define (set-pll2-loopfilter-r4 regval value)
  (set-pll2-loopfilter-r4-bits regval (value->bits pll2-loopfilter-resistor-map value)))

(define (set-pll2-n-calibration-phase-high regval value)
  (with-constraints (value (>= 0) (<= #b11))
    (set-pll2-n-calibration-phase-high-bits regval value)))

(define (set-pll2-n-calibration-phase-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-pll2-n-calibration-phase-low-bits regval value)))

(define (set-pll2-n-calibration-phase-mid regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-pll2-n-calibration-phase-mid-bits regval value)))

(define (set-pll2-n-divider-high regval value)
  (with-constraints (value (>= 0) (<= #b11))
    (set-pll2-n-divider-high-bits regval value)))

(define (set-pll2-n-divider-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-pll2-n-divider-low-bits regval value)))

(define (set-pll2-n-divider-mid regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-pll2-n-divider-mid-bits regval value)))

(define (set-pll2-nclk-mux regval value)
  (set-pll2-nclk-mux-bits regval (value->bits pll2-nclk-mux-map value)))

(define (set-pll2-prescaler regval value)
  (set-pll2-prescaler-bits regval (value->bits pll2-prescaler-map value)))

(define (set-pll2-r-divider-high regval value)
  (with-constraints (value (>= 0) (<= #b1111))
    (set-pll2-r-divider-high-bits regval value)))

(define (set-pll2-r-divider-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-pll2-r-divider-low-bits regval value)))

(define (set-pll2-window-size regval value)
  (set-pll2-window-size-bits regval (value->bits pll2-window-size-map value)))

(define (set-reset-mux regval value)
  (set-reset-mux-bits regval (value->bits reset-mux-map value)))

(define (set-reset-type regval value)
  (set-reset-type-bits regval (value->bits pin-type-map value)))

(define (set-sdclkout1-adly regval value)
  (set-sdclkout1-adly-bits regval (conv:sysout-analog-delay->bits value)))

(define (set-sdclkout1-ddly regval value)
  (set-sdclkout1-ddly-bits regval (value->bits sdclkout-ddelay-map value)))

(define (set-sdclkout1-disable-mode regval value)
  (set-sdclkout1-disable-mode-bits regval (value->bits sysref-output-state-map value)))

(define (set-sdclkout1-mux regval value)
  (set-sdclkout1-mux-bits regval (value->bits devclk-sysref-map value)))

(define (set-sdclkout1-polarity regval value)
  (set-sdclkout1-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-sdclkout11-adly regval value)
  (set-sdclkout11-adly-bits regval (conv:sysout-analog-delay->bits value)))

(define (set-sdclkout11-ddly regval value)
  (set-sdclkout11-ddly-bits regval (value->bits sdclkout-ddelay-map value)))

(define (set-sdclkout11-disable-mode regval value)
  (set-sdclkout11-disable-mode-bits regval (value->bits sysref-output-state-map value)))

(define (set-sdclkout11-mux regval value)
  (set-sdclkout11-mux-bits regval (value->bits devclk-sysref-map value)))

(define (set-sdclkout11-polarity regval value)
  (set-sdclkout11-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-sdclkout13-adly regval value)
  (set-sdclkout13-adly-bits regval (conv:sysout-analog-delay->bits value)))

(define (set-sdclkout13-ddly regval value)
  (set-sdclkout13-ddly-bits regval (value->bits sdclkout-ddelay-map value)))

(define (set-sdclkout13-disable-mode regval value)
  (set-sdclkout13-disable-mode-bits regval (value->bits sysref-output-state-map value)))

(define (set-sdclkout13-mux regval value)
  (set-sdclkout13-mux-bits regval (value->bits devclk-sysref-map value)))

(define (set-sdclkout13-polarity regval value)
  (set-sdclkout13-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-sdclkout3-adly regval value)
  (set-sdclkout3-adly-bits regval (conv:sysout-analog-delay->bits value)))

(define (set-sdclkout3-ddly regval value)
  (set-sdclkout3-ddly-bits regval (value->bits sdclkout-ddelay-map value)))

(define (set-sdclkout3-disable-mode regval value)
  (set-sdclkout3-disable-mode-bits regval (value->bits sysref-output-state-map value)))

(define (set-sdclkout3-mux regval value)
  (set-sdclkout3-mux-bits regval (value->bits devclk-sysref-map value)))

(define (set-sdclkout3-polarity regval value)
  (set-sdclkout3-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-sdclkout5-adly regval value)
  (set-sdclkout5-adly-bits regval (conv:sysout-analog-delay->bits value)))

(define (set-sdclkout5-ddly regval value)
  (set-sdclkout5-ddly-bits regval (value->bits sdclkout-ddelay-map value)))

(define (set-sdclkout5-disable-mode regval value)
  (set-sdclkout5-disable-mode-bits regval (value->bits sysref-output-state-map value)))

(define (set-sdclkout5-mux regval value)
  (set-sdclkout5-mux-bits regval (value->bits devclk-sysref-map value)))

(define (set-sdclkout5-polarity regval value)
  (set-sdclkout5-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-sdclkout7-adly regval value)
  (set-sdclkout7-adly-bits regval (conv:sysout-analog-delay->bits value)))

(define (set-sdclkout7-ddly regval value)
  (set-sdclkout7-ddly-bits regval (value->bits sdclkout-ddelay-map value)))

(define (set-sdclkout7-disable-mode regval value)
  (set-sdclkout7-disable-mode-bits regval (value->bits sysref-output-state-map value)))

(define (set-sdclkout7-mux regval value)
  (set-sdclkout7-mux-bits regval (value->bits devclk-sysref-map value)))

(define (set-sdclkout7-polarity regval value)
  (set-sdclkout7-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-sdclkout9-adly regval value)
  (set-sdclkout9-adly-bits regval (conv:sysout-analog-delay->bits value)))

(define (set-sdclkout9-ddly regval value)
  (set-sdclkout9-ddly-bits regval (value->bits sdclkout-ddelay-map value)))

(define (set-sdclkout9-disable-mode regval value)
  (set-sdclkout9-disable-mode-bits regval (value->bits sysref-output-state-map value)))

(define (set-sdclkout9-mux regval value)
  (set-sdclkout9-mux-bits regval (value->bits devclk-sysref-map value)))

(define (set-sdclkout9-polarity regval value)
  (set-sdclkout9-polarity-bits regval (value->bits output-polarity-map value)))

(define (set-sdio-readback-type regval value)
  (set-sdio-readback-type-bits regval (value->bits sdio-readback-type-map value)))

(define (enable-spi-lock-high regval) 255)
(define (disable-spi-lock-high regval) 0)
(define (enable-spi-lock-mid regval) 255)
(define (disable-spi-lock-mid regval) 0)
(define (enable-spi-lock-low regval) 255)
(define (disable-spi-lock-low regval) 83)

(define (set-sync-mode regval value)
  (set-sync-mode-bits regval (value->bits sync-mode-map value)))

(define (set-sync-polarity regval value)
  (set-sync-polarity-bits regval (value->bits sync-polarity-map value)))

(define (set-sysref-ddly-high regval value)
  (with-constraints (value (>= 0) (<= #b11111))
    (set-sysref-ddly-high-bits regval value)))

(define (set-sysref-ddly-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-sysref-ddly-low-bits regval value)))

(define (set-sysref-div-high regval value)
  (with-constraints (value (>= 0) (<= #b11111))
    (set-sysref-div-high-bits regval value)))

(define (set-sysref-div-low regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-sysref-div-low-bits regval value)))

(define (set-sysref-mux regval value)
  (set-sysref-mux-bits regval (value->bits sysref-mux-map value)))

(define (power-of-two? x)
  (= 1 (logcount x)))

(define (set-sysref-pulse-cnt regval value)
  (with-constraints (value (>= 1) (<= 8) (power-of-two?))
    (set-sysref-pulse-cnt-bits regval (inexact->exact (/ (log10 value)
                                                         (log10 2))))))

(define (set-vco-mux regval value)
  (set-vco-mux-bits regval (value->bits vco-mux-map value)))
