;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti lmk04828)
  #:use-module (ice-9 optargs)
  #:use-module (bitops)
  #:use-module (chip-remote decode)
  #:use-module (chip-remote level-3)
  #:use-module (chip-remote protocol)
  #:use-module ((chip-remote devices ti lmk04828 program)
                #:renamer (symbol-prefix-proc 'lvl2/))
  #:use-module (chip-remote devices ti lmk04828 registers)
  #:export (decode-device
            decode-register
            decode-register-value
            read-register
            setup-connection
            write-register))

(define (setup-connection conn index)
  (throw 'cr-setup-connection-not-implemented-yet))

(define (read-register conn addr)
  (throw 'cr-read-register-not-implemented-yet))

(define (write-register conn addr value)
  (throw 'cr-write-register-not-implemented-yet))

(define* (decode-register-value addr value #:key (colour? (to-tty?)))
  (value-decoder lmk04828-register-map lmk04828-register-width addr value colour?))

(define* (decode-register conn addr #:key (colour? (to-tty?)))
  (decode-register-value addr (read-register conn addr) #:colour? colour?))

(define* (decode-device conn #:key (colour? (to-tty?)))
  (device-decoder #:register-map lmk04828-register-map
                  #:reader (lambda (a) (read-register conn a))
                  #:decoder (lambda (a v) (decode lmk04828-register-map a v))
                  #:interconnections lmk04828-register-interconns
                  #:filter-predicate #f
                  #:width lmk04828-register-width
                  #:colour? colour?))

;; Register post-processors:
(define (postproc:x145 value) #b1111111)
(define (postproc:x169 value) (logior value #b1))
(define (postproc:x17c value) #b10101)
(define (postproc:x17d value) #b110011)

(define-bit-field-frontends
  (disable-clear-pll1-ld-lost regaddr:clear-pll1-ld-lost
                              lvl2/disable-clear-pll1-ld-lost)
  (disable-clkout0-1-idl regaddr:clkout0-1-idl
                         lvl2/disable-clkout0-1-idl)
  (disable-clkout0-1-odl regaddr:clkout0-1-odl
                         lvl2/disable-clkout0-1-odl)
  (disable-clkout0-1-powerdown regaddr:clkout0-1-powerdown
                               lvl2/disable-clkout0-1-powerdown)
  (disable-clkout10-11-idl regaddr:clkout10-11-idl
                           lvl2/disable-clkout10-11-idl)
  (disable-clkout10-11-odl regaddr:clkout10-11-odl
                           lvl2/disable-clkout10-11-odl)
  (disable-clkout10-11-powerdown regaddr:clkout10-11-powerdown
                                 lvl2/disable-clkout10-11-powerdown)
  (disable-clkout12-13-idl regaddr:clkout12-13-idl
                           lvl2/disable-clkout12-13-idl)
  (disable-clkout12-13-odl regaddr:clkout12-13-odl
                           lvl2/disable-clkout12-13-odl)
  (disable-clkout12-13-powerdown regaddr:clkout12-13-powerdown
                                 lvl2/disable-clkout12-13-powerdown)
  (disable-clkout2-3-idl regaddr:clkout2-3-idl
                         lvl2/disable-clkout2-3-idl)
  (disable-clkout2-3-odl regaddr:clkout2-3-odl
                         lvl2/disable-clkout2-3-odl)
  (disable-clkout2-3-powerdown regaddr:clkout2-3-powerdown
                               lvl2/disable-clkout2-3-powerdown)
  (disable-clkout4-5-idl regaddr:clkout4-5-idl
                         lvl2/disable-clkout4-5-idl)
  (disable-clkout4-5-odl regaddr:clkout4-5-odl
                         lvl2/disable-clkout4-5-odl)
  (disable-clkout4-5-powerdown regaddr:clkout4-5-powerdown
                               lvl2/disable-clkout4-5-powerdown)
  (disable-clkout6-7-odl regaddr:clkout6-7-odl
                         lvl2/disable-clkout6-7-odl)
  (disable-clkout6-7-powerdown regaddr:clkout6-7-powerdown
                               lvl2/disable-clkout6-7-powerdown)
  (disable-clkout6-8-idl regaddr:clkout6-8-idl
                         lvl2/disable-clkout6-8-idl)
  (disable-clkout8-9-idl regaddr:clkout8-9-idl
                         lvl2/disable-clkout8-9-idl)
  (disable-clkout8-9-odl regaddr:clkout8-9-odl
                         lvl2/disable-clkout8-9-odl)
  (disable-clkout8-9-powerdown regaddr:clkout8-9-powerdown
                               lvl2/disable-clkout8-9-powerdown)
  (disable-clr-pll2-ld-lost regaddr:clr-pll2-ld-lost
                            lvl2/disable-clr-pll2-ld-lost)
  (disable-dclkout0-adly-mux regaddr:dclkout0-adly-mux
                             lvl2/disable-dclkout0-adly-mux)
  (disable-dclkout0-adly-powerdown regaddr:dclkout0-adly-powerdown
                                   lvl2/disable-dclkout0-adly-powerdown)
  (disable-dclkout0-adlyg-powerdown regaddr:dclkout0-adlyg-powerdown
                                    lvl2/disable-dclkout0-adlyg-powerdown)
  (disable-dclkout0-ddly-powerdown regaddr:dclkout0-ddly-powerdown
                                   lvl2/disable-dclkout0-ddly-powerdown)
  (disable-dclkout0-hs regaddr:dclkout0-hs
                       lvl2/disable-dclkout0-hs)
  (disable-dclkout0-hsg-powerdown regaddr:dclkout0-hsg-powerdown
                                  lvl2/disable-dclkout0-hsg-powerdown)
  (disable-dclkout10-adly-mux regaddr:dclkout10-adly-mux
                              lvl2/disable-dclkout10-adly-mux)
  (disable-dclkout10-adly-powerdown regaddr:dclkout10-adly-powerdown
                                    lvl2/disable-dclkout10-adly-powerdown)
  (disable-dclkout10-ddly-powerdown regaddr:dclkout10-ddly-powerdown
                                    lvl2/disable-dclkout10-ddly-powerdown)
  (disable-dclkout10-hs regaddr:dclkout10-hs
                        lvl2/disable-dclkout10-hs)
  (disable-dclkout10-hsg-powerdown regaddr:dclkout10-hsg-powerdown
                                   lvl2/disable-dclkout10-hsg-powerdown)
  (disable-dclkout12-adly-mux regaddr:dclkout12-adly-mux
                              lvl2/disable-dclkout12-adly-mux)
  (disable-dclkout12-adly-powerdown regaddr:dclkout12-adly-powerdown
                                    lvl2/disable-dclkout12-adly-powerdown)
  (disable-dclkout12-adlyg-powerdown regaddr:dclkout12-adlyg-powerdown
                                     lvl2/disable-dclkout12-adlyg-powerdown)
  (disable-dclkout12-ddly-powerdown regaddr:dclkout12-ddly-powerdown
                                    lvl2/disable-dclkout12-ddly-powerdown)
  (disable-dclkout12-hs regaddr:dclkout12-hs
                        lvl2/disable-dclkout12-hs)
  (disable-dclkout12-hsg-powerdown regaddr:dclkout12-hsg-powerdown
                                   lvl2/disable-dclkout12-hsg-powerdown)
  (disable-dclkout2-adly-mux regaddr:dclkout2-adly-mux
                             lvl2/disable-dclkout2-adly-mux)
  (disable-dclkout2-adly-powerdown regaddr:dclkout2-adly-powerdown
                                   lvl2/disable-dclkout2-adly-powerdown)
  (disable-dclkout2-adlyg-powerdown regaddr:dclkout2-adlyg-powerdown
                                    lvl2/disable-dclkout2-adlyg-powerdown)
  (disable-dclkout2-ddly-powerdown regaddr:dclkout2-ddly-powerdown
                                   lvl2/disable-dclkout2-ddly-powerdown)
  (disable-dclkout2-hs regaddr:dclkout2-hs
                       lvl2/disable-dclkout2-hs)
  (disable-dclkout2-hsg-powerdown regaddr:dclkout2-hsg-powerdown
                                  lvl2/disable-dclkout2-hsg-powerdown)
  (disable-dclkout4-adly-mux regaddr:dclkout4-adly-mux
                             lvl2/disable-dclkout4-adly-mux)
  (disable-dclkout4-adly-powerdown regaddr:dclkout4-adly-powerdown
                                   lvl2/disable-dclkout4-adly-powerdown)
  (disable-dclkout4-adlyg-powerdown regaddr:dclkout4-adlyg-powerdown
                                    lvl2/disable-dclkout4-adlyg-powerdown)
  (disable-dclkout4-ddly-powerdown regaddr:dclkout4-ddly-powerdown
                                   lvl2/disable-dclkout4-ddly-powerdown)
  (disable-dclkout4-hs regaddr:dclkout4-hs
                       lvl2/disable-dclkout4-hs)
  (disable-dclkout4-hsg-powerdown regaddr:dclkout4-hsg-powerdown
                                  lvl2/disable-dclkout4-hsg-powerdown)
  (disable-dclkout6-adly-mux regaddr:dclkout6-adly-mux
                             lvl2/disable-dclkout6-adly-mux)
  (disable-dclkout6-adly-powerdown regaddr:dclkout6-adly-powerdown
                                   lvl2/disable-dclkout6-adly-powerdown)
  (disable-dclkout6-adlyg-powerdown regaddr:dclkout6-adlyg-powerdown
                                    lvl2/disable-dclkout6-adlyg-powerdown)
  (disable-dclkout6-ddly-powerdown regaddr:dclkout6-ddly-powerdown
                                   lvl2/disable-dclkout6-ddly-powerdown)
  (disable-dclkout6-hs regaddr:dclkout6-hs
                       lvl2/disable-dclkout6-hs)
  (disable-dclkout6-hsg-powerdown regaddr:dclkout6-hsg-powerdown
                                  lvl2/disable-dclkout6-hsg-powerdown)
  (disable-dclkout8-adly-mux regaddr:dclkout8-adly-mux
                             lvl2/disable-dclkout8-adly-mux)
  (disable-dclkout8-adly-powerdown regaddr:dclkout8-adly-powerdown
                                   lvl2/disable-dclkout8-adly-powerdown)
  (disable-dclkout8-adlyg-powerdown regaddr:dclkout8-adlyg-powerdown
                                    lvl2/disable-dclkout8-adlyg-powerdown)
  (disable-dclkout8-ddly-powerdown regaddr:dclkout8-ddly-powerdown
                                   lvl2/disable-dclkout8-ddly-powerdown)
  (disable-dclkout8-hs regaddr:dclkout8-hs
                       lvl2/disable-dclkout8-hs)
  (disable-dclkout8-hsg-powerdown regaddr:dclkout8-hsg-powerdown
                                  lvl2/disable-dclkout8-hsg-powerdown)
  (enable-sync-0 regaddr:disable-sync-0
                 lvl2/enable-sync-0)
  (enable-sync-10 regaddr:disable-sync-10
                  lvl2/enable-sync-10)
  (enable-sync-12 regaddr:disable-sync-12
                  lvl2/enable-sync-12)
  (enable-sync-2 regaddr:disable-sync-2
                 lvl2/enable-sync-2)
  (enable-sync-4 regaddr:disable-sync-4
                 lvl2/enable-sync-4)
  (enable-sync-6 regaddr:disable-sync-6
                 lvl2/enable-sync-6)
  (enable-sync-8 regaddr:disable-sync-8
                 lvl2/enable-sync-8)
  (enable-sync-sysref regaddr:disable-sync-sysref
                      lvl2/enable-sync-sysref)
  (disable-dlclkout10-adlyg-powerdown regaddr:dlclkout10-adlyg-powerdown
                                      lvl2/disable-dlclkout10-adlyg-powerdown)
  (disable-clkin-0-auto-mode regaddr:enable-clkin-0-auto-mode
                             lvl2/disable-clkin-0-auto-mode)
  (disable-clkin-1-auto-mode regaddr:enable-clkin-1-auto-mode
                             lvl2/disable-clkin-1-auto-mode)
  (disable-clkin-2-auto-mode regaddr:enable-clkin-2-auto-mode
                             lvl2/disable-clkin-2-auto-mode)
  (disable-ddlyd-sysref regaddr:enable-ddlyd-sysref
                        lvl2/disable-ddlyd-sysref)
  (disable-ddlyd0 regaddr:enable-ddlyd0
                  lvl2/disable-ddlyd0)
  (disable-ddlyd10 regaddr:enable-ddlyd10
                   lvl2/disable-ddlyd10)
  (disable-ddlyd12 regaddr:enable-ddlyd12
                   lvl2/disable-ddlyd12)
  (disable-ddlyd2 regaddr:enable-ddlyd2
                  lvl2/disable-ddlyd2)
  (disable-ddlyd4 regaddr:enable-ddlyd4
                  lvl2/disable-ddlyd4)
  (disable-ddlyd6 regaddr:enable-ddlyd6
                  lvl2/disable-ddlyd6)
  (disable-ddlyd7 regaddr:enable-ddlyd7
                  lvl2/disable-ddlyd7)
  (disable-fb-mux regaddr:enable-fb-mux
                  lvl2/disable-fb-mux)
  (disable-holdover regaddr:enable-holdover
                    lvl2/disable-holdover)
  (disable-los regaddr:enable-los
               lvl2/disable-los)
  (disable-manual-dac regaddr:enable-manual-dac
                      lvl2/disable-manual-dac)
  (disable-pll2-freq-calibration regaddr:enable-pll2-freq-calibration
                                 lvl2/disable-pll2-freq-calibration)
  (disable-pll2-ref-2x regaddr:enable-pll2-ref-2x
                       lvl2/disable-pll2-ref-2x)
  (disable-pll2-xtal regaddr:enable-pll2-xtal
                     lvl2/disable-pll2-xtal)
  (disable-sdcklout11-adly regaddr:enable-sdcklout11-adly
                           lvl2/disable-sdcklout11-adly)
  (disable-sdclkout1-adly regaddr:enable-sdclkout1-adly
                          lvl2/disable-sdclkout1-adly)
  (disable-sdclkout13-adly regaddr:enable-sdclkout13-adly
                           lvl2/disable-sdclkout13-adly)
  (disable-sdclkout3-adly regaddr:enable-sdclkout3-adly
                          lvl2/disable-sdclkout3-adly)
  (disable-sdclkout5-adly regaddr:enable-sdclkout5-adly
                          lvl2/disable-sdclkout5-adly)
  (disable-sdclkout7-adly regaddr:enable-sdclkout7-adly
                          lvl2/disable-sdclkout7-adly)
  (disable-sdclkout9-adly regaddr:enable-sdclkout9-adly
                          lvl2/disable-sdclkout9-adly)
  (disable-spi-three-wire regaddr:enable-spi-three-wire
                          lvl2/disable-spi-three-wire)
  (disable-sync regaddr:enable-sync
                lvl2/disable-sync)
  (disable-sync-1shot regaddr:enable-sync-1shot
                      lvl2/disable-sync-1shot)
  (disable-track regaddr:enable-track
                 lvl2/disable-track)
  (disable-holdover-force regaddr:holdover-force
                          lvl2/disable-holdover-force)
  (disable-holdover-hitless-switch regaddr:holdover-hitless-switch
                                   lvl2/disable-holdover-hitless-switch)
  (disable-holdover-los-detect regaddr:holdover-los-detect
                               lvl2/disable-holdover-los-detect)
  (disable-holdover-pll1-detect regaddr:holdover-pll1-detect
                                lvl2/disable-holdover-pll1-detect)
  (disable-holdover-vtune-detect regaddr:holdover-vtune-detect
                                 lvl2/disable-holdover-vtune-detect)
  (disable-oscin-powerdown regaddr:oscin-powerdown
                           lvl2/disable-oscin-powerdown)
  (disable-pll1-cp-tri-state regaddr:pll1-cp-tri-state
                             lvl2/disable-pll1-cp-tri-state)
  (disable-pll1-powerdown regaddr:pll1-powerdown
                          lvl2/disable-pll1-powerdown)
  (disable-pll2-cp-tri-state regaddr:pll2-cp-tri-state
                             lvl2/disable-pll2-cp-tri-state
                             postproc:x169)
  (disable-pll2-powerdown regaddr:pll2-powerdown
                          lvl2/disable-pll2-powerdown)
  (disable-pll2-prescaler-powerdown regaddr:pll2-prescaler-powerdown
                                    lvl2/disable-pll2-prescaler-powerdown)
  (disable-powerdown regaddr:powerdown
                     lvl2/disable-powerdown)
  (disable-reset regaddr:reset
                 lvl2/disable-reset)
  (disable-sdclkout1-hs regaddr:sdclkout1-hs
                        lvl2/disable-sdclkout1-hs)
  (disable-sdclkout1-powerdown regaddr:sdclkout1-powerdown
                               lvl2/disable-sdclkout1-powerdown)
  (disable-sdclkout11-hs regaddr:sdclkout11-hs
                         lvl2/disable-sdclkout11-hs)
  (disable-sdclkout11-powerdown regaddr:sdclkout11-powerdown
                                lvl2/disable-sdclkout11-powerdown)
  (disable-sdclkout13-hs regaddr:sdclkout13-hs
                         lvl2/disable-sdclkout13-hs)
  (disable-sdclkout13-powerdown regaddr:sdclkout13-powerdown
                                lvl2/disable-sdclkout13-powerdown)
  (disable-sdclkout3-hs regaddr:sdclkout3-hs
                        lvl2/disable-sdclkout3-hs)
  (disable-sdclkout3-powerdown regaddr:sdclkout3-powerdown
                               lvl2/disable-sdclkout3-powerdown)
  (disable-sdclkout5-hs regaddr:sdclkout5-hs
                        lvl2/disable-sdclkout5-hs)
  (disable-sdclkout5-powerdown regaddr:sdclkout5-powerdown
                               lvl2/disable-sdclkout5-powerdown)
  (disable-sdclkout7-hs regaddr:sdclkout7-hs
                        lvl2/disable-sdclkout7-hs)
  (disable-sdclkout7-powerdown regaddr:sdclkout7-powerdown
                               lvl2/disable-sdclkout7-powerdown)
  (disable-sdclkout9-hs regaddr:sdclkout9-hs
                        lvl2/disable-sdclkout9-hs)
  (disable-sdclkout9-powerdown regaddr:sdclkout9-powerdown
                               lvl2/disable-sdclkout9-powerdown)
  (disable-sync-pll1-dld regaddr:sync-pll1-dld
                         lvl2/disable-sync-pll1-dld)
  (disable-sync-pll2-dld regaddr:sync-pll2-dld
                         lvl2/disable-sync-pll2-dld)
  (disable-sysref-ddly-clr regaddr:sysref-ddly-clr
                           lvl2/disable-sysref-ddly-clr)
  (disable-sysref-ddly-powerdown regaddr:sysref-ddly-powerdown
                                 lvl2/disable-sysref-ddly-powerdown)
  (disable-sysref-global-powerdown regaddr:sysref-global-powerdown
                                   lvl2/disable-sysref-global-powerdown)
  (disable-sysref-plsr-powerdown regaddr:sysref-plsr-powerdown
                                 lvl2/disable-sysref-plsr-powerdown)
  (disable-sysref-powerdown regaddr:sysref-powerdown
                            lvl2/disable-sysref-powerdown)
  (disable-vco-ldo-powerdown regaddr:vco-ldo-powerdown
                             lvl2/disable-vco-ldo-powerdown)
  (disable-vco-powerdown regaddr:vco-powerdown
                         lvl2/disable-vco-powerdown)
  (enable-clear-pll1-ld-lost regaddr:clear-pll1-ld-lost
                             lvl2/enable-clear-pll1-ld-lost)
  (enable-clkout0-1-idl regaddr:clkout0-1-idl
                        lvl2/enable-clkout0-1-idl)
  (enable-clkout0-1-odl regaddr:clkout0-1-odl
                        lvl2/enable-clkout0-1-odl)
  (enable-clkout0-1-powerdown regaddr:clkout0-1-powerdown
                              lvl2/enable-clkout0-1-powerdown)
  (enable-clkout10-11-idl regaddr:clkout10-11-idl
                          lvl2/enable-clkout10-11-idl)
  (enable-clkout10-11-odl regaddr:clkout10-11-odl
                          lvl2/enable-clkout10-11-odl)
  (enable-clkout10-11-powerdown regaddr:clkout10-11-powerdown
                                lvl2/enable-clkout10-11-powerdown)
  (enable-clkout12-13-idl regaddr:clkout12-13-idl
                          lvl2/enable-clkout12-13-idl)
  (enable-clkout12-13-odl regaddr:clkout12-13-odl
                          lvl2/enable-clkout12-13-odl)
  (enable-clkout12-13-powerdown regaddr:clkout12-13-powerdown
                                lvl2/enable-clkout12-13-powerdown)
  (enable-clkout2-3-idl regaddr:clkout2-3-idl
                        lvl2/enable-clkout2-3-idl)
  (enable-clkout2-3-odl regaddr:clkout2-3-odl
                        lvl2/enable-clkout2-3-odl)
  (enable-clkout2-3-powerdown regaddr:clkout2-3-powerdown
                              lvl2/enable-clkout2-3-powerdown)
  (enable-clkout4-5-idl regaddr:clkout4-5-idl
                        lvl2/enable-clkout4-5-idl)
  (enable-clkout4-5-odl regaddr:clkout4-5-odl
                        lvl2/enable-clkout4-5-odl)
  (enable-clkout4-5-powerdown regaddr:clkout4-5-powerdown
                              lvl2/enable-clkout4-5-powerdown)
  (enable-clkout6-7-odl regaddr:clkout6-7-odl
                        lvl2/enable-clkout6-7-odl)
  (enable-clkout6-7-powerdown regaddr:clkout6-7-powerdown
                              lvl2/enable-clkout6-7-powerdown)
  (enable-clkout6-8-idl regaddr:clkout6-8-idl
                        lvl2/enable-clkout6-8-idl)
  (enable-clkout8-9-idl regaddr:clkout8-9-idl
                        lvl2/enable-clkout8-9-idl)
  (enable-clkout8-9-odl regaddr:clkout8-9-odl
                        lvl2/enable-clkout8-9-odl)
  (enable-clkout8-9-powerdown regaddr:clkout8-9-powerdown
                              lvl2/enable-clkout8-9-powerdown)
  (enable-clr-pll2-ld-lost regaddr:clr-pll2-ld-lost
                           lvl2/enable-clr-pll2-ld-lost)
  (enable-dclkout0-adly-mux regaddr:dclkout0-adly-mux
                            lvl2/enable-dclkout0-adly-mux)
  (enable-dclkout0-adly-powerdown regaddr:dclkout0-adly-powerdown
                                  lvl2/enable-dclkout0-adly-powerdown)
  (enable-dclkout0-adlyg-powerdown regaddr:dclkout0-adlyg-powerdown
                                   lvl2/enable-dclkout0-adlyg-powerdown)
  (enable-dclkout0-ddly-powerdown regaddr:dclkout0-ddly-powerdown
                                  lvl2/enable-dclkout0-ddly-powerdown)
  (enable-dclkout0-hs regaddr:dclkout0-hs
                      lvl2/enable-dclkout0-hs)
  (enable-dclkout0-hsg-powerdown regaddr:dclkout0-hsg-powerdown
                                 lvl2/enable-dclkout0-hsg-powerdown)
  (enable-dclkout10-adly-mux regaddr:dclkout10-adly-mux
                             lvl2/enable-dclkout10-adly-mux)
  (enable-dclkout10-adly-powerdown regaddr:dclkout10-adly-powerdown
                                   lvl2/enable-dclkout10-adly-powerdown)
  (enable-dclkout10-ddly-powerdown regaddr:dclkout10-ddly-powerdown
                                   lvl2/enable-dclkout10-ddly-powerdown)
  (enable-dclkout10-hs regaddr:dclkout10-hs
                       lvl2/enable-dclkout10-hs)
  (enable-dclkout10-hsg-powerdown regaddr:dclkout10-hsg-powerdown
                                  lvl2/enable-dclkout10-hsg-powerdown)
  (enable-dclkout12-adly-mux regaddr:dclkout12-adly-mux
                             lvl2/enable-dclkout12-adly-mux)
  (enable-dclkout12-adly-powerdown regaddr:dclkout12-adly-powerdown
                                   lvl2/enable-dclkout12-adly-powerdown)
  (enable-dclkout12-adlyg-powerdown regaddr:dclkout12-adlyg-powerdown
                                    lvl2/enable-dclkout12-adlyg-powerdown)
  (enable-dclkout12-ddly-powerdown regaddr:dclkout12-ddly-powerdown
                                   lvl2/enable-dclkout12-ddly-powerdown)
  (enable-dclkout12-hs regaddr:dclkout12-hs
                       lvl2/enable-dclkout12-hs)
  (enable-dclkout12-hsg-powerdown regaddr:dclkout12-hsg-powerdown
                                  lvl2/enable-dclkout12-hsg-powerdown)
  (enable-dclkout2-adly-mux regaddr:dclkout2-adly-mux
                            lvl2/enable-dclkout2-adly-mux)
  (enable-dclkout2-adly-powerdown regaddr:dclkout2-adly-powerdown
                                  lvl2/enable-dclkout2-adly-powerdown)
  (enable-dclkout2-adlyg-powerdown regaddr:dclkout2-adlyg-powerdown
                                   lvl2/enable-dclkout2-adlyg-powerdown)
  (enable-dclkout2-ddly-powerdown regaddr:dclkout2-ddly-powerdown
                                  lvl2/enable-dclkout2-ddly-powerdown)
  (enable-dclkout2-hs regaddr:dclkout2-hs
                      lvl2/enable-dclkout2-hs)
  (enable-dclkout2-hsg-powerdown regaddr:dclkout2-hsg-powerdown
                                 lvl2/enable-dclkout2-hsg-powerdown)
  (enable-dclkout4-adly-mux regaddr:dclkout4-adly-mux
                            lvl2/enable-dclkout4-adly-mux)
  (enable-dclkout4-adly-powerdown regaddr:dclkout4-adly-powerdown
                                  lvl2/enable-dclkout4-adly-powerdown)
  (enable-dclkout4-adlyg-powerdown regaddr:dclkout4-adlyg-powerdown
                                   lvl2/enable-dclkout4-adlyg-powerdown)
  (enable-dclkout4-ddly-powerdown regaddr:dclkout4-ddly-powerdown
                                  lvl2/enable-dclkout4-ddly-powerdown)
  (enable-dclkout4-hs regaddr:dclkout4-hs
                      lvl2/enable-dclkout4-hs)
  (enable-dclkout4-hsg-powerdown regaddr:dclkout4-hsg-powerdown
                                 lvl2/enable-dclkout4-hsg-powerdown)
  (enable-dclkout6-adly-mux regaddr:dclkout6-adly-mux
                            lvl2/enable-dclkout6-adly-mux)
  (enable-dclkout6-adly-powerdown regaddr:dclkout6-adly-powerdown
                                  lvl2/enable-dclkout6-adly-powerdown)
  (enable-dclkout6-adlyg-powerdown regaddr:dclkout6-adlyg-powerdown
                                   lvl2/enable-dclkout6-adlyg-powerdown)
  (enable-dclkout6-ddly-powerdown regaddr:dclkout6-ddly-powerdown
                                  lvl2/enable-dclkout6-ddly-powerdown)
  (enable-dclkout6-hs regaddr:dclkout6-hs
                      lvl2/enable-dclkout6-hs)
  (enable-dclkout6-hsg-powerdown regaddr:dclkout6-hsg-powerdown
                                 lvl2/enable-dclkout6-hsg-powerdown)
  (enable-dclkout8-adly-mux regaddr:dclkout8-adly-mux
                            lvl2/enable-dclkout8-adly-mux)
  (enable-dclkout8-adly-powerdown regaddr:dclkout8-adly-powerdown
                                  lvl2/enable-dclkout8-adly-powerdown)
  (enable-dclkout8-adlyg-powerdown regaddr:dclkout8-adlyg-powerdown
                                   lvl2/enable-dclkout8-adlyg-powerdown)
  (enable-dclkout8-ddly-powerdown regaddr:dclkout8-ddly-powerdown
                                  lvl2/enable-dclkout8-ddly-powerdown)
  (enable-dclkout8-hs regaddr:dclkout8-hs
                      lvl2/enable-dclkout8-hs)
  (enable-dclkout8-hsg-powerdown regaddr:dclkout8-hsg-powerdown
                                 lvl2/enable-dclkout8-hsg-powerdown)
  (disable-sync-0 regaddr:disable-sync-0
                  lvl2/disable-sync-0)
  (disable-sync-10 regaddr:disable-sync-10
                   lvl2/disable-sync-10)
  (disable-sync-12 regaddr:disable-sync-12
                   lvl2/disable-sync-12)
  (disable-sync-2 regaddr:disable-sync-2
                  lvl2/disable-sync-2)
  (disable-sync-4 regaddr:disable-sync-4
                  lvl2/disable-sync-4)
  (disable-sync-6 regaddr:disable-sync-6
                  lvl2/disable-sync-6)
  (disable-sync-8 regaddr:disable-sync-8
                  lvl2/disable-sync-8)
  (disable-sync-sysref regaddr:disable-sync-sysref
                       lvl2/disable-sync-sysref)
  (enable-dlclkout10-adlyg-powerdown regaddr:dlclkout10-adlyg-powerdown
                                     lvl2/enable-dlclkout10-adlyg-powerdown)
  (enable-clkin-0-auto-mode regaddr:enable-clkin-0-auto-mode
                            lvl2/enable-clkin-0-auto-mode)
  (enable-clkin-1-auto-mode regaddr:enable-clkin-1-auto-mode
                            lvl2/enable-clkin-1-auto-mode)
  (enable-clkin-2-auto-mode regaddr:enable-clkin-2-auto-mode
                            lvl2/enable-clkin-2-auto-mode)
  (enable-ddlyd-sysref regaddr:enable-ddlyd-sysref
                       lvl2/enable-ddlyd-sysref)
  (enable-ddlyd0 regaddr:enable-ddlyd0
                 lvl2/enable-ddlyd0)
  (enable-ddlyd10 regaddr:enable-ddlyd10
                  lvl2/enable-ddlyd10)
  (enable-ddlyd12 regaddr:enable-ddlyd12
                  lvl2/enable-ddlyd12)
  (enable-ddlyd2 regaddr:enable-ddlyd2
                 lvl2/enable-ddlyd2)
  (enable-ddlyd4 regaddr:enable-ddlyd4
                 lvl2/enable-ddlyd4)
  (enable-ddlyd6 regaddr:enable-ddlyd6
                 lvl2/enable-ddlyd6)
  (enable-ddlyd7 regaddr:enable-ddlyd7
                 lvl2/enable-ddlyd7)
  (enable-fb-mux regaddr:enable-fb-mux
                 lvl2/enable-fb-mux)
  (enable-holdover regaddr:enable-holdover
                   lvl2/enable-holdover)
  (enable-los regaddr:enable-los
              lvl2/enable-los)
  (enable-manual-dac regaddr:enable-manual-dac
                     lvl2/enable-manual-dac)
  (enable-pll2-freq-calibration regaddr:enable-pll2-freq-calibration
                                lvl2/enable-pll2-freq-calibration)
  (enable-pll2-ref-2x regaddr:enable-pll2-ref-2x
                      lvl2/enable-pll2-ref-2x)
  (enable-pll2-xtal regaddr:enable-pll2-xtal
                    lvl2/enable-pll2-xtal)
  (enable-sdcklout11-adly regaddr:enable-sdcklout11-adly
                          lvl2/enable-sdcklout11-adly)
  (enable-sdclkout1-adly regaddr:enable-sdclkout1-adly
                         lvl2/enable-sdclkout1-adly)
  (enable-sdclkout13-adly regaddr:enable-sdclkout13-adly
                          lvl2/enable-sdclkout13-adly)
  (enable-sdclkout3-adly regaddr:enable-sdclkout3-adly
                         lvl2/enable-sdclkout3-adly)
  (enable-sdclkout5-adly regaddr:enable-sdclkout5-adly
                         lvl2/enable-sdclkout5-adly)
  (enable-sdclkout7-adly regaddr:enable-sdclkout7-adly
                         lvl2/enable-sdclkout7-adly)
  (enable-sdclkout9-adly regaddr:enable-sdclkout9-adly
                         lvl2/enable-sdclkout9-adly)
  (enable-spi-three-wire regaddr:enable-spi-three-wire
                         lvl2/enable-spi-three-wire)
  (enable-sync regaddr:enable-sync
               lvl2/enable-sync)
  (enable-sync-1shot regaddr:enable-sync-1shot
                     lvl2/enable-sync-1shot)
  (enable-track regaddr:enable-track
                lvl2/enable-track)
  (enable-holdover-force regaddr:holdover-force
                         lvl2/enable-holdover-force)
  (enable-holdover-hitless-switch regaddr:holdover-hitless-switch
                                  lvl2/enable-holdover-hitless-switch)
  (enable-holdover-los-detect regaddr:holdover-los-detect
                              lvl2/enable-holdover-los-detect)
  (enable-holdover-pll1-detect regaddr:holdover-pll1-detect
                               lvl2/enable-holdover-pll1-detect)
  (enable-holdover-vtune-detect regaddr:holdover-vtune-detect
                                lvl2/enable-holdover-vtune-detect)
  (enable-oscin-powerdown regaddr:oscin-powerdown
                          lvl2/enable-oscin-powerdown)
  (enable-pll1-cp-tri-state regaddr:pll1-cp-tri-state
                            lvl2/enable-pll1-cp-tri-state)
  (enable-pll1-powerdown regaddr:pll1-powerdown
                         lvl2/enable-pll1-powerdown)
  (enable-pll2-cp-tri-state regaddr:pll2-cp-tri-state
                            lvl2/enable-pll2-cp-tri-state
                            postproc:x169)
  (enable-pll2-powerdown regaddr:pll2-powerdown
                         lvl2/enable-pll2-powerdown)
  (enable-pll2-prescaler-powerdown regaddr:pll2-prescaler-powerdown
                                   lvl2/enable-pll2-prescaler-powerdown)
  (enable-powerdown regaddr:powerdown
                    lvl2/enable-powerdown)
  (enable-reset regaddr:reset
                lvl2/enable-reset)
  (enable-sdclkout1-hs regaddr:sdclkout1-hs
                       lvl2/enable-sdclkout1-hs)
  (enable-sdclkout1-powerdown regaddr:sdclkout1-powerdown
                              lvl2/enable-sdclkout1-powerdown)
  (enable-sdclkout11-hs regaddr:sdclkout11-hs
                        lvl2/enable-sdclkout11-hs)
  (enable-sdclkout11-powerdown regaddr:sdclkout11-powerdown
                               lvl2/enable-sdclkout11-powerdown)
  (enable-sdclkout13-hs regaddr:sdclkout13-hs
                        lvl2/enable-sdclkout13-hs)
  (enable-sdclkout13-powerdown regaddr:sdclkout13-powerdown
                               lvl2/enable-sdclkout13-powerdown)
  (enable-sdclkout3-hs regaddr:sdclkout3-hs
                       lvl2/enable-sdclkout3-hs)
  (enable-sdclkout3-powerdown regaddr:sdclkout3-powerdown
                              lvl2/enable-sdclkout3-powerdown)
  (enable-sdclkout5-hs regaddr:sdclkout5-hs
                       lvl2/enable-sdclkout5-hs)
  (enable-sdclkout5-powerdown regaddr:sdclkout5-powerdown
                              lvl2/enable-sdclkout5-powerdown)
  (enable-sdclkout7-hs regaddr:sdclkout7-hs
                       lvl2/enable-sdclkout7-hs)
  (enable-sdclkout7-powerdown regaddr:sdclkout7-powerdown
                              lvl2/enable-sdclkout7-powerdown)
  (enable-sdclkout9-hs regaddr:sdclkout9-hs
                       lvl2/enable-sdclkout9-hs)
  (enable-sdclkout9-powerdown regaddr:sdclkout9-powerdown
                              lvl2/enable-sdclkout9-powerdown)
  (enable-sync-pll1-dld regaddr:sync-pll1-dld
                        lvl2/enable-sync-pll1-dld)
  (enable-sync-pll2-dld regaddr:sync-pll2-dld
                        lvl2/enable-sync-pll2-dld)
  (enable-sysref-ddly-clr regaddr:sysref-ddly-clr
                          lvl2/enable-sysref-ddly-clr)
  (enable-sysref-ddly-powerdown regaddr:sysref-ddly-powerdown
                                lvl2/enable-sysref-ddly-powerdown)
  (enable-sysref-global-powerdown regaddr:sysref-global-powerdown
                                  lvl2/enable-sysref-global-powerdown)
  (enable-sysref-plsr-powerdown regaddr:sysref-plsr-powerdown
                                lvl2/enable-sysref-plsr-powerdown)
  (enable-sysref-powerdown regaddr:sysref-powerdown
                           lvl2/enable-sysref-powerdown)
  (enable-vco-ldo-powerdown regaddr:vco-ldo-powerdown
                            lvl2/enable-vco-ldo-powerdown)
  (enable-vco-powerdown regaddr:vco-powerdown
                        lvl2/enable-vco-powerdown)
  (set-clkin-0-type regaddr:clkin-0-type
                    lvl2/set-clkin-0-type (value))
  (set-clkin-1-type regaddr:clkin-1-type
                    lvl2/set-clkin-1-type (value))
  (set-clkin-2-type regaddr:clkin-2-type
                    lvl2/set-clkin-2-type (value))
  (set-clkin-sel-mode regaddr:clkin-sel-mode
                      lvl2/set-clkin-sel-mode (value))
  (set-clkin-sel-polarity regaddr:clkin-sel-polarity
                          lvl2/set-clkin-sel-polarity (value))
  (set-clkin-sel0-mux regaddr:clkin-sel0-mux
                      lvl2/set-clkin-sel0-mux (value))
  (set-clkin-sel0-type regaddr:clkin-sel0-type
                       lvl2/set-clkin-sel0-type (value))
  (set-clkin-sel1-mux regaddr:clkin-sel1-mux
                      lvl2/set-clkin-sel1-mux (value))
  (set-clkin-sel1-type regaddr:clkin-sel1-type
                       lvl2/set-clkin-sel1-type (value))
  (set-clkin0-out-mux regaddr:clkin0-out-mux
                      lvl2/set-clkin0-out-mux (value))
  (set-clkin1-out-mux regaddr:clkin1-out-mux
                      lvl2/set-clkin1-out-mux (value))
  (set-clkout0-format regaddr:clkout0-format
                      lvl2/set-clkout0-format (value))
  (set-clkout1-format regaddr:clkout1-format
                      lvl2/set-clkout1-format (value))
  (set-clkout10-format regaddr:clkout10-format
                       lvl2/set-clkout10-format (value))
  (set-clkout11-format regaddr:clkout11-format
                       lvl2/set-clkout11-format (value))
  (set-clkout12-format regaddr:clkout12-format
                       lvl2/set-clkout12-format (value))
  (set-clkout13-format regaddr:clkout13-format
                       lvl2/set-clkout13-format (value))
  (set-clkout2-format regaddr:clkout2-format
                      lvl2/set-clkout2-format (value))
  (set-clkout3-format regaddr:clkout3-format
                      lvl2/set-clkout3-format (value))
  (set-clkout4-format regaddr:clkout4-format
                      lvl2/set-clkout4-format (value))
  (set-clkout5-format regaddr:clkout5-format
                      lvl2/set-clkout5-format (value))
  (set-clkout6-format regaddr:clkout6-format
                      lvl2/set-clkout6-format (value))
  (set-clkout7-format regaddr:clkout7-format
                      lvl2/set-clkout7-format (value))
  (set-clkout8-format regaddr:clkout8-format
                      lvl2/set-clkout8-format (value))
  (set-clkout9-format regaddr:clkout9-format
                      lvl2/set-clkout9-format (value))
  (set-dac-clk-cntr regaddr:dac-clk-cntr
                    lvl2/set-dac-clk-cntr (value))
  (set-dac-clk-mult regaddr:dac-clk-mult
                    lvl2/set-dac-clk-mult (value))
  (set-dac-trip-high regaddr:dac-trip-high
                     lvl2/set-dac-trip-high (value))
  (set-dac-trip-low regaddr:dac-trip-low
                    lvl2/set-dac-trip-low (value))
  (set-dclkout0-adly regaddr:dclkout0-adly
                     lvl2/set-dclkout0-adly (value))
  (set-dclkout0-ddly-cnt-high regaddr:dclkout0-ddly-cnt-high
                              lvl2/set-dclkout0-ddly-cnt-high (value))
  (set-dclkout0-ddly-cnt-low regaddr:dclkout0-ddly-cnt-low
                             lvl2/set-dclkout0-ddly-cnt-low (value))
  (set-dclkout0-div regaddr:dclkout0-div
                    lvl2/set-dclkout0-div (value))
  (set-dclkout0-mux regaddr:dclkout0-mux
                    lvl2/set-dclkout0-mux (value))
  (set-dclkout0-polarity regaddr:dclkout0-polarity
                         lvl2/set-dclkout0-polarity (value))
  (set-dclkout10-adly regaddr:dclkout10-adly
                      lvl2/set-dclkout10-adly (value))
  (set-dclkout10-ddly-cnt-high regaddr:dclkout10-ddly-cnt-high
                               lvl2/set-dclkout10-ddly-cnt-high (value))
  (set-dclkout10-ddly-cnt-low regaddr:dclkout10-ddly-cnt-low
                              lvl2/set-dclkout10-ddly-cnt-low (value))
  (set-dclkout10-div regaddr:dclkout10-div
                     lvl2/set-dclkout10-div (value))
  (set-dclkout10-mux regaddr:dclkout10-mux
                     lvl2/set-dclkout10-mux (value))
  (set-dclkout10-polarity regaddr:dclkout10-polarity
                          lvl2/set-dclkout10-polarity (value))
  (set-dclkout12-adly regaddr:dclkout12-adly
                      lvl2/set-dclkout12-adly (value))
  (set-dclkout12-ddly-cnt-high regaddr:dclkout12-ddly-cnt-high
                               lvl2/set-dclkout12-ddly-cnt-high (value))
  (set-dclkout12-ddly-cnt-low regaddr:dclkout12-ddly-cnt-low
                              lvl2/set-dclkout12-ddly-cnt-low (value))
  (set-dclkout12-div regaddr:dclkout12-div
                     lvl2/set-dclkout12-div (value))
  (set-dclkout12-mux regaddr:dclkout12-mux
                     lvl2/set-dclkout12-mux (value))
  (set-dclkout12-polarity regaddr:dclkout12-polarity
                          lvl2/set-dclkout12-polarity (value))
  (set-dclkout2-adly regaddr:dclkout2-adly
                     lvl2/set-dclkout2-adly (value))
  (set-dclkout2-ddly-cnt-high regaddr:dclkout2-ddly-cnt-high
                              lvl2/set-dclkout2-ddly-cnt-high (value))
  (set-dclkout2-ddly-cnt-low regaddr:dclkout2-ddly-cnt-low
                             lvl2/set-dclkout2-ddly-cnt-low (value))
  (set-dclkout2-div regaddr:dclkout2-div
                    lvl2/set-dclkout2-div (value))
  (set-dclkout2-mux regaddr:dclkout2-mux
                    lvl2/set-dclkout2-mux (value))
  (set-dclkout2-polarity regaddr:dclkout2-polarity
                         lvl2/set-dclkout2-polarity (value))
  (set-dclkout4-adly regaddr:dclkout4-adly
                     lvl2/set-dclkout4-adly (value))
  (set-dclkout4-ddly-cnt-high regaddr:dclkout4-ddly-cnt-high
                              lvl2/set-dclkout4-ddly-cnt-high (value))
  (set-dclkout4-ddly-cnt-low regaddr:dclkout4-ddly-cnt-low
                             lvl2/set-dclkout4-ddly-cnt-low (value))
  (set-dclkout4-div regaddr:dclkout4-div
                    lvl2/set-dclkout4-div (value))
  (set-dclkout4-mux regaddr:dclkout4-mux
                    lvl2/set-dclkout4-mux (value))
  (set-dclkout4-polarity regaddr:dclkout4-polarity
                         lvl2/set-dclkout4-polarity (value))
  (set-dclkout6-adly regaddr:dclkout6-adly
                     lvl2/set-dclkout6-adly (value))
  (set-dclkout6-ddly-cnt-high regaddr:dclkout6-ddly-cnt-high
                              lvl2/set-dclkout6-ddly-cnt-high (value))
  (set-dclkout6-ddly-cnt-low regaddr:dclkout6-ddly-cnt-low
                             lvl2/set-dclkout6-ddly-cnt-low (value))
  (set-dclkout6-div regaddr:dclkout6-div
                    lvl2/set-dclkout6-div (value))
  (set-dclkout6-mux regaddr:dclkout6-mux
                    lvl2/set-dclkout6-mux (value))
  (set-dclkout6-polarity regaddr:dclkout6-polarity
                         lvl2/set-dclkout6-polarity (value))
  (set-dclkout8-adly regaddr:dclkout8-adly
                     lvl2/set-dclkout8-adly (value))
  (set-dclkout8-ddly-cnt-high regaddr:dclkout8-ddly-cnt-high
                              lvl2/set-dclkout8-ddly-cnt-high (value))
  (set-dclkout8-ddly-cnt-low regaddr:dclkout8-ddly-cnt-low
                             lvl2/set-dclkout8-ddly-cnt-low (value))
  (set-dclkout8-div regaddr:dclkout8-div
                    lvl2/set-dclkout8-div (value))
  (set-dclkout8-mux regaddr:dclkout8-mux
                    lvl2/set-dclkout8-mux (value))
  (set-dclkout8-polarity regaddr:dclkout8-polarity
                         lvl2/set-dclkout8-polarity (value))
  (set-ddlyd-step-cnt regaddr:ddlyd-step-cnt
                      lvl2/set-ddlyd-step-cnt (value))
  (set-enable-sysref-req regaddr:enable-sysref-req
                         lvl2/set-enable-sysref-req (value))
  (set-fb-mux regaddr:fb-mux
              lvl2/set-fb-mux (value))
  (set-fixed-reg:0145 regaddr:fixed-reg:0145
                      lvl2/set-fixed-reg:0145 (value)
                      postproc:x145)
  (set-los-timeout regaddr:los-timeout
                   lvl2/set-los-timeout (value))
  (set-opt-reg-1 regaddr:opt-reg-1
                 lvl2/set-opt-reg-1 (value)
                 postproc:x17c)
  (set-opt-reg-2 regaddr:opt-reg-2
                 lvl2/set-opt-reg-2 (value)
                 postproc:x17d)
  (set-oscin-freq regaddr:oscin-freq
                  lvl2/set-oscin-freq (value))
  (set-oscout-format regaddr:oscout-format
                     lvl2/set-oscout-format (value))
  (set-oscout-mux regaddr:oscout-mux
                  lvl2/set-oscout-mux (value))
  (set-pll1-cp-gain regaddr:pll1-cp-gain
                    lvl2/set-pll1-cp-gain (value))
  (set-pll1-cp-polarity regaddr:pll1-cp-polarity
                        lvl2/set-pll1-cp-polarity (value))
  (set-pll1-ld-mux regaddr:pll1-ld-mux
                   lvl2/set-pll1-ld-mux (value))
  (set-pll1-ld-type regaddr:pll1-ld-type
                    lvl2/set-pll1-ld-type (value))
  (set-pll1-n-divider-dly regaddr:pll1-n-divider-dly
                          lvl2/set-pll1-n-divider-dly (value))
  (set-pll1-nclk-mux regaddr:pll1-nclk-mux
                     lvl2/set-pll1-nclk-mux (value))
  (set-pll1-r-divider-dly regaddr:pll1-r-divider-dly
                          lvl2/set-pll1-r-divider-dly (value))
  (set-pll1-window-size regaddr:pll1-window-size
                        lvl2/set-pll1-window-size (value))
  (set-pll2-cp-gain regaddr:pll2-cp-gain
                    lvl2/set-pll2-cp-gain (value)
                    postproc:x169)
  (set-pll2-cp-polarity regaddr:pll2-cp-polarity
                        lvl2/set-pll2-cp-polarity (value)
                        postproc:x169)
  (set-pll2-ld-mux regaddr:pll2-ld-mux
                   lvl2/set-pll2-ld-mux (value))
  (set-pll2-ld-type regaddr:pll2-ld-type
                    lvl2/set-pll2-ld-type (value))
  (set-pll2-loopfilter-c3 regaddr:pll2-loopfilter-c3
                          lvl2/set-pll2-loopfilter-c3 (value))
  (set-pll2-loopfilter-c4 regaddr:pll2-loopfilter-c4
                          lvl2/set-pll2-loopfilter-c4 (value))
  (set-pll2-loopfilter-r3 regaddr:pll2-loopfilter-r3
                          lvl2/set-pll2-loopfilter-r3 (value))
  (set-pll2-loopfilter-r4 regaddr:pll2-loopfilter-r4
                          lvl2/set-pll2-loopfilter-r4 (value))
  (set-pll2-nclk-mux regaddr:pll2-nclk-mux
                     lvl2/set-pll2-nclk-mux (value))
  (set-pll2-prescaler regaddr:pll2-prescaler
                      lvl2/set-pll2-prescaler (value))
  (set-pll2-window-size regaddr:pll2-window-size
                        lvl2/set-pll2-window-size (value)
                        postproc:x169)
  (set-reset-mux regaddr:reset-mux
                 lvl2/set-reset-mux (value))
  (set-reset-type regaddr:reset-type
                  lvl2/set-reset-type (value))
  (set-sdclkout1-adly regaddr:sdclkout1-adly
                      lvl2/set-sdclkout1-adly (value))
  (set-sdclkout1-ddly regaddr:sdclkout1-ddly
                      lvl2/set-sdclkout1-ddly (value))
  (set-sdclkout1-disable-mode regaddr:sdclkout1-disable-mode
                              lvl2/set-sdclkout1-disable-mode (value))
  (set-sdclkout1-mux regaddr:sdclkout1-mux
                     lvl2/set-sdclkout1-mux (value))
  (set-sdclkout1-polarity regaddr:sdclkout1-polarity
                          lvl2/set-sdclkout1-polarity (value))
  (set-sdclkout11-adly regaddr:sdclkout11-adly
                       lvl2/set-sdclkout11-adly (value))
  (set-sdclkout11-ddly regaddr:sdclkout11-ddly
                       lvl2/set-sdclkout11-ddly (value))
  (set-sdclkout11-disable-mode regaddr:sdclkout11-disable-mode
                               lvl2/set-sdclkout11-disable-mode (value))
  (set-sdclkout11-mux regaddr:sdclkout11-mux
                      lvl2/set-sdclkout11-mux (value))
  (set-sdclkout11-polarity regaddr:sdclkout11-polarity
                           lvl2/set-sdclkout11-polarity (value))
  (set-sdclkout13-adly regaddr:sdclkout13-adly
                       lvl2/set-sdclkout13-adly (value))
  (set-sdclkout13-ddly regaddr:sdclkout13-ddly
                       lvl2/set-sdclkout13-ddly (value))
  (set-sdclkout13-disable-mode regaddr:sdclkout13-disable-mode
                               lvl2/set-sdclkout13-disable-mode (value))
  (set-sdclkout13-mux regaddr:sdclkout13-mux
                      lvl2/set-sdclkout13-mux (value))
  (set-sdclkout13-polarity regaddr:sdclkout13-polarity
                           lvl2/set-sdclkout13-polarity (value))
  (set-sdclkout3-adly regaddr:sdclkout3-adly
                      lvl2/set-sdclkout3-adly (value))
  (set-sdclkout3-ddly regaddr:sdclkout3-ddly
                      lvl2/set-sdclkout3-ddly (value))
  (set-sdclkout3-disable-mode regaddr:sdclkout3-disable-mode
                              lvl2/set-sdclkout3-disable-mode (value))
  (set-sdclkout3-mux regaddr:sdclkout3-mux
                     lvl2/set-sdclkout3-mux (value))
  (set-sdclkout3-polarity regaddr:sdclkout3-polarity
                          lvl2/set-sdclkout3-polarity (value))
  (set-sdclkout5-adly regaddr:sdclkout5-adly
                      lvl2/set-sdclkout5-adly (value))
  (set-sdclkout5-ddly regaddr:sdclkout5-ddly
                      lvl2/set-sdclkout5-ddly (value))
  (set-sdclkout5-disable-mode regaddr:sdclkout5-disable-mode
                              lvl2/set-sdclkout5-disable-mode (value))
  (set-sdclkout5-mux regaddr:sdclkout5-mux
                     lvl2/set-sdclkout5-mux (value))
  (set-sdclkout5-polarity regaddr:sdclkout5-polarity
                          lvl2/set-sdclkout5-polarity (value))
  (set-sdclkout7-adly regaddr:sdclkout7-adly
                      lvl2/set-sdclkout7-adly (value))
  (set-sdclkout7-ddly regaddr:sdclkout7-ddly
                      lvl2/set-sdclkout7-ddly (value))
  (set-sdclkout7-disable-mode regaddr:sdclkout7-disable-mode
                              lvl2/set-sdclkout7-disable-mode (value))
  (set-sdclkout7-mux regaddr:sdclkout7-mux
                     lvl2/set-sdclkout7-mux (value))
  (set-sdclkout7-polarity regaddr:sdclkout7-polarity
                          lvl2/set-sdclkout7-polarity (value))
  (set-sdclkout9-adly regaddr:sdclkout9-adly
                      lvl2/set-sdclkout9-adly (value))
  (set-sdclkout9-ddly regaddr:sdclkout9-ddly
                      lvl2/set-sdclkout9-ddly (value))
  (set-sdclkout9-disable-mode regaddr:sdclkout9-disable-mode
                              lvl2/set-sdclkout9-disable-mode (value))
  (set-sdclkout9-mux regaddr:sdclkout9-mux
                     lvl2/set-sdclkout9-mux (value))
  (set-sdclkout9-polarity regaddr:sdclkout9-polarity
                          lvl2/set-sdclkout9-polarity (value))
  (set-sdio-readback-type regaddr:sdio-readback-type
                          lvl2/set-sdio-readback-type (value))
  (enable-spi-lock-high WARNING-no-such-address
                        lvl2/enable-spi-lock-high)
  (disable-spi-lock-high WARNING-no-such-address
                         lvl2/disable-spi-lock-high)
  (enable-spi-lock-mid WARNING-no-such-address
                       lvl2/enable-spi-lock-mid)
  (disable-spi-lock-mid WARNING-no-such-address
                        lvl2/disable-spi-lock-mid)
  (enable-spi-lock-low WARNING-no-such-address
                       lvl2/enable-spi-lock-low)
  (disable-spi-lock-low WARNING-no-such-address
                        lvl2/disable-spi-lock-low)
  (set-sync-mode regaddr:sync-mode
                 lvl2/set-sync-mode (value))
  (set-sync-polarity regaddr:sync-polarity
                     lvl2/set-sync-polarity (value))
  (set-sysref-mux regaddr:sysref-mux
                  lvl2/set-sysref-mux (value))
  (set-sysref-pulse-cnt regaddr:sysref-pulse-cnt
                        lvl2/set-sysref-pulse-cnt (value))
  (set-vco-mux regaddr:vco-mux
               lvl2/set-vco-mux (value)))

(define-public (set-sysref-divider conn value)
  ;; 0..7 are not allowed.
  (with-constraints (value (>= 8) (<= (one-bits 13)))
    (let ((data (split-word value 5 8)))
      (replay-register conn regaddr:sysref-div-high
                       lvl2/set-sysref-div-high
                       ((list-ref data 0)))
      (replay-register conn regaddr:sysref-div-low
                       lvl2/set-sysref-div-low
                       ((list-ref data 1))))))

(define-public (set-sysref-digital-delay conn value)
  ;; Again, 0..7 are not allowed.
  (with-constraints (value (>= 8) (<= (one-bits 13)))
    (let ((data (split-word value 5 8)))
      (replay-register conn regaddr:sysref-ddly-high
                       lvl2/set-sysref-ddly-high
                       ((list-ref data 0)))
      (replay-register conn regaddr:sysref-ddly-low
                       lvl2/set-sysref-ddly-low
                       ((list-ref data 1))))))

(define-public (set-manual-dac conn value)
  (with-constraints (value (>= 0) (<= (one-bits 10)))
    (let ((data (split-word value 2 8)))
      (replay-register conn regaddr:manual-dac-high
                       lvl2/set-manual-dac-high
                       ((list-ref data 0)))
      (replay-register conn regaddr:manual-dac-low
                       lvl2/set-manual-dac-low
                       ((list-ref data 1))))))

(define-public (set-holdover-dld-cnt conn value)
  (with-constraints (value (>= 0) (<= (one-bits 14)))
    (let ((data (split-word value 6 8)))
      (replay-register conn regaddr:holdover-dld-cnt-high
                       lvl2/set-holdover-dld-cnt-high
                       ((list-ref data 0)))
      (replay-register conn regaddr:holdover-dld-cnt-low
                       lvl2/set-holdover-dld-cnt-low
                       ((list-ref data 1))))))

(define-public (set-pll2-r-divider conn value)
  ;; value := 0 is not allowed
  (with-constraints (value (>= 1) (<= (one-bits 12)))
    (let ((data (split-word value 4 8)))
      (replay-register conn regaddr:pll2-r-divider-high
                       lvl2/set-pll2-r-divider-high
                       ((list-ref data 0)))
      (replay-register conn regaddr:pll2-r-divider-low
                       lvl2/set-pll2-r-divider-low
                       ((list-ref data 1))))))

(define-public (set-pll2-n-divider conn value)
  ;; Again, value := 0 is not allowed
  (with-constraints (value (>= 1) (<= (one-bits 18)))
    (let ((data (split-word value 2 8 8)))
      (replay-register conn regaddr:pll2-n-divider-high
                       lvl2/set-pll2-n-divider-high
                       ((list-ref data 0)))
      (replay-register conn regaddr:pll2-n-divider-mid
                       lvl2/set-pll2-n-divider-mid
                       ((list-ref data 1)))
      (replay-register conn regaddr:pll2-n-divider-low
                       lvl2/set-pll2-n-divider-low
                       ((list-ref data 2))))))

(define-public (set-pll2-n-calibration-phase conn value)
  ;; Again, value := 0 is not allowed
  (with-constraints (value (>= 1) (<= (one-bits 18)))
    (let ((data (split-word value 2 8 8)))
      (replay-register conn regaddr:pll2-n-calibration-phase-high
                       lvl2/set-pll2-n-calibration-phase-high
                       ((list-ref data 0)))
      (replay-register conn regaddr:pll2-n-calibration-phase-mid
                       lvl2/set-pll2-n-calibration-phase-mid
                       ((list-ref data 1)))
      (replay-register conn regaddr:pll2-n-calibration-phase-low
                       lvl2/set-pll2-n-calibration-phase-low
                       ((list-ref data 2))))))

(define-public (set-pll2-dld-cnt conn value)
  ;; Again, value := 0 is not allowed
  (with-constraints (value (>= 1) (<= 14))
    (let ((data (split-word value 6 8)))
      (replay-register conn regaddr:pll2-dld-cnt-high
                       lvl2/set-pll2-dld-cnt-high
                       ((list-ref data 0)))
      (replay-register conn regaddr:pll2-dld-cnt-low
                       lvl2/set-pll2-dld-cnt-low
                       ((list-ref data 1))))))

(define-public (set-pll1-dld-cnt conn value)
  ;; Again, value := 0 is not allowed
  (with-constraints (value (>= 1) (<= 14))
    (let ((data (split-word value 6 8)))
      (replay-register conn regaddr:pll1-dld-cnt-high
                       lvl2/set-pll1-dld-cnt-high
                       ((list-ref data 0)))
      (replay-register conn regaddr:pll1-dld-cnt-low
                       lvl2/set-pll1-dld-cnt-low
                       ((list-ref data 1))))))

(define-public (set-pll1-n-divider conn value)
  ;; Again, value := 0 is not allowed
  (with-constraints (value (>= 1) (<= 14))
    (let ((data (split-word value 6 8)))
      (replay-register conn regaddr:pll1-n-divider-high
                       lvl2/set-pll1-n-divider-high
                       ((list-ref data 0)))
      (replay-register conn regaddr:pll1-n-divider-low
                       lvl2/set-pll1-n-divider-low
                       ((list-ref data 1))))))

(define-public (set-clkin2-r-divider conn value)
  ;; Again, value := 0 is not allowed
  (with-constraints (value (>= 1) (<= 14))
    (let ((data (split-word value 6 8)))
      (replay-register conn regaddr:clkin2-r-divider-high
                       lvl2/set-clkin2-r-divider-high
                       ((list-ref data 0)))
      (replay-register conn regaddr:clkin2-r-divider-low
                       lvl2/set-clkin2-r-divider-low
                       ((list-ref data 1))))))

(define-public (set-clkin1-r-divider conn value)
  ;; Again, value := 0 is not allowed
  (with-constraints (value (>= 1) (<= 14))
    (let ((data (split-word value 6 8)))
      (replay-register conn regaddr:clkin1-r-divider-high
                       lvl2/set-clkin1-r-divider-high
                       ((list-ref data 0)))
      (replay-register conn regaddr:clkin1-r-divider-low
                       lvl2/set-clkin1-r-divider-low
                       ((list-ref data 1))))))

(define-public (set-clkin0-r-divider conn value)
  ;; Again, value := 0 is not allowed
  (with-constraints (value (>= 1) (<= 14))
    (let ((data (split-word value 6 8)))
      (replay-register conn regaddr:clkin0-r-divider-high
                       lvl2/set-clkin0-r-divider-high
                       ((list-ref data 0)))
      (replay-register conn regaddr:clkin0-r-divider-low
                       lvl2/set-clkin0-r-divider-low
                       ((list-ref data 1))))))
