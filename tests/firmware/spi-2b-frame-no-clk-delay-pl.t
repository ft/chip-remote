#!/usr/bin/perl
use strict;
use Test::ChipRemote;
use Test::ChipRemote::SPI;

cr_test_title q{SPI: 2 Bit frame without clk-phase-delay works};

cr_run_script [ spi_default_setup,
                cr_request("SET 0 FRAME-LENGTH 2", "OK"),
                cr_request("SET 0 CLK-PHASE-DELAY FALSE", "OK"),
                cr_request_io("INIT 0", "OK", [ cs_high, clk_low ] ),
                cr_request("FOCUS 0", "OK"),
                # This is similar to the line trace from
                # “spi-2b-frame-defaults-pl.t”, except, that the clock phase
                # delay is turned off, which means, that clk is toggled
                # immediately after mosi is latched.
                cr_request_io("TRANSMIT 2", "1",
                        [ cs_low,
                          cs_setup,
                          # First bit:
                          mosi_high,
                          clk_high,
                          clk_halfcycle,
                          miso_low,
                          clk_halfcycle,
                          clk_low,
                          # Second bit:
                          mosi_low,
                          clk_high,
                          clk_halfcycle,
                          miso_high,
                          clk_halfcycle,
                          clk_low,
                          cs_setup,
                          cs_high ] ) ];
