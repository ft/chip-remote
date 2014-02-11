#!/usr/bin/perl

# Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
#
# Terms for redistribution and use can be found in LICENCE.

use strict;
use Test::ChipRemote;
use Test::ChipRemote::SPI;

cr_test_title q{SPI: 2 Bit frame with lsb-first works};

cr_run_script [ spi_default_setup,
                cr_request("SET 0 FRAME-LENGTH 2", "OK"),
                cr_request("SET 0 BIT-ORDER LSB-FIRST", "OK"),
                cr_request_io("INIT 0", "OK",
                          [ cs_high, clk_low ] ),
                cr_request("FOCUS 0", "OK"),
                # With LSB-FIRST data is send AND received in that mode, so the
                # send-bit-stream is "01" and the receive-bit-stream is "10"
                # (which translates to a hex string of "2").
                cr_request_io("TRANSMIT 2", "2",
                        [ cs_low,
                          cs_setup,
                          # First bit:
                          mosi_low,
                          clk_halfcycle,
                          clk_high,
                          miso_low,
                          clk_halfcycle,
                          clk_low,
                          # Second bit:
                          mosi_high,
                          clk_halfcycle,
                          clk_high,
                          miso_high,
                          clk_halfcycle,
                          clk_low,
                          cs_setup,
                          cs_high ] ) ];
