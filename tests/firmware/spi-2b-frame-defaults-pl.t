#!/usr/bin/perl

# Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
#
# Terms for redistribution and use can be found in LICENCE.

use strict;
use Test::ChipRemote;
use Test::ChipRemote::SPI;

cr_test_title q{SPI: 2 Bit frame with default settings works};

cr_run_script [ spi_default_setup,
                cr_request("SET 0 FRAME-LENGTH 2", "OK"),
                cr_request_io("INIT 0", "OK",
                          [ cs_high, clk_low ] ),
                cr_request("FOCUS 0", "OK"),
                # FRAME-LENGTH is set to 2. BIT-ORDER is MSB-FIRST, so
                # transmitting "2" is the bit-stream "10". So, MOSI is first
                # high, then low.
                #
                # CLK-PHASE-DELAY being active means, that MOSI is set, then
                # the firmware waits half a CLK cycle and then toggles CLK.
                #
                # CLK-POLARITY determines which of the two edges (rising or
                # falling) latches data.
                #
                # The simulator version of the firmware currently alternates
                # between 0 and one, when reading from pins. So, the returned
                # bit-stream will be "01", thus resulting in "1" as the
                # firmware's reply.
                cr_request_io("TRANSMIT 2", "1",
                        [ cs_low,
                          cs_setup,
                          # First bit:
                          mosi_high,
                          clk_halfcycle,
                          clk_high,
                          miso_low,
                          clk_halfcycle,
                          clk_low,
                          # Second bit:
                          mosi_low,
                          clk_halfcycle,
                          clk_high,
                          miso_high,
                          clk_halfcycle,
                          clk_low,
                          cs_setup,
                          cs_high ] ) ];
