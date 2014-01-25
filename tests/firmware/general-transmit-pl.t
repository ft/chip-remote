#!/usr/bin/perl
use strict;
use Test::ChipRemote;
use Test::ChipRemote::SPI;

cr_test_title q{Transmitting words works generally};

cr_run_script [ spi_default_setup,
                cr_request("SET 0 FRAME-LENGTH 2", "OK"),
                cr_request("INIT 0", "OK"),
                cr_request("FOCUS 0", "OK"),
                # FRAME-LENGTH is set to 2. BIT-ORDER is MSB-FIRST, so
                # transmitting "2" is the bit-stream "10". So, MOSI is first
                # high, then low.
                #
                # CLK-PHASE-DELAY being active means, that MOSI is set, then
                # the firmware waits half a CLK cycle and then toggles CLK.
                #
                # CLK-POLARITY determines two things: IDLE-LOW means that the
                # pin is low while idling, which means that its rising edge
                # latches data.
                #
                # The simulator version of the firmware currently alternates
                # between 0 and one, when reading from pins. So, the returned
                # bit-stream will be "01", thus resulting in "1" as the
                # firmware's reply.
                cr_transmit("2", "1",
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
