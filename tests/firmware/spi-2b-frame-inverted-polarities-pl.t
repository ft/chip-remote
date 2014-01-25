#!/usr/bin/perl
use strict;
use Test::ChipRemote;
use Test::ChipRemote::SPI;

cr_test_title q{SPI: 2 Bit frame with default settings works};

cr_run_script [ spi_default_setup,
                cr_request("SET 0 FRAME-LENGTH 2", "OK"),
                cr_request("SET 0 CLK-POLARITY IDLE-HIGH", "OK"),
                cr_request("SET 0 CS-POLARITY ACTIVE-HIGH", "OK"),
                cr_request_io("INIT 0", "OK",
                          [ cs_low, clk_high ] ),
                cr_request("FOCUS 0", "OK"),
                cr_request_io("TRANSMIT 2", "1",
                        [ cs_high,
                          cs_setup,
                          # First bit:
                          mosi_high,
                          clk_halfcycle,
                          clk_low,
                          miso_low,
                          clk_halfcycle,
                          clk_high,
                          # Second bit:
                          mosi_low,
                          clk_halfcycle,
                          clk_low,
                          miso_high,
                          clk_halfcycle,
                          clk_high,
                          cs_setup,
                          cs_low ] ) ];
