#!/usr/bin/perl

# Copyright (c) 2011-2021 chip-remote workers, All rights reserved.
#
# Terms for redistribution and use can be found in LICENCE.

use strict;
use Test::ChipRemote;
use Test::ChipRemote::SPI;

cr_test_title q{SPI port default parameters check out};
cr_run_script [
    cr_request("SET 0 MODE SPI", "OK"),
    cr_request("SET 0 CS-LINES 1", "OK"),
    cr_request("LINE 0 0 CLK", "OK"),
    cr_request("LINE 0 1 CS:0", "OK"),
    cr_request("LINE 0 2 MOSI", "OK"),
    cr_request("LINE 0 3 MISO", "OK"),
    cr_request_io("INIT 0", "OK", [ cs_is_output, cs_high,
                                    clk_is_output, clk_low,
                                    mosi_is_output, miso_is_input ] ),
    cr_request("PORT 0",
               "MODE SPI",
               "LINES 7",
               "RATE 0 FIXED",
               "BIT-ORDER MSB-FIRST",
               "CLK-PHASE-DELAY TRUE",
               "CLK-POLARITY RISING-EDGE",
               "CS-LINES 1",
               "CS-POLARITY ACTIVE-LOW",
               "FRAME-LENGTH 8") ];
