#!/usr/bin/perl
use strict;
use Test::ChipRemote;

cr_test_title q{SPI port default parameters check out};
cr_run_script [
    cr_request("SET 0 MODE SPI", "OK"),
    cr_request("INIT 0", "OK"),
    cr_request("PORT 0",
               "MODE SPI",
               "LINES 4",
               "RATE DEFAULT FIXED",
               "BIT-ORDER MSB-FIRST",
               "CLK-PHASE-DELAY TRUE",
               "CLK-POLARITY ACTIVE-HIGH",
               "CS-LINES 1 FIXED",
               "CS-POLARITY ACTIVE-HIGH",
               "FRAME-LENGTH 8") ];
