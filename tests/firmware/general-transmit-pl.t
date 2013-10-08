#!/usr/bin/perl
use strict;
use Test::ChipRemote;

cr_test_title q{Transmitting words works generally},
              q{TODO SPI internals are not complete yet};
cr_run_script [ cr_request("SET 0 MODE SPI", "OK"),
                cr_request("INIT 0", "OK"),
                cr_request("FOCUS 0", "OK"),
                cr_request("TRANSMIT beefaffe", "0") ];
