#!/usr/bin/perl

# Copyright (c) 2011-2018 chip-remote workers, All rights reserved.
#
# Terms for redistribution and use can be found in LICENCE.

use strict;
use Test::ChipRemote;
use Test::ChipRemote::SPI;

cr_test_title q{SPI: address changes work};

sub tohex {
    my ($n) = @_;
    return sprintf '%x', $n;
}

sub datatransfer {
    return ( cs_setup(),
             mosi_high(),
             clk_halfcycle(),
             clk_high(),
             miso_low(),
             clk_halfcycle(),
             clk_low(),
             mosi_low(),
             clk_halfcycle(),
             clk_high(),
             miso_high(),
             clk_halfcycle(),
             clk_low(),
             cs_setup() );
}

cr_run_script [ spi_default_setup,
                spi_extended_setup,
                cr_request("SET 0 FRAME-LENGTH 2", "OK"),
                cr_request_io("INIT 0", "OK",
                          [ cs_is_output, cs_high,
                            csN_is_output(1), csN_high(1),
                            csN_is_output(2), csN_high(2),
                            csN_is_output(3), csN_high(3),
                            clk_is_output, clk_low,
                            mosi_is_output, miso_is_input ] ),
                cr_request("FOCUS 0", "OK"),
                # Default address is 1, so only CS:0 is active
                cr_request_io("TRANSMIT 2", "1",
                        [ cs_low,
                          datatransfer,
                          cs_high ] ),
                # Let's dial to CS:2
                cr_request("ADDRESS " . tohex(1<<2), "OK"),
                cr_request_io("TRANSMIT 2", "1",
                        [ csN_low(2),
                          datatransfer,
                          csN_high(2) ] ),
                # Now dial to CS:1 and CS:3
                cr_request("ADDRESS " . tohex((1<<3) | (1<<1)), "OK"),
                cr_request_io("TRANSMIT 2", "1",
                        [ csN_low(1),
                          csN_low(3),
                          datatransfer,
                          csN_high(1),
                          csN_high(3) ] ),
                # No address line at all is possible too, if that is so
                # desired:
                cr_request("ADDRESS 0", "OK"),
                cr_request_io("TRANSMIT 2", "1", [ datatransfer ] ) ];
