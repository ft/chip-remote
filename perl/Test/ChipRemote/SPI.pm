package Test::ChipRemote::SPI;

# Copyright (c) 2014 chip-remote workers, All rights reserved.
# Terms for redistribution and use can be found in LICENCE.

use strict;
use warnings;
use diagnostics;
use English qw{ -no_match_vars };

use Exporter;
use base qw{ Exporter };
use vars qw{ @EXPORT };
@EXPORT = qw{ spi_default_setup
              clk_halfcycle
              cs_setup
              clk_high
              clk_low
              cs_high
              cs_low
              miso_high
              miso_low
              mosi_high
              mosi_low };

use Test::ChipRemote;

# These are hardcoded delay times from remote-fw's ‘spi.c’.
my $cs_setup_time = 2;
my $clk_halfcycle_time = 4;

sub cs_setup {
    return cr_line_trace_wait($cs_setup_time);
}

sub clk_halfcycle {
    return cr_line_trace_wait($clk_halfcycle_time);
}

my %clk  = ( port => 'A', index => 0, role => "CLK" );
my %cs   = ( port => 'A', index => 1, role => "CS:0" );
my %mosi = ( port => 'A', index => 2, role => "MOSI" );
my %miso = ( port => 'A', index => 3, role => "MISO" );

sub spi_default_setup {
    return ( cr_request("SET 0 MODE SPI", "OK"),
             cr_request("SET 0 FRAME-LENGTH 8", "OK"),
             # CS-LINES is currently non-writable in remote-fw!
             #cr_request("SET 0 CS-LINES 1", "OK"),
             cr_request("SET 0 BIT-ORDER MSB-FIRST", "OK"),
             cr_request("SET 0 CLK-PHASE-DELAY TRUE", "OK"),
             cr_request("SET 0 CLK-POLARITY RISING-EDGE", "OK"),
             cr_request("SET 0 CS-POLARITY ACTIVE-LOW", "OK"),
             cr_request("LINE 0 $clk{index} $clk{role}", "OK"),
             cr_request("LINE 0 $cs{index} $cs{role}", "OK"),
             cr_request("LINE 0 $mosi{index} $mosi{role}", "OK"),
             cr_request("LINE 0 $miso{index} $miso{role}", "OK") );
}

sub clk_high {
    return cr_line_trace_reply_out(\%clk, 1),
}

sub clk_low {
    return cr_line_trace_reply_out(\%clk, 0),
}

sub cs_high {
    return cr_line_trace_reply_out(\%cs, 1),
}

sub cs_low {
    return cr_line_trace_reply_out(\%cs, 0),
}

sub mosi_high {
    return cr_line_trace_reply_out(\%mosi, 1),
}

sub mosi_low {
    return cr_line_trace_reply_out(\%mosi, 0),
}

sub miso_high {
    return cr_line_trace_reply_in(\%miso, 1),
}

sub miso_low {
    return cr_line_trace_reply_in(\%miso, 0),
}
