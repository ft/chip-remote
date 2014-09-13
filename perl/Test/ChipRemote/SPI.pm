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
              spi_extended_setup
              clk_halfcycle
              cs_setup
              clk_high
              clk_low
              clk_is_output
              cs_high
              cs_low
              cs_is_output
              csN_high
              csN_low
              csN_is_output
              miso_high
              miso_low
              miso_is_input
              mosi_high
              mosi_low
              mosi_is_output };

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
my %cs1   = ( port => 'A', index => 4, role => "CS:1" );
my %cs2   = ( port => 'A', index => 5, role => "CS:2" );
my %cs3   = ( port => 'A', index => 6, role => "CS:3" );
my %mosi = ( port => 'A', index => 2, role => "MOSI" );
my %miso = ( port => 'A', index => 3, role => "MISO" );
my @css = ( \%cs, \%cs1, \%cs2, \%cs3 );

sub spi_default_setup {
    return ( cr_request("SET 0 MODE SPI", "OK"),
             cr_request("SET 0 FRAME-LENGTH 8", "OK"),
             cr_request("SET 0 CS-LINES 1", "OK"),
             cr_request("SET 0 BIT-ORDER MSB-FIRST", "OK"),
             cr_request("SET 0 CLK-PHASE-DELAY TRUE", "OK"),
             cr_request("SET 0 CLK-POLARITY RISING-EDGE", "OK"),
             cr_request("SET 0 CS-POLARITY ACTIVE-LOW", "OK"),
             cr_request("LINE 0 $clk{index} $clk{role}", "OK"),
             cr_request("LINE 0 $cs{index} $cs{role}", "OK"),
             cr_request("LINE 0 $mosi{index} $mosi{role}", "OK"),
             cr_request("LINE 0 $miso{index} $miso{role}", "OK") );
}

sub spi_extended_setup {
    return ( cr_request("LINE 0 $cs1{index} $cs1{role}", "OK"),
             cr_request("LINE 0 $cs2{index} $cs2{role}", "OK"),
             cr_request("LINE 0 $cs3{index} $cs3{role}", "OK"),
             cr_request("SET 0 CS-LINES 4", "OK") );
}

sub clk_high {
    return cr_line_trace_reply_out(\%clk, 1),
}

sub clk_low {
    return cr_line_trace_reply_out(\%clk, 0),
}

sub clk_is_output {
    return cr_line_trace_reply_direction_write(\%clk),
}

sub cs_high {
    return cr_line_trace_reply_out(\%cs, 1),
}

sub cs_low {
    return cr_line_trace_reply_out(\%cs, 0),
}

sub cs_is_output {
    return cr_line_trace_reply_direction_write(\%cs),
}

sub csN_high {
    my ($n) = @_;
    return cr_line_trace_reply_out($css[$n], 1);
}

sub csN_low {
    my ($n) = @_;
    return cr_line_trace_reply_out($css[$n], 0);
}

sub csN_is_output {
    my ($n) = @_;
    return cr_line_trace_reply_direction_write($css[$n]);
}

sub mosi_high {
    return cr_line_trace_reply_out(\%mosi, 1),
}

sub mosi_low {
    return cr_line_trace_reply_out(\%mosi, 0),
}

sub mosi_is_output {
    return cr_line_trace_reply_direction_write(\%mosi),
}

sub miso_high {
    return cr_line_trace_reply_in(\%miso, 1),
}

sub miso_low {
    return cr_line_trace_reply_in(\%miso, 0),
}

sub miso_is_input {
    return cr_line_trace_reply_direction_read(\%miso),
}
