#!/usr/bin/perl

# Copyright (c) 2014 chip-remote workers, All rights reserved.
#
# Terms for redistribution and use can be found in LICENCE.

use strict;
use Test::ChipRemote;

my %clk  = ( port => 'A', index => 15, role => "CLK" );
my %addr2  = ( port => 'A', index => 14, role => "ADDR:2" );
my %addr1  = ( port => 'A', index => 13, role => "ADDR:1" );
my %addr0  = ( port => 'A', index => 12, role => "ADDR:0" );
my %data7  = ( port => 'A', index => 11, role => "DATA:7" );
my %data6  = ( port => 'A', index => 10, role => "DATA:6" );
my %data5  = ( port => 'A', index => 9, role => "DATA:5" );
my %data4  = ( port => 'A', index => 8, role => "DATA:4" );
my %data3  = ( port => 'A', index => 7, role => "DATA:3" );
my %data2  = ( port => 'A', index => 6, role => "DATA:2" );
my %data1  = ( port => 'A', index => 5, role => "DATA:1" );
my %data0  = ( port => 'A', index => 4, role => "DATA:0" );

sub clk_is_output { cr_line_trace_reply_direction_write(\%clk) }

sub addr_init {
    my @data;
    foreach my $line (\%addr2, \%addr1, \%addr0) {
        push @data, cr_line_trace_reply_direction_write($line);
        push @data, cr_line_trace_reply_out($line, 0);
    }
    return @data;
}

sub clk_low {
    return cr_line_trace_reply_out(\%clk, 0),
}

sub clk_high {
    return cr_line_trace_reply_out(\%clk, 1),
}

sub data_is_output {
    my @data;
    foreach my $line (\%data7, \%data6, \%data5, \%data4,
                          \%data3, \%data2, \%data1, \%data0) {
        push @data, cr_line_trace_reply_direction_write($line);
    }
    return @data;
}

sub data_is_input {
    my @data;
    foreach my $line (\%data7, \%data6, \%data5, \%data4,
                          \%data3, \%data2, \%data1, \%data0) {
        push @data, cr_line_trace_reply_direction_read($line);
    }
    return @data;
}

sub data_init {
    my @data;
    foreach my $line (\%data7, \%data6, \%data5, \%data4,
                          \%data3, \%data2, \%data1, \%data0) {
        push @data, cr_line_trace_reply_direction_write($line);
        push @data, cr_line_trace_reply_out($line, 0);
    }
    return @data;
}

cr_test_title q{PAR-EX: Single data exchange works properly};

cr_run_script [ cr_request("SET 1 MODE PAR-EX", "OK"),
                cr_request("LINE 1 b CLK", "OK"),
                cr_request("LINE 1 a ADDR:2", "OK"),
                cr_request("LINE 1 9 ADDR:1", "OK"),
                cr_request("LINE 1 8 ADDR:0", "OK"),
                cr_request("LINE 1 7 DATA:7", "OK"),
                cr_request("LINE 1 6 DATA:6", "OK"),
                cr_request("LINE 1 5 DATA:5", "OK"),
                cr_request("LINE 1 4 DATA:4", "OK"),
                cr_request("LINE 1 3 DATA:3", "OK"),
                cr_request("LINE 1 2 DATA:2", "OK"),
                cr_request("LINE 1 1 DATA:1", "OK"),
                cr_request("LINE 1 0 DATA:0", "OK"),
                cr_request_io("INIT 1", "OK",
                          [ clk_is_output,
                            clk_low,
                            addr_init,
                            data_init ] ),
                cr_request("FOCUS 1", "OK"),
                cr_request_io("TRANSMIT 5cf", "55",
                        [ cr_line_trace_reply_out(\%addr2, 1),
                          cr_line_trace_reply_out(\%addr1, 0),
                          cr_line_trace_reply_out(\%addr0, 1),
                          cr_line_trace_reply_out(\%data7, 1),
                          cr_line_trace_reply_out(\%data6, 1),
                          cr_line_trace_reply_out(\%data5, 0),
                          cr_line_trace_reply_out(\%data4, 0),
                          cr_line_trace_reply_out(\%data3, 1),
                          cr_line_trace_reply_out(\%data2, 1),
                          cr_line_trace_reply_out(\%data1, 1),
                          cr_line_trace_reply_out(\%data0, 1),
                          cr_line_trace_wait(5),
                          clk_high,
                          cr_line_trace_wait(5),
                          data_is_input,
                          clk_low,
                          cr_line_trace_wait(5),
                          clk_high,
                          cr_line_trace_reply_in(\%data7, 0),
                          cr_line_trace_reply_in(\%data6, 1),
                          cr_line_trace_reply_in(\%data5, 0),
                          cr_line_trace_reply_in(\%data4, 1),
                          cr_line_trace_reply_in(\%data3, 0),
                          cr_line_trace_reply_in(\%data2, 1),
                          cr_line_trace_reply_in(\%data1, 0),
                          cr_line_trace_reply_in(\%data0, 1),
                          cr_line_trace_wait(5),
                          clk_low,
                          data_is_output,
                        ] ) ];
