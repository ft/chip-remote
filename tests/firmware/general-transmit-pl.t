#!/usr/bin/perl
use strict;
use Test::ChipRemote;

my %clk  = ( port => 'A', index => 0, role => "CLK" );
my %cs   = ( port => 'A', index => 1, role => "CS:0" );
my %mosi = ( port => 'A', index => 2, role => "MOSI" );
my %miso = ( port => 'A', index => 3, role => "MISO" );

sub setup_cs {
    cr_line_trace_wait(2);
}

sub clk_halfcycle {
    cr_line_trace_wait(4);
}

cr_test_title q{Transmitting words works generally};

cr_run_script [ cr_request("SET 0 MODE SPI", "OK"),
                # FRAME-LENGTH is set to 2. BIT-ORDER is MSB-FIRST, so
                # transmitting "2" is the bit-stream "10". So, MOSI is first
                # high, then low.
                cr_request("SET 0 FRAME-LENGTH 2", "OK"),
                cr_request("SET 0 BIT-ORDER MSB-FIRST", "OK"),
                # CLK-PHASE-DELAY being active means, that MOSI is set, then
                # the firmware waits half a CLK cycle and then toggles CLK.
                cr_request("SET 0 CLK-PHASE-DELAY TRUE", "OK"),
                # CLK-POLARITY determines two things: IDLE-LOW means that the
                # pin is low while idling, which means that its rising edge
                # latches data.
                cr_request("SET 0 CLK-POLARITY IDLE-LOW", "OK"),
                cr_request("LINE 0 $clk{index} $clk{role}", "OK"),
                cr_request("LINE 0 $cs{index} $cs{role}", "OK"),
                cr_request("LINE 0 $mosi{index} $mosi{role}", "OK"),
                cr_request("LINE 0 $miso{index} $miso{role}", "OK"),
                cr_request("INIT 0", "OK"),
                cr_request("FOCUS 0", "OK"),
                # The simulator version of the firmware currently alternates
                # between 0 and one, when reading from pins. So, the returned
                # bit-stream will be "01", thus resulting in "1" as the
                # firmware's reply.
                cr_transmit("2", "1",
                        [ cr_line_trace_reply_out(\%cs, 0),
                          setup_cs,
                          # First bit:
                          cr_line_trace_reply_out(\%mosi, 1),
                          clk_halfcycle,
                          cr_line_trace_reply_out(\%clk, 1),
                          cr_line_trace_reply_in(\%miso, 0),
                          clk_halfcycle,
                          cr_line_trace_reply_out(\%clk, 0),
                          # Second bit:
                          cr_line_trace_reply_out(\%mosi, 0),
                          clk_halfcycle,
                          cr_line_trace_reply_out(\%clk, 1),
                          cr_line_trace_reply_in(\%miso, 1),
                          clk_halfcycle,
                          cr_line_trace_reply_out(\%clk, 0),
                          setup_cs,
                          cr_line_trace_reply_out(\%cs, 1) ] ) ];
