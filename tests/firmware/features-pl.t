#!/usr/bin/perl
use strict;
use Test::ChipRemote;

cr_test_title q{FEATURES request works};
cr_run_script [
    cr_request(qw{ FEATURES
                   FOCUS
                   LINES
                   MODES
                   PORT
                   PORTS
                   SET } ) ];