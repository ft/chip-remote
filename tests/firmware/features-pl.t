#!/usr/bin/perl

# Copyright (c) 2011-2021 chip-remote workers, All rights reserved.
#
# Terms for redistribution and use can be found in LICENCE.

use strict;
use Test::ChipRemote;

cr_test_title q{FEATURES request works};
cr_run_script [
    cr_request(qw{ FEATURES
                   FOCUS
                   INIT
                   LINES
                   LINE
                   MODES
                   PORT
                   PORTS
                   SET } ) ];
