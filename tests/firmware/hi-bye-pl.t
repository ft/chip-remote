#!/usr/bin/perl

# Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
#
# Terms for redistribution and use can be found in LICENCE.

use strict;
use Test::ChipRemote;

cr_test_title q{Trivial conversation};
cr_run_script [
    # HI in “active” state and BYE in “idle” state trigger an unexpected
    # command reply. We can't test the BYE case, because the simulator instance
    # of the firmware exists after it processes a BYE request.
    cr_request("HI", "WTF Unexpected command") ];
