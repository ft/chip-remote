#!/usr/bin/perl

# Copyright (c) 2011-2018 chip-remote workers, All rights reserved.
#
# Terms for redistribution and use can be found in LICENCE.

use strict;
use Test::ChipRemote;

cr_test_title q{VERSION request works};
cr_run_script [ cr_request("VERSION",
                           "VERSION 2 0 0") ];
