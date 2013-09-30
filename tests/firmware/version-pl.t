#!/usr/bin/perl
use strict;
use Test::ChipRemote;

cr_test_title q{VERSION request works};
cr_run_script [ cr_request("VERSION",
                           "VERSION 2 0 0") ];
