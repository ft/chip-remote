#!/bin/sh

# Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
#
# Terms for redistribution and use can be found in LICENCE.

_num_tests=0
_test_name=''

define_test () {
    _num_tests=$(expr "$_num_tests" + 1)
    _test_name="$1"
    shift
    if { "$@"; }; then
        printf 'ok %d - %s\n' "$_num_tests" "$_test_name"
    else
        printf 'nok %d - %s\n' "$_num_tests" "$_test_name"
    fi
}

plan () {
    printf '1..%d\n' "$1"
}

no_plan () {
    printf '1..%d\n' "$_num_tests"
}
