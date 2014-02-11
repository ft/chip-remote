#!/bin/sh

# Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
#
# Terms for redistribution and use can be found in LICENCE.

. "${DISPATCH_ROOT}/tap.sh"
. "${DISPATCH_ROOT}/scheme.env"

simulator_test () {
    _scm_script="tests/sim/$1"
    define_test "$_scm_script" \
                perl "${SOURCE_DIR}/simulator/boardsimu" -f guile "$_scm_script"
}
