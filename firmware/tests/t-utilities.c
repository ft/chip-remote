/*
 * Copyright (c) 2022 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file t-utilities.c
 * @brief Unit tests for utilities module
 */

#include <stdlib.h>

#include <common/compiler.h>
#include <test/tap.h>

#include <chip-remote.h>
#include <cr-utilities.h>

int
main(UNUSED int argc, UNUSED char *argv[])
{
    plan(26);
    unsigned int flags;
    char *stop;
    cr_number n;

    n = cr_parse_number("a", 16u, &stop, &flags);
    ok(n == 10, "0xa is 10 decimal");
    n = cr_parse_number("f", 16u, &stop, &flags);
    ok(n == 15, "0xf is 15 decimal");
    n = cr_parse_number("10", 8u, &stop, &flags);
    ok(n == 8, "010 is 8 decimal");
    n = cr_parse_number("10", 2u, &stop, &flags);
    ok(n == 2, "0b10 is 2 decimal");
    n = cr_parse_number("10", 36u, &stop, &flags);
    ok(n == 36, "10 (base 36) is 36 decimal");

    n = cr_parse_number("0", 16u, &stop, &flags);
    ok(n == 0, "zero is zero - b16");
    n = cr_parse_number("0", 10u, &stop, &flags);
    ok(n == 0, "zero is zero - b10");
    n = cr_parse_number("0", 8u, &stop, &flags);
    ok(n == 0, "zero is zero - b8");
    n = cr_parse_number("0", 2u, &stop, &flags);
    ok(n == 0, "zero is zero - b2");
    n = cr_parse_number("0", 36u, &stop, &flags);
    ok(n == 0, "zero is zero - b36");

    n = cr_parse_number("101", 16u, &stop, &flags);
    ok(n == 257, "101 is 257 - b16");
    n = cr_parse_number("101", 10u, &stop, &flags);
    ok(n == 101, "101 is 101 - b10");
    n = cr_parse_number("101", 8u, &stop, &flags);
    ok(n == 65, "101 is 65 - b8");
    n = cr_parse_number("101", 2u, &stop, &flags);
    ok(n == 5, "101 is 5 - b2");
    n = cr_parse_number("101", 36u, &stop, &flags);
    ok(n == 1297, "101 is 1297 - b36");

    n = cr_parse_number("a", 10u, &stop, &flags);
    ok(flags == 1, "a is not a decimal digit - flags");
    ok(stop != NULL && *stop == 'a', "a is not a decimal digit - stop");

    n = cr_parse_number("8", 8u, &stop, &flags);
    ok(flags == 1, "8 is not an octal digit - flags");
    ok(stop != NULL && *stop == '8', "8 is not an octal digit - stop");

    n = cr_parse_number("2", 2u, &stop, &flags);
    ok(flags == 1, "2 is not a binary digit - flags");
    ok(stop != NULL && *stop == '2', "2 is not a binary digit - stop");

    n = cr_parse_number("ffffffffffffffff", 16u, &stop, &flags);
    ok(n == 0xffffffffffffffffULL, "CR_NUMBER_MAX fits - n");
    ok(flags == 0, "CR_NUMBER_MAX fits - flags");
    ok(stop != NULL && *stop == '\0', "CR_NUMBER_MAX fits - stop");
    n = cr_parse_number("10000000000000000", 16u, &stop, &flags);
    ok(n == 0xffffffffffffffffULL, "CR_NUMBER_MAX + 1 does not fit - n");
    ok(flags == 2, "CR_NUMBER_MAX + 1 does not fit - flags");

    return EXIT_SUCCESS;
}
