/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file t-parse-string.c
 * @brief Unit tests for parse-string module
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <parse-string.h>

#include <test/tap.h>
#include <c/compat/strings.h>
#include <common/compiler.h>

#define BUFFER_SIZE 1024u
char test_buffer[BUFFER_SIZE];

static void
basic_failure(enum cr_proto_result code)
{
    printf("# Initial test returned 0x%04x. "
           "Dismissing dependent tests.\n",
           code);
}

static char reply_buffer[BUFFER_SIZE];

static void
mem_sink(const char *buf)
{
    strlcpy(reply_buffer, buf, BUFFER_SIZE);
}

static void
test_command_hi(void)
{
    struct cr_proto_parse result;
    enum cr_proto_result code;

    strlcpy(reply_buffer, "", sizeof(test_buffer));
    strlcpy(test_buffer, "HI", sizeof(test_buffer));

    code = cr_parse_string(mem_sink, test_buffer, &result);
    bool success = code == CR_PROTO_RESULT_OK;
    ok(success, "HI parses ok");

    if (success == false) {
        basic_failure(code);
    } else {
        ok(result.cmd->id == CR_PROTO_CMD_HI, "HI result has correct id");
        ok(result.argn == 0, "HI takes no arguments");
    }
}

int
main(UNUSED int argc, UNUSED char *argv[])
{
    plan(3);
    test_command_hi(); /* Tests: 3 */
    return EXIT_SUCCESS;
}
