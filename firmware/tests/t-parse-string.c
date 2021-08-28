/*
 * Copyright (c) 2020-2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file t-parse-string.c
 * @brief Unit tests for parse-string module
 */

#include <stdbool.h>
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
    strlcpy(test_buffer, "hi", sizeof(test_buffer));

    code = cr_parse_string(mem_sink, test_buffer, &result);
    bool success = code == CR_PROTO_RESULT_OK;
    ok(success, "hi parses ok");

    if (success == false) {
        basic_failure(code);
    } else {
        ok(result.cmd->id == CR_PROTO_CMD_HI, "hi result has correct id");
        ok(result.argn == 0, "hi takes no arguments");
    }
}

static void
test_command_transmit(void)
{
    struct cr_proto_parse result;
    enum cr_proto_result code;

    strlcpy(reply_buffer, "", sizeof(test_buffer));
    strlcpy(test_buffer, "transmit 12345678", sizeof(test_buffer));

    code = cr_parse_string(mem_sink, test_buffer, &result);
    bool success = code == CR_PROTO_RESULT_OK;
    ok(success, "'%s' parses ok", test_buffer);

    if (success == false) {
        basic_failure(code);
    } else {
        ok(result.cmd->id == CR_PROTO_CMD_TRANSMIT,
           "transmit result has correct id");
        ok(result.argn == 1, "One rgument parsed");
        ok(result.args[0].type == CR_PROTO_ARG_TYPE_INTEGER,
           "Argument type: Integer");
        ok(result.args[0].data.u32 == 0x12345678,
           "Argument value: 0x12345678");
    }
}

int
main(UNUSED int argc, UNUSED char *argv[])
{
    plan(3u + 5u);
    test_command_hi();            /* Tests: 3 */
    test_command_transmit();      /* Tests: 5 */
    return EXIT_SUCCESS;
}
