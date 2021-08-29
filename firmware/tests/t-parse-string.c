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

struct resultandcode {
    struct cr_proto_parse result;
    enum cr_proto_result code;
    bool success;
};

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
static size_t rbi = 0u;

static void
mem_sink_init(void)
{
    reply_buffer[0] = '\0';
    rbi = 0u;
}

static void
mem_sink(const char *buf)
{
    if (rbi >= BUFFER_SIZE)
        return;

    strlcpy(reply_buffer + rbi, buf, BUFFER_SIZE - rbi);
    rbi += strlen(buf);
}

static struct resultandcode
test_parse(const char *str, const enum cr_proto_command id)
{
    struct resultandcode rv;
    mem_sink_init();
    strlcpy(test_buffer, str, sizeof(test_buffer));

    rv.code = cr_parse_string(mem_sink, test_buffer, &rv.result);
    rv.success = rv.code == CR_PROTO_RESULT_OK;
    ok(rv.success, "'%s' parses ok", str);

    if (rv.success == false) {
        basic_failure(rv.code);
    } else {
        unless (ok(rv.result.cmd->id == id, "'%s' result has correct id", str))
            rv.success = false;
    }

    return rv;
}

static void
test_trivial_command(const char *name, const enum cr_proto_command id)
{
    struct resultandcode pr = test_parse(name, id);
    if (pr.success == false)
        return;

    ok(pr.result.argn == 0, "%s takes no arguments", name);
}

#define ttc(n,id) test_trivial_command(n, CR_PROTO_CMD_ ## id)

static void
test_command_transmit(void)
{
    struct resultandcode pr = test_parse("transmit 12345678",
                                         CR_PROTO_CMD_TRANSMIT);
    if (pr.success == false)
        return;

    const struct cr_value *arg = pr.result.args;
    ok(pr.result.argn == 1, "One argument parsed");
    ok(arg->type == CR_PROTO_ARG_TYPE_INTEGER, "Argument type: Integer");
    ok(arg->data.u32 == 0x12345678, "Argument value: 0x12345678");
}

int
main(UNUSED int argc, UNUSED char *argv[])
{
    plan(7u * 3u
         + 5u);
    ttc("hi", HI);                /* Tests: 3 */
    ttc("bye", BYE);              /* Tests: 3 */
    ttc("features", FEATURES);    /* Tests: 3 */
    ttc("ports", PORTS);          /* Tests: 3 */
    ttc("version", VERSION);      /* Tests: 3 */
    ttc("VERSION", UVERSION);     /* Tests: 3 */
    ttc("+version", FW_VERSION);  /* Tests: 3 */
    test_command_transmit();      /* Tests: 5 */
    return EXIT_SUCCESS;
}
