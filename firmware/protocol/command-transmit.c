/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-transmit.c
 * @brief Implemenation of TRANSMIT command
 */

#include <stdio.h>

#include <common/compiler.h>

#include <commands.h>
#include <commands-private.h>

struct cr_argument transmit_arguments[] = {
    { .optional = false, .type = CR_PROTO_ARG_TYPE_INTEGER },
    { .optional = false, .type = CR_PROTO_ARG_TYPE_VOID }
};

static void
uint2str(uint32_t num, char *buf)
{
    static char chtable[] = {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        'a', 'b', 'c', 'd', 'e', 'f' };

    /* buf needs to be able to hold 8+1 characters. */
    int i, max, step;

    for (i = 7; i >= 0; --i) {
        step = 4 * i;
        if (num & (uint32_t)(0xful << step)) {
            max = i;
            break;
        }
    }

    if (i < 0) {
        buf[0] = '0';
        buf[1] = '\0';
        return;
    }

    buf[i+1] = '\0';
    for (i = max; i >= 0; --i) {
        step = 4 * i;
        buf[max-i] = chtable[(num & (uint32_t)(0xful << step)) >> step];
    }
    buf[8] = '\0';
}

static void
put_u32(const struct cr_protocol *proto, const uint32_t value)
{
    char buffer[9u];
    uint2str(value, buffer);
    proto->reply(buffer);
}

enum cr_proto_state
cr_handle_transmit(const struct cr_protocol *proto,
                   UNUSED const struct cr_command *cmd,
                   const struct cr_value *arg,
                   UNUSED unsigned int argn)
{
    uint32_t rx;
    proto->transmit(proto->ports.table + proto->ports.current,
                    arg[0].data.u32, &rx);
    put_u32(proto, rx);
    proto->reply("\n");
    return CR_PROTO_STATE_ACTIVE;
}
