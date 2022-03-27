/*
 * Copyright (c) 2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-set.c
 * @brief Implemenation of SET command
 */

#include <stdint.h>
#include <string.h>

#include <common/compiler.h>

#include <chip-remote.h>
#include <commands-private.h>
#include <cr-utilities.h>

void
cr_handle_set(struct cr_protocol *proto, UNUSED struct cr_command *cmd,
              struct cr_value *t, UNUSED unsigned int n)
{
    const cr_number idx = t[0].data.number;
    const char *key = t[1].data.symbol;
    const char *value = t[2].data.symbol;
    struct cr_port *p = proto->ports.table[idx];

    if (p->api->set == NULL) {
        proto->reply("wtf Port does not support configuration!\n");
        return;
    }

    switch (p->api->set(p, key, value)) {
    case 0:
        proto->reply("ok\n");
        break;
    case 1:
        proto->reply("wtf Port does not support mode changes\n");
        break;
    case -1:
        proto->reply("value-out-of-range ");
        proto->reply(key);
        proto->reply(": ");
        proto->reply(value);
        cr_proto_put_newline(proto);
        break;
    case -2:
        proto->reply("malformed-request Invalid parameter: ");
        proto->reply(key);
        cr_proto_put_newline(proto);
        break;
    case -3:
        proto->reply("broken-value Invalid argument for ");
        proto->reply(key);
        proto->reply(": ");
        proto->reply(value);
        cr_proto_put_newline(proto);
        break;
    default:
        proto->reply("wtf Unknown configuration error.\n");
        break;
    }
}
