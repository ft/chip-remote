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
cr_handle_set(const struct cr_protocol *proto,
              const struct cr_proto_parse *cmd)
{
    const uint32_t idx = cmd->args[0].data.u32;
    const char *key = cmd->args[1].data.string;
    const char *value = cmd->args[2].data.string;
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
