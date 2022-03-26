/*
 * Copyright (c) 2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-init.c
 * @brief Implemenation of PORTS command
 */

#include <stdint.h>

#include <common/compiler.h>

#include <chip-remote.h>
#include <commands-private.h>
#include <cr-utilities.h>

struct cr_argument init_arguments[] = {
    CMD_MANDATORY_ARG(INTEGER),
    CMD_END_OF_ARGS
};

void
cr_handle_init(const struct cr_protocol *proto,
               const struct cr_proto_parse *cmd)
{
    cr_number idx = cmd->args[0].data.number;
    if (idx >= proto->ports.tablesize) {
        proto->reply("value-out-of-range ");
        cr_proto_put_number(proto, idx);
        proto->reply(" > ");
        cr_proto_put_number(proto, proto->ports.tablesize - 1);
        cr_proto_put_newline(proto);
    } else {
        struct cr_port *p = proto->ports.table[proto->ports.current];
        int rv = p->api->init(p);
        if (rv == 0)
            proto->reply("ok\n");
        else
            proto->reply("wtf Port initialisation failed!\n");
    }
}
