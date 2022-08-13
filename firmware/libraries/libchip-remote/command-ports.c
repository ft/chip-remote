/*
 * Copyright (c) 2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-ports.c
 * @brief Implemenation of PORTS command
 */

#include <ufw/compiler.h>

#include <chip-remote.h>
#include <commands-private.h>
#include <cr-utilities.h>

void
cr_handle_ports(struct cr_protocol *proto, UNUSED struct cr_command *cmd,
                UNUSED struct cr_value *t, UNUSED unsigned int n)
{
    proto->reply("ports ");
    cr_proto_put_number(proto, proto->ports.tablesize);
    proto->reply(";focus ");
    if (proto->ports.focused) {
        cr_proto_put_number(proto, proto->ports.current);
    } else {
        proto->reply("none");
    }
    cr_proto_put_newline(proto);
}
