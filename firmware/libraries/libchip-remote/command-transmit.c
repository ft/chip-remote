/*
 * Copyright (c) 2020-2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-transmit.c
 * @brief Implemenation of TRANSMIT command
 */

#include <ufw/compiler.h>

#include <commands.h>
#include <commands-private.h>
#include <chip-remote.h>
#include <cr-port.h>
#include <cr-utilities.h>

void
cr_handle_transmit(struct cr_protocol *proto, UNUSED struct cr_command *cmd,
                   struct cr_value *t, unsigned int n)
{
    struct cr_port *p = current_port(proto);

    if (p->initialised == false) {
        proto->reply("wtf Focused port is not initialised!\n");
        return;
    }

    cr_handle_port_value(proto, p->api->xfer(proto, p, n-1, t+1));
}
