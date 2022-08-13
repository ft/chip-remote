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

#include <ufw/compiler.h>

#include <chip-remote.h>
#include <commands-private.h>
#include <cr-utilities.h>

void
cr_handle_init(struct cr_protocol *proto, UNUSED struct cr_command *cmd,
               struct cr_value *t, UNUSED unsigned int n)
{
    if (cr_require_numofargs(proto, n, 2) == false) {
        return;
    }
    if (REQUIRE_ARG_TYPE(proto, t, 1, INTEGER) == false) {
        return;
    }
    if (cr_value_max(proto, t, 1, proto->ports.tablesize - 1) == false) {
        return;
    }

    cr_number idx = t[1].data.number;
    struct cr_port *p = port_by_index(proto, idx);
    cr_handle_port_value(proto, p->api->init(proto, p));
}
