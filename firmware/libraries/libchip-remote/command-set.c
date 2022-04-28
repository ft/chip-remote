/*
 * Copyright (c) 2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-set.c
 * @brief Implemenation of SET command
 */

#include <assert.h>
#include <stdint.h>
#include <string.h>

#include <common/compiler.h>

#include <chip-remote.h>
#include <commands-private.h>
#include <cr-utilities.h>

void
cr_handle_set(struct cr_protocol *proto, UNUSED struct cr_command *cmd,
              struct cr_value *t, unsigned int n)
{
    if (cr_require_numofargs(proto, n, 4) == false) {
        return;
    }

    if (REQUIRE_ARG_TYPE(proto, t, 1, INTEGER) == false) {
        return;
    }
    if (cr_value_max(proto, t, 1, proto->ports.tablesize - 1) == false) {
        return;
    }

    const cr_number idx = t[1].data.number;
    struct cr_port *p = port_by_index(proto, idx);
    if (cr_unknown_port(proto, p)) {
        return;
    }

    if (p->api->set == NULL) {
        proto->reply("wtf Port does not support configuration!\n");
        return;
    }

    cr_handle_port_value(proto, p->api->set(proto, p, n-2, t+2));
}
