/*
 * Copyright (c) 2013-2014 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <stdlib.h>

#include "chip-remote.h"
#include "platform.h"

/* -------------------------------------------------------------------------- */

#define DEFAULT_RATE { CR_IMMUTABLE, -1 }
#define NO_RATE { CR_IMMUTABLE, 0 }

#define NEW_LINE(a,m)      \
    {                      \
        a,                 \
        m,                 \
        { (char)0 },       \
        CR_ROLE_NONE,      \
        CR_NO_INDEX,       \
        CR_MUTABLE         \
    }

#define LINE_LIST_END \
    { NULL, 0, { (char)0 }, CR_ROLE_NONE, CR_NO_INDEX, CR_MUTABLE }

#define NEW_PORT(n,l)                   \
    {                                   \
        n,                              \
        0,                              \
        DEFAULT_RATE,                   \
        { CR_MUTABLE, CR_MODE_NONE },   \
        NULL,                           \
        l,                              \
        NULL                            \
    }

#define PORT_LIST_END \
    { 0, 0, NO_RATE, { CR_IMMUTABLE, CR_MODE_INVALID }, NULL, NULL, NULL }

/* -------------------------------------------------------------------------- */

#ifdef CR_MSP430F1481

#include "arch/cr-msp430.h"

static struct cr_line port1_lines[] = {
    NEW_LINE(access_port1, 1<<0),
    NEW_LINE(access_port1, 1<<1),
    NEW_LINE(access_port1, 1<<2),
    NEW_LINE(access_port1, 1<<3),
    LINE_LIST_END
};

static struct cr_line port2_lines[] = {
    NEW_LINE(access_port1, 1<<4),
    NEW_LINE(access_port1, 1<<5),
    NEW_LINE(access_port1, 1<<6),
    NEW_LINE(access_port1, 1<<7),
    LINE_LIST_END
};

struct cr_port cr_ports[] = {
    NEW_PORT(4, port1_lines),
    NEW_PORT(8, port2_lines),
};

#endif /* CR_MSP430F1481 */

#ifdef CR_STDOUT

#include "arch/stdout.h"

static struct cr_line port1_lines[] = {
    NEW_LINE(access_portA, 1<<0),
    NEW_LINE(access_portA, 1<<1),
    NEW_LINE(access_portA, 1<<2),
    NEW_LINE(access_portA, 1<<3),
    LINE_LIST_END
};

static struct cr_line port2_lines[] = {
    NEW_LINE(access_portA, 1<<4),
    NEW_LINE(access_portA, 1<<5),
    NEW_LINE(access_portA, 1<<6),
    NEW_LINE(access_portA, 1<<7),
    NEW_LINE(access_portA, 1<<8),
    NEW_LINE(access_portA, 1<<9),
    NEW_LINE(access_portA, 1<<10),
    NEW_LINE(access_portA, 1<<11),
    LINE_LIST_END
};

struct cr_port cr_ports[] = {
    NEW_PORT(4, port1_lines),
    NEW_PORT(8, port2_lines),
    PORT_LIST_END
};

#endif /* CR_STDOUT */

#ifdef CR_SIM
int
main(int argc, char *argv[])
#else
void
main(void)
#endif
{
    /*
     * This is where you'd perform default port configuration. If you don't,
     * all ports defined above do not have any default configuration with all
     * lines and parameters are marked as mutable.
     */

    cr_init(1);
    for (;;)
        cr_top_level();

#ifdef CR_SIM
    return EXIT_SUCCESS;
#endif
}
