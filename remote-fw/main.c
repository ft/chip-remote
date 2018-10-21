/*
 * Copyright (c) 2017 Frank Terbeck <ft@bewatermyfriend.org>, All rights
 * reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <stddef.h>
#include <stdint.h>
#ifdef WITH_SEMIHOSTING
#include <stdio.h>
#endif /* WITH_SEMIHOSTING */

#include <cr-interface.h>
#include <chip-remote.h>

#include "board.h"

#ifdef WITH_SEMIHOSTING

/* Couldn't find this defined in a header. I'll blame it on the nocturnal hour
 * I am trying to find out. Here's my shortcut for tonight. */
extern void initialise_monitor_handles(void);

#endif /* WITH_SEMIHOSTING */

#define DEFAULT_RATE { CR_IMMUTABLE, -1 }
#define NO_RATE { CR_IMMUTABLE, 0 }

#define NEW_LINE(a,d,m)    \
    {                      \
        a,                 \
        d,                 \
        m,                 \
        { (char)0 },       \
        CR_ROLE_NONE,      \
        CR_NO_INDEX,       \
        CR_MUTABLE         \
    }

#define LINE_LIST_END \
    { NULL, NULL, 0, { (char)0 }, CR_ROLE_NONE, CR_NO_INDEX, CR_MUTABLE }

#define NEW_PORT(n,l)                           \
    {                                           \
        n,                                      \
        0,                                      \
        DEFAULT_RATE,                           \
        { CR_MUTABLE, CR_MODE_NONE, { NULL } }, \
        NULL,                                   \
        l,                                      \
        NULL                                    \
    }

#define PORT_LIST_END \
    { 0, 0, NO_RATE, { CR_IMMUTABLE, CR_MODE_INVALID, {NULL}},  \
      NULL, NULL, NULL }

static struct cr_line port0_lines[] = {
    NEW_LINE(access_port0, dir_port0, 10),
    NEW_LINE(access_port0, dir_port0, 12),
    NEW_LINE(access_port0, dir_port0, 14),
    NEW_LINE(access_port0, dir_port0, 15),
    NEW_LINE(access_port0, dir_port0, 2),
    LINE_LIST_END
};

struct cr_port cr_ports[] = {
    NEW_PORT(4, port0_lines),
    PORT_LIST_END
};

int
main(void)
{
#ifdef WITH_SEMIHOSTING
    initialise_monitor_handles();
#endif /* WITH_SEMIHOSTING */

    board_init();
    cr_init(1);
    for (;;) {
        cr_top_level();
    }
}
