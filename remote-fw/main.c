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

#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>

#include "cr/chip-remote.h"
#include "cr/nucleo-144.h"

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

static struct cr_line port1_lines[] = {
    /* CLK, MOSI, MISO plus for times CS: */
    NEW_LINE(access_port1, dir_port1, 1<<4),
    NEW_LINE(access_port1, dir_port1, 1<<5),
    NEW_LINE(access_port1, dir_port1, 1<<6),
    NEW_LINE(access_port1, dir_port1, 1<<7),
    NEW_LINE(access_port2, dir_port2, 1<<0),
    NEW_LINE(access_port2, dir_port2, 1<<1),
    NEW_LINE(access_port2, dir_port2, 1<<2),
    LINE_LIST_END
};

static struct cr_line port2_lines[] = {
    /* CLK, 3*ADDRESS, 8*DATA: */
    NEW_LINE(access_port4, dir_port4, 1<<0),
    NEW_LINE(access_port4, dir_port4, 1<<1),
    NEW_LINE(access_port4, dir_port4, 1<<2),
    NEW_LINE(access_port4, dir_port4, 1<<3),
    NEW_LINE(access_port4, dir_port4, 1<<4),
    NEW_LINE(access_port4, dir_port4, 1<<5),
    NEW_LINE(access_port4, dir_port4, 1<<6),
    NEW_LINE(access_port4, dir_port4, 1<<7),
    NEW_LINE(access_port5, dir_port5, 1<<0),
    NEW_LINE(access_port5, dir_port5, 1<<1),
    NEW_LINE(access_port5, dir_port5, 1<<2),
    NEW_LINE(access_port5, dir_port5, 1<<3),
    LINE_LIST_END
};

struct cr_port cr_ports[] = {
    NEW_PORT(7, port1_lines),
    NEW_PORT(12, port2_lines),
    PORT_LIST_END
};

static void
init_gpio(void)
{
    rcc_periph_clock_enable(RCC_GPIOB);
    gpio_mode_setup(GPIOB, GPIO_MODE_OUTPUT, GPIO_PUPD_NONE, GPIO0);
}

int
main(void)
{
#ifdef WITH_SEMIHOSTING
    initialise_monitor_handles();
#endif /* WITH_SEMIHOSTING */

    init_gpio();
    cr_init(1);
    for (;;) {
        cr_top_level();
    }
}
