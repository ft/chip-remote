/*
 * Copyright (c) 2014-2018 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file par-ex.c
 * @brief Parallel-Exchange interface implementation
 *
 * The parallel-exchange interface is a simple interface, that consists of 12
 * parallel lines:
 *
 *   CLK:ADDR2:ADDR1:ADDR0:D7:D6:D5:D4:D3:D2:D1:D0
 *
 * The interface is a point-to-point master-slave design. This is the
 * implemenation for the master-side of the interface.
 *
 * The data bits (Dn) are shared between master and slave. CLK and ADDRn are
 * *always* under the control of the master.
 *
 * Data is latched at the POSITIVE-EDGE of CLK. An exchange always happens in
 * two cycles:
 *
 *   - Master → Slave
 *   - Master ← Slave.
 *
 * There is absolutely no configurability with this interface whatsoever.
 */

#include "chip-remote.h"
#include "line.h"
#include "par-ex.h"
#include "utils.h"

static inline void
cr_parex_wait(uint32_t n)
{
    xcr_wait(n);
}

static void
cr_line_write(struct cr_line *line, int value)
{
    line->access(line, CR_ACCESS_WRITE, value);
}

static int
cr_line_read(struct cr_line *line)
{
    return line->access(line, CR_ACCESS_READ, 0);
}

static void
cr_line_input(struct cr_line *line)
{
    line->dir(line, CR_ACCESS_READ);
}

static void
cr_line_output(struct cr_line *line)
{
    line->dir(line, CR_ACCESS_WRITE);
}

int
cr_parex_init(struct cr_port *port)
{
    struct cr_parex_map *map;
    int i;

    map = port->mode.map.parex;
    map->clk = cr_get_line(port, "CLK");
    if (map->clk == NULL)
        return 0;
    for (i = 0; i < 3; ++i) {
        map->addr[i] = cr_get_indexed_line(port, "ADDR", i);
        if ((map->addr[i]) == NULL)
            return 0;
    }
    for (i = 0; i < 8; ++i) {
        map->data[i] = cr_get_indexed_line(port, "DATA", i);
        if ((map->data[i]) == NULL)
            return 0;
    }
    cr_line_output(map->clk);
    cr_line_write(map->clk, 0);
    for (i = 2; i >= 0; --i) {
        cr_line_output(map->addr[i]);
        cr_line_write(map->addr[i], 0);
    }
    for (i = 7; i >= 0; --i) {
        cr_line_output(map->data[i]);
        cr_line_write(map->data[i], 0);
    }
    return 1;
}

uint32_t
cr_parex_transmit(struct cr_port *port, uint32_t data)
{
    struct cr_parex_map *map;
    uint32_t rv;
    int i;

    map = port->mode.map.parex;
    rv = 0;

    /* Setup data word for writing... */
    for (i = 2; i >= 0; --i)
        cr_line_write(map->addr[i], (data & (1<<(i+8))) ? 1 : 0);
    for (i = 7; i >= 0; --i)
        cr_line_write(map->data[i], (data & (1<<i)) ? 1 : 0);

    cr_parex_wait(CR_T_PAREX_HALFPERIOD);
    /* Rising edge latches data */
    cr_line_write(map->clk, 1);

    cr_parex_wait(CR_T_PAREX_HALFPERIOD);

    /* Release data bits for reading... */
    for (i = 7; i >= 0; --i)
        cr_line_input(map->data[i]);

    /* Falling edge tells slave, that data[] is free for it to take. */
    cr_line_write(map->clk, 0);

    cr_parex_wait(CR_T_PAREX_HALFPERIOD);
    cr_line_write(map->clk, 1);

    /* Read data after read-cycle rising edge... */
    for (i = 7; i >= 0; --i)
        rv |= cr_line_read(map->data[i]) ? (1<<i) : 0;

    cr_parex_wait(CR_T_PAREX_HALFPERIOD);
    cr_line_write(map->clk, 0);

    /* Reclaim data bits. */
    for (i = 7; i >= 0; --i)
        cr_line_output(map->data[i]);

    return rv;
}

int
cr_parex_params(struct cr_port *port)
{
    return 0;
}

int
cr_parex_map(struct cr_port *port)
{
    port->mode.map.parex = xmalloc(1, struct cr_parex_map);
    if (port->mode.map.parex == NULL)
        return -1;
    port->mode.map.parex->addr = xmalloc(3, struct cr_line *);
    port->mode.map.parex->data = xmalloc(8, struct cr_line *);
    return 0;
}

int
cr_parex_destroy_map(struct cr_port *port)
{
    struct cr_parex_map *map;

    map = port->mode.map.parex;
    if (map != NULL) {
        free(map->addr);
        map->addr = NULL;
        free(map->data);
        map->data = NULL;
        port->mode.map.spi = NULL;
    }
    return 0;
}
