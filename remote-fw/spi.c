/*
 * Copyright (c) 2013-2014 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file spi.c
 * @brief Portable code for SPI operation
 *
 * The Serial Peripheral Interface Bus (SPI) is a synchronous serial interface,
 * that is very commonly used for chip configuration purposes. It features
 * exactly one master and - depending on the setup - one or more slave devices
 * that share the bus.
 *
 * In its most generic form it uses four lines:
 *
 * - CLK
 * - CS
 * - MOSI
 * - MISO
 *
 * CLK is the general clock line, that the master controls and all slaves
 * synchronise to.
 *
 * CS is a chip-select line, that the master uses to tell which of the slaves
 * should listen (sometimes this line is also called LE [latch-enable] and
 * sometimes SS [slave-select], but we'll stick with CS).
 *
 * MOSI and MISO are the data lines. MOSI is Master-Out-Slave-In and MISO is
 * Master-In-Slave-Out (the former is sometimes called Data-Out and the latter
 * Data-In, but again we'll stick with MOSI and MISO).
 *
 * As a variation of this scheme, in a point-to-point setup between one master
 * and exactly one slave, the CS line is sometimes omitted. This is sometimes
 * called 3-pin SPI.
 *
 * Data exchange works like this: CLK runs and CS is enabled. Now the master
 * transmits on MOSI and listens on MISO while the slave does the same thing
 * just the other way around.
 *
 * That is about all there is to know about the basic operation of the bus.
 *
 * There are a number of details, that need further discussion:
 *
 * - Clock Rate: While this seems like an obvious parameter that one should be
 *   able to tweak, in reality this is not very critical: Since the operation
 *   is synchronous, the master dictates the rate of operation in any case.
 *   While there is a maximum there usually isn't a minimum rate. Tweaking
 *   clock-rate to an exact value is not very easy and might complicate the
 *   code. So it is currently not on the agenda.
 *
 * - Frame Length: Unit of a fixed number of bits that is exchanged in one
 *   transaction. This is obviously important and depends on the specific
 *   slave.
 *
 * - Number of CS lines: This may range from zero (which implies 3-wire SPI
 *   operation) to any number the port in question can support, in order to
 *   address more than one SPI slave on the same bus. If more than one
 *   chip-select line is configured, the client needs to switch the chip-select
 *   focus before sending data.
 *
 * - CS Polarity: Determines whether CS is active high or active low.
 *
 * - CLK Polarity: Does CLK idle high or low?
 *
 * - CLK Phase Delay: When set, CLK phase is delayed by one half CLK cycle and
 *   data is latched at the leading edge (if CLK Polarity is 0 - falling edge
 *   otherwise) of CLK. Otherwise data is latched at the other CLK edge.
 *
 * - Bit Order: In a frame transmit data LSB-first or MSB-first?
 *
 * The reference implementation uses GPIO Pins to implement SPI in a
 * configurable, portable, bit-banged fashion (the controller-specific code
 * only needs to expose a way to manipulate a pin's polarity). Other
 * implementations may obviously leverage hardware SPI interfaces should their
 * target controller support that.
 */

#include <stdint.h>

#include "chip-remote.h"
#include "parameters.h"
#include "properties.h"
#include "line.h"
#include "spi.h"
#include "utils.h"

static inline void
spi_wait(uint32_t n)
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
cr_set_line_with_polarity(struct cr_line *line, int polarity)
{
    cr_line_write(line, !polarity);
}

static void
cr_unset_line_with_polarity(struct cr_line *line, int polarity)
{
    cr_line_write(line, polarity);
}

static void
cr_spi_set_addr(struct cr_line **cs, int n, uint32_t addr, int polarity)
{
    int i;

    for (i = 0; i < n; ++i)
        if ((1<<i) & addr)
            cr_set_line_with_polarity(cs[i], polarity);
}

static void
cr_spi_unset_addr(struct cr_line **cs, int n, uint32_t addr, int polarity)
{
    int i;

    for (i = 0; i < n; ++i)
        if ((1<<i) & addr)
            cr_unset_line_with_polarity(cs[i], polarity);
}

uint32_t
cr_spi_transmit(struct cr_port *port, uint32_t data)
{
    struct cr_spi_map *map;
    uint32_t rv, scan;
    int i;

    rv = 0;
    map = port->mode.map.spi;
    cr_spi_set_addr(map->cs, map->cs_lines_num, map->address, map->cs_polarity);
    spi_wait(CR_T_SPI_CS_SETUP);
    for (i = 0; i < map->frame_length; ++i) {
        /* Determine the current scan-mask depending on bit-order */
        if (map->bit_order == SPI_LSB_FIRST)
            scan = (uint32_t)1 << i;
        else
            scan = (uint32_t)1 << (map->frame_length - (i+1));

        /* Setup the current bit on MOSI */
        cr_line_write(map->mosi, (data & scan) ? 1 : 0);

        if (map->clk_phase_delay) {
            spi_wait(CR_T_SPI_BIT_DURATION / 2);
            cr_set_line_with_polarity(map->clk, map->clk_polarity);
        } else {
            cr_set_line_with_polarity(map->clk, map->clk_polarity);
            spi_wait(CR_T_SPI_BIT_DURATION / 2);
        }

        /* Read MISO after half a bit */
        rv |= cr_line_read(map->miso) ? scan : 0;

        if (map->clk_phase_delay) {
            spi_wait(CR_T_SPI_BIT_DURATION / 2);
            cr_unset_line_with_polarity(map->clk, map->clk_polarity);
        } else {
            cr_unset_line_with_polarity(map->clk, map->clk_polarity);
            spi_wait(CR_T_SPI_BIT_DURATION / 2);
        }
    }
    spi_wait(CR_T_SPI_CS_SETUP);
    cr_spi_unset_addr(map->cs,map->cs_lines_num,map->address,map->cs_polarity);
    return rv;
}

/**
 * Check configuration and initialise an SPI port
 *
 * This is called by `cr_init_port()' via the `initfnc' pointer from port.c's
 * `mode_helpers' table. Its job is to process the port's parameter and line
 * lists and set the port's map structure accordingly.
 *
 * @return 0 if initialisation failed; non-zero otherwise.
 */
int
cr_spi_init(struct cr_port *port)
{
    static struct cr_symb_tab cs_polarity_tab[] = {
        { "ACTIVE-HIGH", 0 },
        { "ACTIVE-LOW", 1 },
        { NULL, -1 }
    };
    static struct cr_symb_tab clk_polarity_tab[] = {
        { "RISING-EDGE", 0 },
        { "FALLING-EDGE", 1 },
        { NULL, -1 }
    };
    static struct cr_symb_tab bit_order_tab[] = {
        { "MSB-FIRST", SPI_MSB_FIRST },
        { "LSB-FIRST", SPI_LSB_FIRST },
        { NULL, -1 }
    };
    struct cr_spi_map *map;
    struct cr_parameter *params;
    int i;

    map = port->mode.map.spi;
    params = port->params;

    CR_SYMB_PARAM_GET(params, "CLK-POLARITY",
                      clk_polarity_tab, map->clk_polarity);
    CR_SYMB_PARAM_GET(params, "CS-POLARITY",
                      cs_polarity_tab, map->cs_polarity);
    CR_SYMB_PARAM_GET(params, "BIT-ORDER", bit_order_tab, map->bit_order);
    CR_BOOL_PARAM_GET(params, "CLK-PHASE-DELAY", map->clk_phase_delay);
    CR_UINT_PARAM_GET(params, "FRAME-LENGTH", map->frame_length);
    CR_UINT_PARAM_GET(params, "CS-LINES", map->cs_lines_num);

    map->clk = cr_get_line(port, "CLK");
    if (map->clk == NULL)
        return 0;
    map->mosi = cr_get_line(port, "MOSI");
    if (map->mosi == NULL)
        return 0;
    map->miso = cr_get_line(port, "MISO");
    if (map->miso == NULL)
        return 0;
    for (i = 0; i < map->cs_lines_num; ++i) {
        map->cs[i] = cr_get_indexed_line(port, "CS", i);
        if ((map->cs[i]) == NULL)
            return 0;
    }
    /* Bring CS and CLK to their idle states */
    for (i = 0; i < map->cs_lines_num; ++i) {
        map->cs[i]->dir(map->cs[i], CR_ACCESS_WRITE);
        cr_unset_line_with_polarity(map->cs[i], map->cs_polarity);
    }
    map->address = 1;
    map->clk->dir(map->clk, CR_ACCESS_WRITE);
    cr_unset_line_with_polarity(map->clk, map->clk_polarity);
    /* Directions of MISO and MOSI */
    map->mosi->dir(map->mosi, CR_ACCESS_WRITE);
    map->miso->dir(map->miso, CR_ACCESS_READ);
    return 1;
}

int
cr_spi_destroy_map(struct cr_port *port)
{
    struct cr_spi_map *map;

    map = port->mode.map.spi;
    if (map != NULL) {
        free(map->cs);
        map->cs = NULL;
        free(map);
        port->mode.map.spi = NULL;
    }
    return 0;
}

/**
 * Allocate and initialise a parameter list for SPI mode
 *
 * The SPI mode implemented in this firmware is configurable via a list of
 * parameters. This list of parameters can by modified via the protocol's SET
 * request. Each time, a parameter is changed, the port needs to be
 * re-initialised.
 *
 * This function's job is to allocate a list that contains all SPI parameters,
 * that are supported by this implementation.
 *
 * @param  port   the port in which to link the new list into
 *
 * @return integer; value less than zero signals out-of-memory
 */
int
cr_spi_params(struct cr_port *port)
{
    struct cr_parameter *new;
    int n;
    char buf[CR_INT_MAX_LEN + 1];

    new = xmalloc(7, struct cr_parameter);
    if (new == NULL)
        return -1;

    cr_param_init(new, 0, "BIT-ORDER", "MSB-FIRST");
    cr_param_init(new, 1, "CLK-PHASE-DELAY", "TRUE");
    cr_param_init(new, 2, "CLK-POLARITY", "RISING-EDGE");
    n = port->lines - 3;
    uint2str((uint32_t)n, buf);
    cr_param_init(new, 3, "CS-LINES", buf);
    /* CR_MARK_PROP(&(new[3].value), CR_IMMUTABLE); */
    cr_param_init(new, 4, "CS-POLARITY", "ACTIVE-LOW");
    cr_param_init(new, 5, "FRAME-LENGTH", "8");
    cr_end_param_init(new, 6);
    port->params = new;

    return 0;
}

/**
 * Allocate a mode-specific mapping for SPI mode
 *
 * The port's parameter list is designed to be easy to interface the
 * chip-remote protocol. The port's line list allows for complete control over
 * which physical pin is used for which action. The SPI implementation on the
 * other hand needs mode direct access to all those parameters. There is no
 * time to traverse the different parameter lists every time just to figure
 * out, which pin should be used for the CLK functionality of the interface or
 * whether or not it should be phase-delayed from the data streams on the MISO
 * and MOSI pins (which you'd also have to look up in the line list of the
 * port). This way of doing things wouldn't scale.
 *
 * Instead an interface implementation (such as SPI) defines a map of lines and
 * values for it to have immediate access to, so there is a chance for decent
 * performance.
 *
 * This function's job in that system is to allocate memory for the SPI mode's
 * mode-map. It's doesn't have to do any initialisation, since that's the job
 * of the `init' step (see `cr_spi_in()' above).
 *
 * @param  port   the port in which to link the new map into
 *
 * @return integer; value less than zero signals out-of-memory
 */
int
cr_spi_map(struct cr_port *port)
{
    port->mode.map.spi = xmalloc(1, struct cr_spi_map);
    if (port->mode.map.spi == NULL)
        return -1;

    CR_UINT_PARAM_GET(
        port->params, "CS-LINES", port->mode.map.spi->cs_lines_num);
    port->mode.map.spi->cs = xmalloc(port->mode.map.spi->cs_lines_num,
                                     struct cr_line *);
    if (port->mode.map.spi->cs == NULL) {
        free(port->mode.map.spi);
        return -1;
    }
    return 0;
}

int
cr_spi_address(struct cr_port *port, uint32_t addr)
{
    struct cr_spi_map *map;
    uint32_t max;

    map = port->mode.map.spi;
    max = (1 << map->cs_lines_num) - 1;
    if (addr > max)
        return 0;
    map->address = addr;
    return 1;
}
