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
 *   data is latched at the leading edge (is CLK Polarity is 0 - falling edge
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

#include "chip-remote.h"
#include "spi.h"
#include <stdint.h>

static inline void
spi_wait(uint32_t n)
{
    uint32_t i;
    for (i = 0; i < n; ++i)
        /* nop */;
}

static void
cr_line_write(struct cr_line *line, int value)
{
    line->access(line->bitmask, CR_ACCESS_WRITE, value);
}

static int
cr_line_read(struct cr_line *line)
{
    return line->access(line->bitmask, CR_ACCESS_READ, 0);
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

uint32_t
cr_spi_transmit(struct cr_port *port, uint32_t data)
{
    struct cr_spi_map *map;
    uint32_t rv, scan;
    int i;

    rv = 0;
    map = port->mode.map.spi;
    cr_set_line_with_polarity(map->cs[map->cs_focused], map->cs_polarity);
    spi_wait(CR_T_SPI_CS_SETUP);
    for (i = 0; i < map->frame_length; ++i) {
        /* Determine the current scan-mask depending on bit-order */
        if (map->bit_order == SPI_LSB_FIRST)
            scan = (uint32_t)1 << i;
        else
            scan = (uint32_t)1 << (map->frame_length - (i+1));

        /* Setup the current bit on MOSI */
        cr_line_write(map->mosi, (data & scan));
        if (!map->clk_phase_delay)
            cr_set_line_with_polarity(map->clk, map->clk_polarity);
        spi_wait(CR_T_SPI_BIT_DURATION / 2);

        /* Read MISO after half a bit */
        rv |= cr_line_read(map->miso) ? scan : 0;
        if (map->clk_phase_delay)
            cr_set_line_with_polarity(map->clk, map->clk_polarity);

        spi_wait(CR_T_SPI_BIT_DURATION / 2);
        cr_unset_line_with_polarity(map->clk, map->clk_polarity);
    }
    spi_wait(CR_T_SPI_CS_SETUP);
    cr_set_line_with_polarity(map->cs[map->cs_focused], map->cs_polarity);
    return rv;
}

/**
 * Check configuration and initialise an SPI port
 *
 * - CLK: output; default value depends on CLK Polarity
 * - MOSI: output
 * - MISO: input
 * - CS:*: outputs; default value depands on Chip-Select Polarity
 *
 * This is called by `cr_port_init()', which will have taken care of
 * initialising port->m (which is a pointer to a struct cr_port_mode).
 *
 * If port->m is NULL, that indicates, that this is the initial configuration
 * with this mode. In that case obviously all configuration needs to be
 * performed. There is port->m->u->focused_changed, that indicates, that only
 * the focused chip-select line changed.
 *
 * @return 0 if initialisation failed; non-zero otherwise.
 */
int
cr_spi_init(struct cr_port *port)
{
    return 0;
}

int
cr_spi_params(struct cr_port *port)
{
    return 0;
}

int
cr_spi_map(struct cr_port *port)
{
    return 0;
}
