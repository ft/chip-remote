#ifndef INC_PORT_H
#define INC_PORT_H

#include <stdlib.h>
#include "platform.h"
#include "spi.h"

enum cr_port_modes {
    CR_MODE_SPI
};

/**
 * Description of a port configuration
 *
 * Each port has a current mode attached to it. Each mode has a specific
 * configuration structure, that is accessable via this structure's "u" union.
 */
struct cr_port_cfg {
    enum cr_port_modes mode;
    union {
        struct cr_mode_spi spi;
    } u;
};

/**
 * Description of a line (or pin) within chip-remote
 *
 * In chip-remote, a port is made up of a number of pins. This structure
 * describes such a pin.
 *
 * On microcontrollers, ports are made up of pins, too. Usually in packs of
 * eight or so, like this:
 *
 * @code
 *   +-------------------------------+
 *   | micro-controller port A       |
 *   +-------------------------------+
 *   | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
 *   +-------------------------------+
 *   | CR Port 0 |     CR Port 1     |
 *   +-------------------------------+
 * @endcode
 *
 * And like listed below that, a micro-controller port can host multiple
 * chip-remote ports. Or the other way around: A big chip-remote port can span
 * over multiple micro-controller ports.
 *
 * So, a pin is described by micro-controller port address and bit-mask (in
 * which only one bit should be set unless you are being tricky).
 */
struct cr_line {
    cr_port_addr addr_in;
    cr_port_addr addr_out;
    cr_pin_mask bitmask;
};

/**
 * Description of a port in chip-remote terms
 *
 * A port has a list of lines and a configuration.
 */
struct cr_port {
    struct cr_port_cfg *cfg;
    size_t nlines;
    struct cr_line *lines;
};

extern struct cr_port cr_ports[];

size_t cr_numofports(struct cr_port *);

#endif /* INC_PORT_H */
