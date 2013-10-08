#include "chip-remote.h"
#include "port.h"
#include "parameters.h"
#include "properties.h"
#include "protocol.h"
#include "spi.h"
#include "utils.h"

static struct {
    enum cr_port_modes mode;
    int (*set_parameters)(struct cr_port *);
    int (*set_map)(struct cr_port *);
    int (*destroy_map)(struct cr_port *);
    int (*init_port)(struct cr_port *);
    uint32_t (*transmit)(struct cr_port *, uint32_t);
} mode_helpers[] = {
    { CR_MODE_NONE, NULL, NULL, NULL, NULL },
    { CR_MODE_SPI, cr_spi_params,
                   cr_spi_map,
                   cr_spi_destroy_map,
                   cr_spi_init,
                   cr_spi_transmit },
    { CR_MODE_INVALID, NULL, NULL, NULL, NULL }
};

typedef int (*port_setter)(struct cr_port *);
typedef uint32_t (*port_transmitter)(struct cr_port *, uint32_t);

static port_transmitter
cr_port_transmitter(enum cr_port_modes mode)
{
    int i;

    for (i = 0; mode_helpers[i].mode != CR_MODE_INVALID; ++i)
        if (mode_helpers[i].mode == mode)
            return mode_helpers[i].transmit;
    return NULL;
}

static port_setter
cr_params_setter(enum cr_port_modes mode)
{
    int i;

    for (i = 0; mode_helpers[i].mode != CR_MODE_INVALID; ++i)
        if (mode_helpers[i].mode == mode)
            return mode_helpers[i].set_parameters;
    return NULL;
}

static port_setter
cr_map_setter(enum cr_port_modes mode)
{
    int i;

    for (i = 0; mode_helpers[i].mode != CR_MODE_INVALID; ++i)
        if (mode_helpers[i].mode == mode)
            return mode_helpers[i].set_map;
    return NULL;
}

static port_setter
cr_map_destroy(enum cr_port_modes mode)
{
    int i;

    for (i = 0; mode_helpers[i].mode != CR_MODE_INVALID; ++i)
        if (mode_helpers[i].mode == mode)
            return mode_helpers[i].destroy_map;
    return NULL;
}

static port_setter
cr_port_initfnc(enum cr_port_modes mode)
{
    int i;

    for (i = 0; mode_helpers[i].mode != CR_MODE_INVALID; ++i)
        if (mode_helpers[i].mode == mode)
            return mode_helpers[i].init_port;
    return NULL;
}

struct cr_port *
cr_new_port(struct cr_line *lines)
{
    struct cr_port *new;
    int i;

    new = xmalloc(1, struct cr_port);
    if (new == NULL)
        return NULL;
    new->l = lines;
    for (i = 0; lines[i].access != NULL; ++i)
        /* NOP */;
    new->lines = i;
    cr_int_prop_set(&(new->rate), -1, CR_IMMUTABLE);
    new->mode.mutable_p = CR_IMMUTABLE;
    new->mode.mode = CR_MODE_NONE;
    new->params = NULL;
    new->transmit = NULL;

    return new;
}

static int
cr_params_for_mode(struct cr_port *port, enum cr_port_modes mode)
{
    port_setter setter;

    setter = cr_params_setter(mode);
    if (setter == NULL)
        return 1;
    return setter(port);
}

static int
cr_new_port_map(struct cr_port *port, enum cr_port_modes mode)
{
    port_setter setter;

    setter = cr_map_setter(mode);
    if (setter == NULL)
        return 1;
    return setter(port);
}

static void
cr_destroy_port_map(struct cr_port *port)
{
    port_setter destroy;

    destroy = cr_map_destroy(port->mode.mode);
    if (destroy == NULL)
        return;
    destroy(port);
}

/**
 * Set up a new mode for a port
 *
 * If a port has a configurable mode, this function is responsible for setting
 * up the following mode-specific values:
 *
 *   - The list of mode-specific parameters
 *
 *   - The line-map
 *
 *   - The transmission function
 *
 * The parameter-list and and line-map take dynamic memory, so it needs to be
 * destroyed and freshly allocated.
 *
 * The return value encodes an error-condition:
 *
 *   0: no error
 *   1: Port mode is not configurable
 *   2: out-of-memory condition
 *   4: mode configuration did not assign a tranmission function
 *
 * @param  port   the port to modify
 *
 * @return integer, see description for details.
 */
int
cr_port_mode_set(struct cr_port *port, enum cr_port_modes mode)
{
    int rc;

    if (!port->mode.mutable_p)
        return INT_BIT(0);

    rc = 0;
    cr_destroy_port_map(port);
    port->mode.mode = mode;
    cr_destroy_params(port);
    PORT_MARK_NOT_INITIALISED(port);

    if (cr_params_for_mode(port, mode) < 0) {
        rc = INT_BIT(1);
        goto error;
    }

    if (cr_new_port_map(port, mode) < 0) {
        rc = INT_BIT(1);
        goto error;
    }

    port->transmit = cr_port_transmitter(mode);
    if (port->mode.mode != CR_MODE_NONE && port->transmit == NULL) {
        rc = INT_BIT(2);
        goto error;
    }

    cr_port_reset_line_roles(port);
    return rc;

error:
    cr_destroy_params(port);
    cr_destroy_port_map(port);
    cr_port_reset_line_roles(port);
    port->mode.mode = CR_MODE_NONE;
    return rc;
}

void
cr_init_port(struct cr_port *port)
{
    enum cr_port_modes mode;
    port_setter initfnc;

    mode = port->mode.mode;
    if (mode == CR_MODE_NONE) {
        cr_fail("Focused port is unconfigured.");
        return;
    }
    if (mode == CR_MODE_INVALID) {
        cr_fail("Focused port is invalid. [BUG?]");
        return;
    }
    initfnc = cr_port_initfnc(port->mode.mode);
    if (initfnc == NULL) {
        cr_fail("Port has no init function. [BUG?]");
        return;
    }
    if (!initfnc(port)) {
        cr_fail("Initialisation failed!");
        return;
    }
    PORT_MARK_INITIALISED(port);
    xcr_send_host(OK_REPLY);
}
