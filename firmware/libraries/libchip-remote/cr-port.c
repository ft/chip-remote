#include "cr-port.h"

int
cr_transmit(struct cr_port *p, uint32_t tx, uint32_t *rx)
{
    return p->api->xfer(p, tx, rx);
}
