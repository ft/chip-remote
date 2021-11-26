#include "chip-remote.h"
#include "cr-port.h"

int
cr_transmit(struct cr_port *p, cr_number tx, cr_number *rx)
{
    return p->api->xfer(p, tx, rx);
}
