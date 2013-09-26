#include <stdint.h>

#include "chip-remote.h"
#include "protocol.h"
#include "transmit.h"

int
cr_transmit(uint32_t value, uint32_t *retval)
{
    struct cr_port *cp;
    int fp;

    fp = cr_get_focused_port();
    if (fp < 0) {
        cr_fail("No port has focus.");
        return -1;
    }
    cp = &(cr_ports[fp]);
    if (cp->mode.mode == CR_MODE_NONE) {
        cr_fail("Focused port is unconfigured.");
        return -1;
    }
    if (cp->mode.mode == CR_MODE_INVALID) {
        cr_fail("Focused port is invalid. [BUG?]");
        return -1;
    }
    if (cp->transmit == NULL) {
        cr_fail("Port has no transmission function. [BUG?]");
        return -1;
    }
    *retval = cp->transmit(cp, value);
    return 0;
}
