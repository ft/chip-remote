#include "chip-remote.h"
#include "cr-port.h"
#include "cr-process.h"

void
cr_protocol_boot(struct cr_protocol *proto)
{
    for (size_t i = 0u; i < proto->ports.tablesize; ++i) {
        struct cr_port *p = proto->ports.table[i];
        if (p->api != NULL && p->api->boot != NULL) {
            proto->ports.table[i]->api->boot(proto, p);
        }
    }
}
