#include "chip-remote.h"
#include "cr-port.h"
#include "cr-process.h"

void
cr_handle_port_value(struct cr_protocol *proto, int value)
{
    if (value > 0) {
        return;
    }

    if (value == CR_PORTVAL_OK) {
        proto->reply("ok\n");
    } else if (value == CR_PORTVAL_INVALID_NUMBER_OF_ARGS) {
        proto->reply("wtf Invalid number of arguments.\n");
    } else if (value == CR_PORTVAL_INVALID_TYPE_OF_ARG) {
        proto->reply("wtf Invalid type of argument.\n");
    } else if (value == CR_PORTVAL_INTERNAL_ERROR) {
        proto->reply("wtf Internal error. Please report a bug!\n");
    } else if (value == CR_PORTVAL_ERRNO) {
        proto->reply("wtf OS Reported an error!\n");
    } else {
        proto->reply("wtf Unknown error.\n");
    }
}

struct cr_port*
current_port(const struct cr_protocol *proto)
{
    return proto->ports.table[proto->ports.current];
}

struct cr_port*
port_by_index(const struct cr_protocol *proto, unsigned int n)
{
    return (n >= proto->ports.tablesize) ? NULL : proto->ports.table[n];
}
