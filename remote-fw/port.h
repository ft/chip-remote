#ifndef INC_PORT_H
#define INC_PORT_H

#include <stdlib.h>
#include "chip-remote.h"
#include "platform.h"
#include "spi.h"

#define CR_NO_INDEX -1

extern struct cr_port cr_ports[];
size_t cr_numofports(struct cr_port *);

#endif /* INC_PORT_H */
