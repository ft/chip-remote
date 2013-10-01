#ifndef INC_PORT_H
#define INC_PORT_H

#include "chip-remote.h"

struct cr_port *cr_new_port(struct cr_line *);
int cr_port_mode_set(struct cr_port *, enum cr_port_modes);
void cr_init_port(struct cr_port *);

#endif /* INC_PORT_H */
