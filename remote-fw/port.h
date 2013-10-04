#ifndef INC_PORT_H
#define INC_PORT_H

#include "chip-remote.h"
#include "utils.h"

#define PORT_STATUS_BIT_INITIALISED 0

#define PORT_INITIALISED_P(p)                   \
    INT_BIT_SET_P((p)->status, PORT_STATUS_BIT_INITIALISED)
#define PORT_MARK_INITIALISED(p)                \
    INT_BIT_SET((p)->status, PORT_STATUS_BIT_INITIALISED)
#define PORT_MARK_NOT_INITIALISED(p)                    \
    INT_BIT_CLEAR((p)->status, PORT_STATUS_BIT_INITIALISED)

struct cr_port *cr_new_port(struct cr_line *);
int cr_port_mode_set(struct cr_port *, enum cr_port_modes);
void cr_init_port(struct cr_port *);

#endif /* INC_PORT_H */
