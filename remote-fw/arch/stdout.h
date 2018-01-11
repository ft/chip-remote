/*
 * Copyright (c) 2013-2018 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_STDOUT_H
#define INC_STDOUT_H

#include "../config.sim.h"

int access_portA(struct cr_line *, enum cr_access_mode, int);
void dir_portA(struct cr_line *, enum cr_access_mode);
int access_portB(struct cr_line *, enum cr_access_mode, int);
void dir_portB(struct cr_line *, enum cr_access_mode);

#endif /* INC_STDOUT_H */
