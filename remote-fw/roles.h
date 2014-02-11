/*
 * Copyright (c) 2013-2014 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_ROLES_H
#define INC_ROLES_H

#include "chip-remote.h"

enum cr_pin_role cr_role2id(char *);
char *cr_id2role(enum cr_pin_role);

#endif /* INC_ROLES_H */
