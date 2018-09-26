/*
 * Copyright (c) 2013-2018 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <stdint.h>
#include <string.h>

#include "chip-remote.h"
#include "roles.h"

static struct {
    enum cr_pin_role role;
    char *name;
} role_map[] = {
    { CR_ROLE_NONE, "NONE" },
    { CR_ROLE_CLK, "CLK" },
    { CR_ROLE_CS, "CS" },
    { CR_ROLE_SPI_MOSI, "MOSI" },
    { CR_ROLE_SPI_MISO, "MISO" },
    { CR_ROLE_INVALID, NULL }
};

enum cr_pin_role
cr_role2id(char *name)
{
    int i;

    for (i = 0; role_map[i].name != (char*)NULL; ++i)
        if (!strcmp(name, role_map[i].name))
            return role_map[i].role;
    return CR_ROLE_INVALID;
}

char *
cr_id2role(enum cr_pin_role id)
{
    int i;

    for (i = 0; role_map[i].name != (char*)NULL; ++i)
        if (id == role_map[i].role)
            return role_map[i].name;
    return "INVALID";
}
