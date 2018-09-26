/*
 * Copyright (c) 2013-2018 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <stdint.h>

#include "chip-remote.h"

#include "line.h"
#include "utils.h"

struct cr_line *
cr_get_line(struct cr_port *port, char *role)
{
    int i;

    for (i = 0; i < port->lines; ++i)
        if (STREQ(port->l[i].rolestr, role))
            return port->l + i;

    return NULL;
}

struct cr_line *
cr_get_indexed_line(struct cr_port *port, char *role, int idx)
{
    struct cr_line *l;
    char nbuf[CR_INT_MAX_LEN + 1];
    /* role + ':' + '\0' */
    char buf[CR_MAX_ROLE_STRING + 1 + CR_INT_MAX_LEN + 1];

    uint2str((uint32_t)idx, nbuf);
    strncpy(buf, role, CR_MAX_ROLE_STRING);
    strncat(buf, ":", 1);
    strncat(buf, nbuf, CR_INT_MAX_LEN);

    l = cr_get_line(port, buf);
    if (l == NULL && idx == 0)
        return cr_get_line(port, role);

    return l;
}
