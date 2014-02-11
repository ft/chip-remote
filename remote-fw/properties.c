/*
 * Copyright (c) 2013-2014 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <string.h>
#include "chip-remote.h"
#include "properties.h"
#include "utils.h"

void
cr_int_prop_set(struct cr_int_prop *p, int value, int mutable)
{
    p->value = value;
    p->mutable_p = mutable;
}

void
cr_string_prop_set(struct cr_string_prop *p, char *value, int mutable)
{
    strncpy(p->value, value, CR_STRING_PROP_MAX + 1);
    p->value[CR_STRING_PROP_MAX] = '\0';
    p->mutable_p = mutable;
}

void
cr_string_prop_set_n(struct cr_string_prop *p, char *v, size_t n, int mutable)
{
    if (n > CR_STRING_PROP_MAX)
        cr_string_prop_set(p, v, mutable);
    else {
        strncpy(p->value, v, n);
        p->value[n] = '\0';
        p->mutable_p = mutable;
    }
}
