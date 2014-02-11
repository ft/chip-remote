/*
 * Copyright (c) 2013-2014 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_PROPERTIES_H
#define INC_PROPERTIES_H

#include "chip-remote.h"

void cr_int_prop_set(struct cr_int_prop *, int, int);
void cr_string_prop_set(struct cr_string_prop *, char *, int);
void cr_string_prop_set_n(struct cr_string_prop *, char *, size_t, int);

#define CR_MARK_PROP(p, m)                      \
    do {                                        \
        (p)->mutable_p = m;                     \
    } while (0)

#endif /* INC_PROPERTIES_H */
