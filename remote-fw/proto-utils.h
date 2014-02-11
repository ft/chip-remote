/*
 * Copyright (c) 2013-2014 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_PROTO_UTILS_H
#define INC_PROTO_UTILS_H

#include "chip-remote.h"

void tx_init(void);
void tx_add_n(char *, size_t);
void tx_add(char *);
void tx_add_space(void);
void tx_add_integer(uint32_t);
void tx_add_word(struct cr_words *, size_t);
void tx_trigger(void);

#endif /* INC_PROTO_UTILS_H */
