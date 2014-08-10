/*
 * Copyright (c) 2014 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file par-ex.c
 * @brief Parallel-Exchange interface API
 */

#ifndef INC_PAR_EX_H
#define INC_PAR_EX_H

#include "chip-remote.h"
#include <stdint.h>

#define CR_T_PAREX_HALFPERIOD 5

uint32_t cr_parex_transmit(struct cr_port *, uint32_t);
int cr_parex_map(struct cr_port *);
int cr_parex_destroy_map(struct cr_port *);
int cr_parex_params(struct cr_port *);
int cr_parex_init(struct cr_port *);

#endif /* INC_PAR_EX_H */
