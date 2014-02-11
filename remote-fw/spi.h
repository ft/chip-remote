/*
 * Copyright (c) 2013-2014 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_SPI_H
#define INC_SPI_H

#define SPI_MSB_FIRST 0x0f
#define SPI_LSB_FIRST 0xf0

#include "chip-remote.h"
#include <stdint.h>

#define CR_T_SPI_BIT_DURATION 8
#define CR_T_SPI_CS_SETUP 2

uint32_t cr_spi_transmit(struct cr_port *, uint32_t);
int cr_spi_map(struct cr_port *);
int cr_spi_destroy_map(struct cr_port *);
int cr_spi_params(struct cr_port *);
int cr_spi_init(struct cr_port *);

#endif /* INC_SPI_H */
