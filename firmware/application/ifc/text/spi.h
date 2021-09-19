/*
 * Copyright (c) 2021 Frank Terbeck <ft@bewatermyfriend.org>
 * All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_CR_FW_IFC_BB_SPI_H
#define INC_CR_FW_IFC_BB_SPI_H

#include <cr-port.h>
#include <sx-parser.h>

extern struct cr_port_api cr_port_impl_spi_text;
void cr_spi_text_load(struct sx_node*);

#endif /* INC_CR_FW_IFC_BB_SPI_H */
