/**
 * @file bitbang-spi.h
 * @brief API for simple bit banged SPI bus
 */

#ifndef INC_BITBANG_SPI_H
#define INC_BITBANG_SPI_H

#include <stdint.h>

struct bitbang_spi {
    const struct device *port;
    int cs;
    int clk;
    int mosi;
    int miso;
};

extern struct bitbang_spi bbspi;

void cr_spi_init(const struct bitbang_spi*);
uint32_t cr_spi_xfer(const struct bitbang_spi*, uint32_t);

#endif /* INC_BITBANG_SPI_H */
