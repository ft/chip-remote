#include <stdint.h>

#include <device.h>
#include <kernel.h>

#include <drivers/gpio.h>

#include <common/bit-operations.h>

#include "bitbang-spi.h"

struct bitbang_spi bbspi = {
    .port = DEVICE_DT_GET(DT_NODELABEL(gpioc)),
    .cs   =  8u,
    .clk  =  9u,
    .mosi = 10u,
    .miso = 11u
};

void
cr_spi_init(const struct bitbang_spi *spi)
{
    gpio_pin_configure(spi->port, spi->clk,  GPIO_OUTPUT_ACTIVE);
    gpio_pin_configure(spi->port, spi->cs,   GPIO_OUTPUT_ACTIVE);
    gpio_pin_configure(spi->port, spi->mosi, GPIO_OUTPUT_ACTIVE);
    gpio_pin_configure(spi->port, spi->miso, GPIO_INPUT);
}

static inline void
cr_spi_cs_set(const struct bitbang_spi *spi, int value)
{
    gpio_pin_set(spi->port, spi->cs, value);
}

static inline void
cr_spi_clk_set(const struct bitbang_spi *spi, int value)
{
    gpio_pin_set(spi->port, spi->clk, value);
}

static inline void
cr_spi_mosi_set(const struct bitbang_spi *spi, int value)
{
    gpio_pin_set(spi->port, spi->mosi, value);
}

static inline uint32_t
cr_spi_miso_get(const struct bitbang_spi *spi)
{
    return gpio_pin_get(spi->port, spi->miso);
}

uint32_t
cr_spi_xfer(const struct bitbang_spi *spi, const uint32_t word)
{
    uint32_t rv = 0ull;
    const size_t len = 16;

    cr_spi_cs_set(spi, 0u);
    for (size_t i = 0ull; i < len; ++i) {
        const size_t idx = len - i - 1;
        cr_spi_mosi_set(spi, BITLL_GET(word, 1u, idx));
        cr_spi_clk_set(spi, 1u);
        if (cr_spi_miso_get(spi) > 0) {
            rv |= BITLL(idx);
        }
        cr_spi_clk_set(spi, 0u);
    }
    cr_spi_cs_set(spi, 1u);

    return rv;
}
