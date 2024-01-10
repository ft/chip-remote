/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <zephyr/device.h>
#include <zephyr/drivers/spi.h>

#include <stddef.h>
#include <stdint.h>

#include <ufw/binary-format.h>
#include <ufw/register-protocol.h>
#include <ufw/register-table.h>

#include "registers.h"
#include "peripherals.h"

static inline uint32_t
nextpow2(uint32_t n)
{
    if (n < 8u) {
        return 8u;
    }

    n--;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    n++;

    return n;
}

static void
update_u32(const RegisterHandle r, const uint32_t value)
{
    register_set_unsafe(
        &registers, r,
        (RegisterValue) {
            .type = REG_TYPE_UINT32,
            .value.u32 = value });
}

#define PERIPH_COMMAND_INIT     0ull
#define PERIPH_COMMAND_TRANSMIT 1ull

#define PSTATUS_SUCCESS          0ul
#define PSTATUS_ARG_OUT_OF_RANGE 1ul
#define PSTATUS_INVALID_COMMAND  UINT32_MAX

struct peripheral_api {
    void (*init)(struct peripheral_control*);
    void (*transmit)(struct peripheral_control*);
};

static void papi_spi_init(struct peripheral_control *ctrl);
static void papi_spi_transmit(struct peripheral_control *ctrl);

struct peripheral_api papi[] = {
    [PERIPH_TYPE_SPI] = {
        .init = papi_spi_init,
        .transmit = papi_spi_transmit
    }
};

static void
papi_spi_init(struct peripheral_control *ctrl)
{
    struct peripheral_spi *spi = &ctrl->backend.spi;

    if (spi->cfg == NULL) {
        memcpy(&spi->cfg_b, &spi->cfg_a, sizeof(struct spi_config));
        spi->cfg = &spi->cfg_b;
        spi->flags = 0u;
    }

    if (ctrl->backend.spi.flags & 1u) {
        spi->cfg = (spi->cfg == &spi->cfg_b) ? &spi->cfg_a : &spi->cfg_b;
        spi->flags &= ~(1u);
    }

    RegisterValue rate, vflags, flen;
    register_get(&registers, spi->ctrl.clockrate, &rate);
    register_get(&registers, spi->ctrl.framelength, &flen);
    register_get(&registers, spi->ctrl.flags, &vflags);
    const uint16_t fl = flen.value.u16 & 0x3fu;
    const uint32_t flags = vflags.value.u32;
    spi->cfg->frequency = rate.value.u32;
    spi->cfg->operation =
        ( SPI_WORD_SET(fl)
        | (flags & 1u ? 0u               : SPI_CS_ACTIVE_HIGH)
        | (flags & 2u ? SPI_TRANSFER_MSB : SPI_TRANSFER_LSB)
        | (flags & 4u ? 0u               : SPI_MODE_CPHA)
        | (flags & 8u ? SPI_MODE_CPOL    : 0u));

    update_u32(ctrl->cmdstatus, PSTATUS_SUCCESS);
}

static void
papi_spi_transmit(struct peripheral_control *ctrl)
{
    struct peripheral_spi *spi = &ctrl->backend.spi;

    if (spi->cfg == NULL) {
        papi_spi_init(ctrl);
    }

    RegisterValue arg, flen, fbsize;
    register_get(&registers, ctrl->cmdarg, &arg);
    register_get(&registers, spi->ctrl.framelength, &flen);
    register_get(&registers, ctrl->fbsize, &fbsize);

    const size_t wsize = nextpow2(flen.value.u16) / 8u;
    const size_t maxframes = fbsize.value.u32 / wsize / 2u;

    printk("fbsize: %"PRIu32", wsize: %zu, maxframes: %zu\n",
           fbsize.value.u16, wsize, maxframes);

    if (arg.value.u32 > maxframes) {
        update_u32(ctrl->cmdstatus, PSTATUS_ARG_OUT_OF_RANGE);
        return;
    }

    unsigned char *fbtx = (void*)registers.area[REG_AREA_FB].mem;
    unsigned char *fbrx = fbtx + wsize * arg.value.u32;

    for (size_t i = 0u; i < arg.value.u32; ++i) {
        struct spi_buf tx_buf[] = {{ .buf = fbtx + i * wsize, .len = wsize }};
        struct spi_buf rx_buf[] = {{ .buf = fbrx + i * wsize, .len = wsize }};
        struct spi_buf_set tx = { .buffers = tx_buf, .count = 1 };
        struct spi_buf_set rx = { .buffers = rx_buf, .count = 1 };
        spi_transceive(ctrl->dev, spi->cfg, &tx, &rx);
    }

    update_u32(ctrl->cmdstatus, PSTATUS_SUCCESS);
}

#define PAPI(var, ucmd, lcmd)                   \
    (cmd == PERIPH_COMMAND_##ucmd               \
  && papi[var->type].lcmd != NULL)

void
process_command(RegisterTable *t,
                struct peripheral_control *ctrl,
                const RPFrame *f)
{
    const uint32_t cmd = bf_ref_u16b(f->payload.data);
    printk("Got spi command: %u\n", cmd);
    if (PAPI(ctrl, INIT, init)) {
        papi[ctrl->type].init(ctrl);
    } else if (PAPI(ctrl, TRANSMIT, transmit)) {
        papi[ctrl->type].transmit(ctrl);
    } else {
        update_u32(ctrl->cmdstatus, PSTATUS_INVALID_COMMAND);
    }
}

int
peripheral_check(void)
{
    int rc = 0;

    struct peripheral_control **pc = periph_ctrl;
    for (size_t i = 0u; pc[i] != NULL; ++i) {
        if (pc[i]->dev == NULL) {
            printk("Could not access %s. Giving up.\n", pc[i]->dev->name);
            rc = -EINVAL;
        }
    }

    return rc;
}

#undef PAPI

/*
 * Generate list of actively exposed peripherals
 */

#define MAKE_SPI_CTRL(ID)                                       \
    struct peripheral_control spi##ID##_ctrl = {                \
        .type = PERIPH_TYPE_SPI,                                \
        .dev = DEVICE_DT_GET(DT_CHOSEN(chipremote_spi##ID)),    \
        .backend.spi.ctrl = {                                   \
            .framelength = R_SPI##ID##_FLEN,                    \
            .clockrate   = R_SPI##ID##_RATE,                    \
            .flags       = R_SPI##ID##_FLAGS,                   \
        },                                                      \
        .backend.spi.flags = 0u,                                \
        .backend.spi.cfg = NULL,                                \
        .backend.spi.cfg_a = {                                  \
            .frequency = 1000000ul,                             \
            .operation = (  SPI_OP_MODE_MASTER                  \
                          | SPI_TRANSFER_MSB                    \
                          | SPI_WORD_SET(8))                    \
        },                                                      \
        .fbsize      = R_SPI##ID##_FBSIZE,                      \
        .fbaddr      = R_SPI##ID##_FBADDR,                      \
        .cmd         = R_SPI##ID##_CMD,                         \
        .cmdarg      = R_SPI##ID##_CMDARG,                      \
        .cmdstatus   = R_SPI##ID##_STATUS                       \
    }

#ifdef CR_WITH_SPI_0
MAKE_SPI_CTRL(0);
#endif /* CR_WITH_SPI_0 */

#ifdef CR_WITH_SPI_1
MAKE_SPI_CTRL(1);
#endif /* CR_WITH_SPI_1 */

struct peripheral_control *periph_ctrl[] = {
#ifdef CR_WITH_SPI_0
    &spi0_ctrl,
#endif /* CR_WITH_SPI_0 */
#ifdef CR_WITH_SPI_1
    &spi1_ctrl,
#endif /* CR_WITH_SPI_1 */
    NULL
};
