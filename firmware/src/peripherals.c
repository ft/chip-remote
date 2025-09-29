/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <zephyr/device.h>
#include <zephyr/drivers/i2c.h>
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
#define PSTATUS_INVALID_CONFIG   2ul
#define PSTATUS_TX_OVERFLOW      3ul
#define PSTATUS_RX_OVERFLOW      4ul
#define PSTATUS_IO_ERROR         5ul
#define PSTATUS_INVALID_VALUE    6ul
#define PSTATUS_INTERNAL_ERROR   7ul
#define PSTATUS_INVALID_COMMAND  UINT32_MAX

struct peripheral_api {
    void (*init)(struct peripheral_control*);
    void (*transmit)(struct peripheral_control*);
};

static void papi_spi_init(struct peripheral_control *ctrl);
static void papi_i2c_init(struct peripheral_control *ctrl);

static void papi_spi_transmit(struct peripheral_control *ctrl);
static void papi_i2c_transmit(struct peripheral_control *ctrl);

struct peripheral_api papi[] = {
    [PERIPH_TYPE_SPI] = {
        .init = papi_spi_init,
        .transmit = papi_spi_transmit
    },
    [PERIPH_TYPE_I2C] = {
        .init = papi_i2c_init,
        .transmit = papi_i2c_transmit
    }
};

static void
papi_spi_init(struct peripheral_control *ctrl)
{
    struct peripheral_spi *spi = &ctrl->backend.spi;

    if (spi->cfg == NULL) {
        memcpy(&spi->cfg_b, &spi->cfg_a, sizeof(struct spi_config));
        spi->cfg = &spi->cfg_a;
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

    RegisterValue arg, flen;
    register_get(&registers, ctrl->cmdarg, &arg);
    register_get(&registers, spi->ctrl.framelength, &flen);

    const size_t wsize = nextpow2(flen.value.u16) / 8u;
    const size_t maxframes = FBTX_SIZE / wsize;

    if (arg.value.u32 > maxframes) {
        update_u32(ctrl->cmdstatus, PSTATUS_ARG_OUT_OF_RANGE);
        return;
    }

    unsigned char *fbtx = (void*)registers.area[REG_AREA_FBTX].mem;
    unsigned char *fbrx = (void*)registers.area[REG_AREA_FBRX].mem;

    struct spi_buf tx_buf[] = {{ .buf = fbtx, .len = wsize * arg.value.u32 }};
    struct spi_buf rx_buf[] = {{ .buf = fbrx, .len = wsize * arg.value.u32 }};
    struct spi_buf_set tx = { .buffers = tx_buf, .count = 1 };
    struct spi_buf_set rx = { .buffers = rx_buf, .count = 1 };
    const int rc = spi_transceive(ctrl->dev, spi->cfg, &tx, &rx);
    if (rc < 0) {
        update_u32(ctrl->cmdstatus, PSTATUS_INTERNAL_ERROR);
        printk("error: %d %s\n", -rc, strerror(-rc));
    } else {
        ctrl->backend.spi.flags |= 1u;
        update_u32(ctrl->cmdstatus, PSTATUS_SUCCESS);
        printk("ok.\n");
    }
}

#define CR_I2C_SPEED_STANDARD 0u
#define CR_I2C_SPEED_FAST     1u
#define CR_I2C_SPEED_FASTPLUS 2u
#define CR_I2C_SPEED_HIGH     3u
#define CR_I2C_SPEED_ULTRA    4u

static void
papi_i2c_init(struct peripheral_control *ctrl)
{
    RegisterValue cfgreg;
    register_get(&registers, ctrl->backend.i2c.ctrl.config, &cfgreg);
    uint32_t config = I2C_MODE_CONTROLLER;
    switch (cfgreg.value.u16 & 0x07u) {
    case CR_I2C_SPEED_STANDARD:
        config |= I2C_SPEED_SET(I2C_SPEED_STANDARD);
        break;
    case CR_I2C_SPEED_FAST:
        config |= I2C_SPEED_SET(I2C_SPEED_FAST);
        break;
    case CR_I2C_SPEED_FASTPLUS:
        config |= I2C_SPEED_SET(I2C_SPEED_FAST_PLUS);
        break;
    case CR_I2C_SPEED_HIGH:
        config |= I2C_SPEED_SET(I2C_SPEED_HIGH);
        break;
    case CR_I2C_SPEED_ULTRA:
        config |= I2C_SPEED_SET(I2C_SPEED_ULTRA);
        break;
    default:
        update_u32(ctrl->cmdstatus, PSTATUS_INVALID_CONFIG);
        return;
    }
    const int rc = i2c_configure(ctrl->dev, config);
    update_u32(ctrl->cmdstatus,
               (rc < 0)
               ? PSTATUS_INTERNAL_ERROR
               : PSTATUS_SUCCESS);
}

#define PAPI_I2C_CFG_10bit_MASK  0x80u

#define PAPI_I2C_WRITE_MASK    0x80u
#define PAPI_I2C_EXTENDED_MASK 0x40u
#define PAPI_I2C_END_MASK      0x20u
#define PAPI_I2C_LENGTH_MASK   0x1fu

static void
papi_i2c_transmit(struct peripheral_control *ctrl)
{
    struct i2c_msg msg[CONFIG_CR_MAX_I2C_SECTIONS];
    size_t nmsg = 0u;
    size_t pos = 0u;
    size_t txoffset = 0u;
    size_t rxoffset = 0u;

    RegisterValue config;
    register_get(&registers, ctrl->backend.i2c.ctrl.config, &config);
    const bool use10bitaddr = config.value.u16 & PAPI_I2C_CFG_10bit_MASK;

    unsigned char *fbtx = (void*)registers.area[REG_AREA_FBTX].mem;
    unsigned char *fbrx = (void*)registers.area[REG_AREA_FBRX].mem;

    for (;;) {
        const size_t extoff = (fbtx[pos] & PAPI_I2C_EXTENDED_MASK) ? 1u : 0u;
        if (fbtx[pos] & PAPI_I2C_END_MASK) {
            txoffset = pos + extoff + (fbtx[pos] & PAPI_I2C_END_MASK ? 1u : 0u);
            break;
        }
        pos++;
        if (pos >= R_DEFAULT_FBTX_SIZE) {
            update_u32(ctrl->cmdstatus, PSTATUS_TX_OVERFLOW);
            return;
        }
    }

    if (txoffset >= R_DEFAULT_FBTX_SIZE) {
        update_u32(ctrl->cmdstatus, PSTATUS_TX_OVERFLOW);
        return;
    }

    pos = 0u;
    bool prev_was_write = false;
    for (;;) {
        const bool iswrite = fbtx[pos] & PAPI_I2C_WRITE_MASK;
        const bool islast = fbtx[pos] & PAPI_I2C_END_MASK;
        msg[nmsg].flags = iswrite ? I2C_MSG_WRITE : I2C_MSG_READ;
        msg[nmsg].flags |= use10bitaddr ? I2C_MSG_ADDR_10_BITS : 0u;
        msg[nmsg].flags |= islast ? I2C_MSG_STOP : 0u;
        size_t len = fbtx[pos] & PAPI_I2C_LENGTH_MASK;
        if (fbtx[pos] & PAPI_I2C_EXTENDED_MASK) {
            pos++;
            len <<= 8u;
            len |= fbtx[pos];
        }
        msg[nmsg].len = len;
        if (nmsg == 0 || prev_was_write == !iswrite) {
            msg[nmsg].flags |= I2C_MSG_RESTART;
        }
        prev_was_write = iswrite;
        if (iswrite) {
            msg[nmsg].buf = fbtx + txoffset;
            txoffset += len;
        } else {
            msg[nmsg].buf = fbrx + rxoffset;
            rxoffset += len;
        }
        nmsg++;
        if (islast) {
            break;
        }
        pos++;
        if (nmsg >= CONFIG_CR_MAX_I2C_SECTIONS) {
            update_u32(ctrl->cmdstatus, PSTATUS_ARG_OUT_OF_RANGE);
            return;
        }
        if (pos >= R_DEFAULT_FBTX_SIZE || txoffset >= R_DEFAULT_FBTX_SIZE) {
            update_u32(ctrl->cmdstatus, PSTATUS_TX_OVERFLOW);
            return;
        }
        if (rxoffset >= R_DEFAULT_FBRX_SIZE) {
            update_u32(ctrl->cmdstatus, PSTATUS_RX_OVERFLOW);
            return;
        }
    }

    RegisterValue address;
    register_get(&registers, ctrl->backend.i2c.ctrl.address, &address);
    const int rc = i2c_transfer(ctrl->dev, msg, nmsg, address.value.u16);
    if (rc < 0) {
        printk("error: %d %s\n", -rc, strerror(-rc));
    }

    update_u32(ctrl->cmdstatus,
               (rc == 0)       ? PSTATUS_SUCCESS
             : (rc == -EIO)    ? PSTATUS_IO_ERROR
             : (rc == -EINVAL) ? PSTATUS_INVALID_VALUE
             :                   PSTATUS_INTERNAL_ERROR);
}

#define PAPI(var, ucmd, lcmd)                                           \
    (cmd == PERIPH_COMMAND_##ucmd && papi[var->type].lcmd != NULL)

void
process_command(RegisterTable *t,
                struct peripheral_control *ctrl,
                const RPFrame *f)
{
    const uint32_t cmd = bf_ref_u16b(f->payload.data);
    printk("papi: Got command: %u\n", cmd);
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
        if (device_is_ready(pc[i]->dev) == false) {
            printk("Device not ready: s %s. Giving up.\n", pc[i]->dev->name);
            rc = -EINVAL;
        }
    }

    return rc;
}

#undef PAPI

/*
 * Generate list of actively exposed peripherals
 */

#define spi(ID) DT_CHOSEN(chipremote_spi##ID)
#define PAPI_SPI(ID) DEVICE_DT_GET(spi(ID))
#ifdef CONFIG_ARCH_POSIX
#define PAPI_SPI_CS(ID) {}
#else
#define PAPI_SPI_CS(ID) GPIO_DT_SPEC_GET_BY_IDX(spi(ID), cs_gpios, 0)
#endif /* CONFIG_ARCH_POSIX */

#define MAKE_SPI_CTRL(ID)                               \
    struct peripheral_control spi##ID##_ctrl = {        \
        .type = PERIPH_TYPE_SPI,                        \
        .dev = PAPI_SPI(ID),                            \
        .backend.spi.ctrl = {                           \
            .framelength = R_SPI##ID##_FLEN,            \
            .clockrate   = R_SPI##ID##_RATE,            \
            .flags       = R_SPI##ID##_FLAGS,           \
        },                                              \
        .backend.spi.flags = 0u,                        \
        .backend.spi.cfg = NULL,                        \
        .backend.spi.cfg_a = {                          \
            .cs = {                                     \
                .gpio = PAPI_SPI_CS(ID),                \
                .delay = 2u                             \
            },                                          \
            .frequency = 1000000ul,                     \
            .operation = (  SPI_OP_MODE_MASTER          \
                            | SPI_TRANSFER_MSB          \
                            | SPI_WORD_SET(8))          \
        },                                              \
        .cmd         = R_SPI##ID##_CMD,                 \
        .cmdarg      = R_SPI##ID##_CMDARG,              \
        .cmdstatus   = R_SPI##ID##_STATUS               \
    }

#define i2c(ID) DT_CHOSEN(chipremote_i2c##ID)
#define PAPI_I2C(ID) DEVICE_DT_GET(i2c(ID))

#define MAKE_I2C_CTRL(ID)                                       \
    struct peripheral_control i2c##ID##_ctrl = {                \
        .type = PERIPH_TYPE_I2C,                                \
        .dev = PAPI_I2C(ID),                                    \
        .backend.i2c.ctrl = {                                   \
            .address = R_I2C##ID##_CHIP_ADDRESS,                \
            .config  = R_I2C##ID##_CONFIG                       \
        },                                                      \
        .cmd         = R_I2C##ID##_CMD,                         \
        .cmdarg      = R_I2C##ID##_CMDARG,                      \
        .cmdstatus   = R_I2C##ID##_STATUS                       \
    }

#ifdef CR_WITH_SPI_0
MAKE_SPI_CTRL(0);
#endif /* CR_WITH_SPI_0 */

#ifdef CR_WITH_SPI_1
MAKE_SPI_CTRL(1);
#endif /* CR_WITH_SPI_1 */

#ifdef CR_WITH_I2C_0
MAKE_I2C_CTRL(0);
#endif /* CR_WITH_I2C_0 */

#ifdef CR_WITH_I2C_1
MAKE_I2C_CTRL(1);
#endif /* CR_WITH_I2C_1 */

struct peripheral_control *periph_ctrl[] = {
#ifdef CR_WITH_SPI_0
    &spi0_ctrl,
#endif /* CR_WITH_SPI_0 */
#ifdef CR_WITH_SPI_1
    &spi1_ctrl,
#endif /* CR_WITH_SPI_1 */
#ifdef CR_WITH_I2C_0
    &i2c0_ctrl,
#endif /* CR_WITH_I2C_0 */
#ifdef CR_WITH_I2C_1
    &i2c1_ctrl,
#endif /* CR_WITH_I2C_1 */
    NULL
};
