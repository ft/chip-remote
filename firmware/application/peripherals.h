/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_PERIPHERALS_H_77e44a18
#define INC_PERIPHERALS_H_77e44a18

#include <zephyr/drivers/spi.h>

#include <ufw/register-protocol.h>

#include "registers.h"

enum peripheral_type {
    PERIPH_TYPE_SPI = 0
};

struct spi_control {
    FirmwareRegister framelength;
    FirmwareRegister clockrate;
    FirmwareRegister flags;
};

struct peripheral_spi {
    struct spi_control ctrl;
    struct spi_config *cfg;
    struct spi_config cfg_a;
    struct spi_config cfg_b;
    uint32_t flags;
};

struct peripheral_control {
    enum peripheral_type type;
    const struct device *dev;
    union {
        struct peripheral_spi spi;
    } backend;
    FirmwareRegister fbsize;
    FirmwareRegister fbaddr;
    FirmwareRegister cmd;
    FirmwareRegister cmdarg;
    FirmwareRegister cmdstatus;
};

extern struct peripheral_control *periph_ctrl[];

int peripheral_check(void);
void process_command(RegisterTable *t,
                     struct peripheral_control *ctrl,
                     const RPFrame *f);

#endif /* INC_PERIPHERALS_H_77e44a18 */
