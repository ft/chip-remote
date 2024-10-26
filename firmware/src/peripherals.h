/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_PERIPHERALS_H_77e44a18
#define INC_PERIPHERALS_H_77e44a18

#include <zephyr/drivers/spi.h>

#include <stdint.h>

#include <ufw/register-protocol.h>

#include "registers.h"

enum peripheral_type {
    PERIPH_TYPE_SPI = 0,
    PERIPH_TYPE_I2C
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

struct i2c_control {
    FirmwareRegister config;
    FirmwareRegister address;
};

struct peripheral_i2c {
    struct i2c_control ctrl;
};

struct peripheral_control {
    enum peripheral_type type;
    const struct device *dev;
    union {
        struct peripheral_spi spi;
        struct peripheral_i2c i2c;
    } backend;
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
