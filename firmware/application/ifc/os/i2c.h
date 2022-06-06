/*
 * Copyright (c) 2022 Frank Terbeck <ft@bewatermyfriend.org>
 * All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_CR_FW_IFC_OS_I2C_H
#define INC_CR_FW_IFC_OS_I2C_H

#include <zephyr/device.h>
#include <zephyr/drivers/i2c.h>

#include <cr-port.h>

struct cr_port_i2c_os {
    const struct device *bus;
};

extern struct cr_port_api cr_port_impl_i2c_os;

#endif /* INC_CR_FW_IFC_OS_I2C_H */
