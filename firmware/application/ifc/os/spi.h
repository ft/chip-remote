/*
 * Copyright (c) 2022 Frank Terbeck <ft@bewatermyfriend.org>
 * All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_CR_FW_IFC_OS_SPI_H
#define INC_CR_FW_IFC_OS_SPI_H

#include <zephyr/device.h>
#include <zephyr/drivers/spi.h>

#include <cr-port.h>

/*
 * What's up with the two configuration parameters? Well, zephyr's SPI sub
 * system has APIs to which you pass a configuration pointer. In order to
 * minimise the number of reconfigurations, the OS allows its bus driver to
 * compare configuration structure pointers and only reconfigure in case this
 * address changes.
 *
 * The system caters to device drivers using a bus together, where the idea is
 * that different device drivers have their own configuration structures. So if
 * different drivers use the same bus, it gets reconfigured on the fly like
 * this.
 *
 * Now, chip-remote is different: We are basically a bridge from interfaces
 * commonly found on personal computers to these low-level busses. Which means
 * that actually we'd like to explicitly control the busses parameters. We do
 * this by ways of the ‘set’ command of the protocol.
 *
 * To reconfigure a bus, users need to use the ’init’ command after issueing a
 * call to ‘set’. What we do is this: Each time we reconfigure, we switch to a
 * different configuration parameter. That way different OS API uses see a
 * different configuration pointer each time the configuration changes, which
 * should make the SPI sub system reconfigure the bus implicitly very time it's
 * required.
 */

struct cr_port_spi_os {
    const struct device *bus;
    bool api_used;
    struct {
        int mux;
        struct spi_config a;
        struct spi_config b;
    } cfg;
};

#define CR_PORT_SPI_OS_INIT(dev)                                        \
    ((struct cr_port_spi_os) { .api_used = true,                        \
                               .bus = dev,                              \
                               .cfg.mux = 0 })

extern struct cr_port_api cr_port_impl_spi_os;

#endif /* INC_CR_FW_IFC_OS_SPI_H */
