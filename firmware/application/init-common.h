#ifndef INC_INIT_COMMON_H
#define INC_INIT_COMMON_H

#include <device.h>
#include <kernel.h>

#define CR_MAX_LINE_SIZE 64u

extern char cr_input[CR_MAX_LINE_SIZE];
extern const struct device *uart0;
extern struct cr_protocol proto;
extern struct cr_port port00_spi;
#ifdef CONFIG_SPI
extern struct cr_port port01_spi;
#endif /* CONFIG_SPI */
#ifdef CONFIG_I2C
extern struct cr_port port02_i2c;
#endif /* CONFIG_SPI */

void uart_sink(const char*);

#endif /* INC_INIT_COMMON_H */
