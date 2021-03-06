#ifndef INC_INIT_COMMON_H
#define INC_INIT_COMMON_H

#include <device.h>
#include <kernel.h>

#define CR_MAX_LINE_SIZE 64u

extern char cr_input[CR_MAX_LINE_SIZE];
extern const struct device *uart0;
extern struct cr_protocol proto;
extern struct cr_port port00_spi;

void uart_sink(const char*);

#endif /* INC_INIT_COMMON_H */
