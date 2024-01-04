#ifndef INC_INIT_COMMON_H
#define INC_INIT_COMMON_H

#include <zephyr/device.h>

extern const struct device *uart0;
void uart_sink(const char*);

#endif /* INC_INIT_COMMON_H */
