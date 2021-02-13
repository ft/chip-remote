#ifndef INC_INIT_COMMON_H
#define INC_INIT_COMMON_H

#include <device.h>
#include <kernel.h>

#define CR_MAX_LINE_SIZE 64u

extern char cr_input[CR_MAX_LINE_SIZE];
extern struct k_msgq cr_charqueue;
extern const struct device *uart0;

void uart_sink(const char*);

#endif /* INC_INIT_COMMON_H */
