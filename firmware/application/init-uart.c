#include <zephyr/kernel.h>

#include <zephyr/drivers/gpio.h>
#include <zephyr/drivers/uart.h>

#include <string.h>
#include <stdlib.h>

#include <ufw/compiler.h>

#include "init-common.h"

const struct device *uart0;

void
uart_sink(const char *str)
{
    const size_t len = strlen(str);
    for (size_t i = 0; i < len; ++i) {
        uart_poll_out(uart0, str[i]);
    }
}

#if 0
static void
cr_handle_uart(const struct device *dev, UNUSED void *userdata)
{
    char ch;
    while (uart_fifo_read(dev, &ch, 1u) > 0u)
        cr_toplevel(&proto, ch);
}
#endif

#define LED0_NODE DT_ALIAS(led0)
static const struct gpio_dt_spec led = GPIO_DT_SPEC_GET(LED0_NODE, gpios);

int
main(void)
{
    uart0 = DEVICE_DT_GET(DT_NODELABEL(usart2));
    if (uart0 == NULL) {
        printk("Could not access uart-2. Giving up.\n");
        return EXIT_FAILURE;
    }

    printk("Registering uart callback.\n");
#if 0
    uart_irq_callback_set(uart0, cr_handle_uart);
    printk("Enabling uart rx interrupt.\n");
    uart_irq_rx_enable(uart0);
#endif

    if (!device_is_ready(led.port)) {
        printk("Could not access LED.\n");
        return EXIT_FAILURE;
    }

    int ret = gpio_pin_configure_dt(&led, GPIO_OUTPUT_ACTIVE);
    if (ret < 0) {
        printk("Could not configure LED pin.\n");
        return EXIT_FAILURE;
    }

    printk("ChipRemote Command Processor online!\n");

    for (;;) {
        gpio_pin_toggle_dt(&led);
        k_msleep(100);
    }

    /* NOTREACHED */
    return EXIT_SUCCESS;
}
