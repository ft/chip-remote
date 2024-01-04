#include <zephyr/kernel.h>

#include <zephyr/drivers/gpio.h>
#include <zephyr/drivers/uart.h>
#include <zephyr/usb/usb_device.h>

#include <ufw/compiler.h>

#include "init-common.h"

const struct device *uart0;

void
uart_sink(const char *str)
{
    const size_t len = strlen(str);
    uart_fifo_fill(uart0, str, len);
}

#if 0
static void
cr_handle_usb(const struct device *dev, UNUSED void *userdata)
{
    char ch;
    while (uart_fifo_read(dev, &ch, 1u) > 0u)
        cr_toplevel(&proto, ch);
}
#endif

#define LED0_NODE DT_ALIAS(led0)
static const struct gpio_dt_spec led = GPIO_DT_SPEC_GET(LED0_NODE, gpios);

void
main(void)
{
    uart0 = DEVICE_DT_GET_ONE(zephyr_cdc_acm_uart);
    if (uart0 == NULL) {
        printk("Could not access usb. Giving up.\n");
        return;
    }

    if (usb_enable(NULL) != 0) {
        printk("Could not enable usb. Giving up.\n");
        return;
    }

#if 0
    printk("Registering usb callback.\n");
    uart_irq_callback_set(uart0, cr_handle_usb);
    printk("Enabling usb rx interrupt.\n");
    uart_irq_rx_enable(uart0);
#endif

    if (!device_is_ready(led.port)) {
        printk("Could not access LED.\n");
        return;
    }

    int ret = gpio_pin_configure_dt(&led, GPIO_OUTPUT_ACTIVE);
    if (ret < 0) {
        printk("Could not configure LED pin.\n");
        return;
    }

    printk("ChipRemote Command Processor online!\n");

    uint32_t cnt = 0ul;
    for (;;) {
#if 0
        if ((cnt % 10u) == 0u)
            printk("Still here.\n");
#endif
        cnt += 1u;
        gpio_pin_toggle_dt(&led);
        k_msleep(100);
    }
}
