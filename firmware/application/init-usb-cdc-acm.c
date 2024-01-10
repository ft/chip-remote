#include <zephyr/kernel.h>

#include <zephyr/drivers/gpio.h>
#include <zephyr/drivers/uart.h>
#include <zephyr/usb/usb_device.h>

#include <ufw/compiler.h>
#include <ufw/endpoints.h>
#include <ufw/register-protocol.h>
#include <ufwz/endpoint-uart-poll.h>

#include "chip-remote.h"
#include "peripherals.h"
#include "registers.h"

#define LED0_NODE DT_ALIAS(led0)
static const struct gpio_dt_spec led0 = GPIO_DT_SPEC_GET(LED0_NODE, gpios);

static void
led0_heartbeat(UNUSED void *p1, UNUSED void *p2, UNUSED void *p3)
{
    if (device_is_ready(led0.port) == false) {
        printk("Could not access heartbeat LED.\n");
        return;
    }

    int ret = gpio_pin_configure_dt(&led0, GPIO_OUTPUT_ACTIVE);
    if (ret < 0) {
        printk("Could not configure LED pin.\n");
        return;
    }

    enum { ON, OFF } state = OFF;
    const unsigned int offtimes[] = { 50, 900 };
    const unsigned int ontime = 100;
    unsigned int n = 0u;

    for (;;) {
        switch (state) {
        case OFF:
            k_msleep(offtimes[n%2u]);
            gpio_pin_set_dt(&led0, 1);
            state = ON;
            break;
        case ON:
            k_msleep(ontime);
            gpio_pin_set_dt(&led0, 0);
            state = OFF;
            break;
        }
    }
}

#define LED0_HEARTBEAT_STACKSIZE 128
#define LED0_HEARTBEAT_PRIORITY   12

K_THREAD_DEFINE(led0_headerbeat_thread,
                LED0_HEARTBEAT_STACKSIZE,
                led0_heartbeat, NULL, NULL, NULL,
                LED0_HEARTBEAT_PRIORITY, 0, 0);

void
main(void)
{
    const struct device *uart0 = DEVICE_DT_GET(DT_NODELABEL(cdc_acm_uart0));
    if (uart0 == NULL) {
        printk("Could not access usb. Giving up.\n");
        return;
    }

    if (usb_enable(NULL) != 0) {
        printk("Could not enable usb. Giving up.\n");
        return EXIT_FAILURE;
    }

    if (peripheral_check() < 0) {
        return;
    }

    RegP protocol;
    Source regpsource = UFWZ_UART_POLL_SOURCE(uart0);
    Sink regpsink = UFWZ_UART_POLL_SINK(uart0);

    if (chip_remote_init(&protocol, regpsource, regpsink, &registers) < 0) {
        return;
    }

    printk("ChipRemoteFirmware running on %s\n", CONFIG_BOARD);

    for (;;) {
        const int rc = chip_remote_process(&protocol);
        if (rc < 0) {
            k_usleep(1000);
        }
    }
}
