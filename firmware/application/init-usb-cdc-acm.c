#include <device.h>
#include <kernel.h>

#include <drivers/gpio.h>
#include <drivers/uart.h>
#include <usb/usb_device.h>

#include <common/compiler.h>

#include <cr-port.h>
#include <cr-process.h>

#include "init-common.h"
#include "ifc/bb/spi.h"

#define P_GPIOC DEVICE_DT_GET(DT_NODELABEL(gpioc))

struct cr_line port00_lines[] = {
    { .port = P_GPIOC, .pin =  8u, .mode = CR_LINE_OUTPUT_PUSHPULL },
    { .port = P_GPIOC, .pin =  9u, .mode = CR_LINE_OUTPUT_PUSHPULL },
    { .port = P_GPIOC, .pin = 10u, .mode = CR_LINE_OUTPUT_PUSHPULL },
    { .port = P_GPIOC, .pin = 11u, .mode = CR_LINE_INPUT_PULLDOWN }
};

struct cr_port_spi_bb port00_spi_bb = {
    .cs   = &port00_lines[0],
    .clk  = &port00_lines[1],
    .mosi = &port00_lines[2],
    .miso = &port00_lines[3]
};

struct cr_port port00_spi = {
    .name = "port00-spi",
    .type = CR_PORT_TYPE_SPI,
    .api  = &cr_port_impl_spi_bb,
    .data = &port00_spi_bb,
    .cfg.spi = {
        .address = 0u,
        .frame_length = 16u,
        .bit_order = CR_BIT_MSB_FIRST,
        .cs = {
            .number = 1u,
            .polarity = CR_LOGIC_INVERTED
        },
        .clk = {
            .rate = 0u,
            .edge = CR_EDGE_RISING,
            .phase_delay = false
        }
    },
    .lines = sizeof(port00_lines)/sizeof(*port00_lines),
    .line = port00_lines,
    .initialised = false
};

void 
uart_sink(const char *str)
{
    const size_t len = strlen(str);
    uart_fifo_fill(uart0, str, len);
}

static void
cr_handle_usb(const struct device *dev, UNUSED void *userdata)
{
    char ch;
    while (uart_fifo_read(dev, &ch, 1u) > 0u)
        cr_toplevel(&proto, ch);
}

#define LED0_NODE DT_ALIAS(led0)
#define LED0 DT_GPIO_LABEL(LED0_NODE, gpios)
#define PIN DT_GPIO_PIN(LED0_NODE, gpios)

void
main(void)
{
    uart0 = device_get_binding("CDC_ACM_0");
    if (uart0 == NULL) {
        printk("Could not access usb. Giving up.\n");
        return;
    }

    if (usb_enable(NULL) != 0) {
        printk("Could not enable usb. Giving up.\n");
        return;
    }

    printk("Registering usb callback.\n");
    uart_irq_callback_set(uart0, cr_handle_usb);
    printk("Enabling usb rx interrupt.\n");
    uart_irq_rx_enable(uart0);

    const struct device *led = device_get_binding(LED0);
    if (led == NULL) {
        printk("Could not access LED.\n");
        return;
    }

    int ret = gpio_pin_configure(led, PIN, GPIO_OUTPUT_ACTIVE);
    if (ret < 0) {
        return;
    }

    cr_protocol_boot(&proto);
    printk("ChipRemote Command Processor online!\n");

    bool led_is_on = true;
    uint32_t cnt = 0ul;
    for (;;) {
#if 0
        if ((cnt % 10u) == 0u)
            printk("Still here.\n");
#endif
        cnt += 1u;
        gpio_pin_set(led, PIN, (int)led_is_on);
        led_is_on = !led_is_on;
        k_msleep(100);
    }
}
