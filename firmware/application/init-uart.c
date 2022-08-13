#include <device.h>
#include <kernel.h>

#include <drivers/gpio.h>
#include <drivers/uart.h>

#include <string.h>

#include <ufw/compiler.h>

#include <cr-port.h>
#include <cr-process.h>

#include "init-common.h"
#include "ifc/bb/spi.h"
#include "ifc/os/i2c.h"
#include "ifc/os/spi.h"
#include "sys/time_units.h"

#define P_GPIOC DEVICE_DT_GET(DT_NODELABEL(gpioc))
#define D_SPI1  DEVICE_DT_GET(DT_NODELABEL(spi1))
#define D_I2C1  DEVICE_DT_GET(DT_NODELABEL(i2c1))

struct cr_line port00_lines[] = {
    { .port = P_GPIOC, .pin = 5u, .mode = CR_LINE_OUTPUT_PUSHPULL },
    { .port = P_GPIOC, .pin = 6u, .mode = CR_LINE_OUTPUT_PUSHPULL },
    { .port = P_GPIOC, .pin = 8u, .mode = CR_LINE_OUTPUT_PUSHPULL },
    { .port = P_GPIOC, .pin = 9u, .mode = CR_LINE_INPUT_PULLDOWN }
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

struct cr_port_spi_os spi1data = CR_PORT_SPI_OS_INIT(D_SPI1);

struct cr_port port01_spi = {
    .name = "port01-spi",
    .type = CR_PORT_TYPE_SPI,
    .api  = &cr_port_impl_spi_os,
    .data = &spi1data,
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
    .lines = 0,
    .line = NULL,
    .initialised = false
};

struct cr_port_i2c_os i2c1data = { .bus = D_I2C1 };

struct cr_port port02_i2c = {
    .name = "port02-i2c",
    .type = CR_PORT_TYPE_I2C,
    .api  = &cr_port_impl_i2c_os,
    .data = &i2c1data,
    .lines = 0,
    .line = NULL,
    .initialised = false
};

void
uart_sink(const char *str)
{
    const size_t len = strlen(str);
    for (size_t i = 0; i < len; ++i) {
        uart_poll_out(uart0, str[i]);
    }
}

static void
cr_handle_uart(const struct device *dev, UNUSED void *userdata)
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
    uart0 = device_get_binding("UART_2");
    if (uart0 == NULL) {
        printk("Could not access uart-2. Giving up.\n");
        return;
    }

    printk("Registering uart callback.\n");
    uart_irq_callback_set(uart0, cr_handle_uart);
    printk("Enabling uart rx interrupt.\n");
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

    for (;;) {
        gpio_pin_set(led, PIN, (int)led_is_on);
        led_is_on = !led_is_on;
        k_msleep(100);
    }
}
