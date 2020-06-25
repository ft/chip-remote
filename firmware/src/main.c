#include <device.h>
#include <kernel.h>

#include <drivers/gpio.h>
#include <drivers/uart.h>
#if defined(CONFIG_BOARD_NUCLEO_F767ZI)
#include <usb/usb_device.h>
#endif /* CONFIG_BOARD_NUCLEO_F767ZI */

#include <string.h>
#include <c/compat.h>

#include <chip-remote.h>
#include <cr-process.h>
#include <parse-string.h>

#define MAX_BOARD_NAME_LENGTH 32u

#define CR_QUEUE_SIZE 16u
K_MSGQ_DEFINE(cr_charqueue, sizeof(char), CR_QUEUE_SIZE, 4u);

#define CR_STACK_SIZE 512u
#define CR_PRIORITY 5u

#define CR_MAX_LINE_SIZE 64u
char cr_input[CR_MAX_LINE_SIZE];

void
handle_error(struct cr_protocol *proto)
{
    printk("cr: Error in command processing: %d\n", proto->cmd.result);
}

enum cr_proto_state
run_command(struct cr_protocol *proto)
{
    enum cr_proto_state next;

    /* Exit early, if the protocol parser signalled an error in proto */
    if (proto->cmd.result != CR_PROTO_RESULT_OK) {
        handle_error(proto);
        return proto->state.protocol;
    }

    /* Install a couple of shorthands */
    const struct cr_proto_parse *parsed = &proto->cmd.parsed;
    const cr_command_callback cb =
        (proto->state.protocol == CR_PROTO_STATE_MULTILINE)
        ? proto->multiline_cb
        : parsed->cmd->cb;

    /* Perform plausibility checks; exit early if that's required */
    if (proto->state.protocol != proto->cmd.parsed.cmd->state) {
        printk("cr: Command %s expects state %d but %d is current.\n",
               proto->cmd.parsed.cmd->name,
               proto->cmd.parsed.cmd->state,
               proto->state.protocol);
        proto->reply("WTF Wrong state for command to be issued.\n");
        return proto->state.protocol;
    }

    if (cb == NULL) {
        printk("cr: Got NULL callback in command processing.\n");
        printk("cr: This should never happend and is likely a bug.\n");
        proto->reply("WTF NULL Callback. This is a bug!\n");
        return proto->state.protocol;
    }

    /* Actually run the callback picked depending on current protocol state */
    next = cb(proto, parsed->cmd, parsed->args, parsed->argn);

    /* Perform multiline-mode setup in protocol state */
    switch (next) {
    case CR_PROTO_STATE_MULTILINE:
        if (proto->state.protocol == CR_PROTO_STATE_ACTIVE) {
            printk("cr: Protocol state active->multiline\n");
            proto->multiline_cb = parsed->cmd->cb;
        }
        break;
    case CR_PROTO_STATE_ACTIVE:
        if (proto->state.protocol == CR_PROTO_STATE_MULTILINE) {
            printk("cr: Protocol state multiline->active\n");
            proto->multiline_cb = NULL;
        } else if (proto->state.protocol == CR_PROTO_STATE_IDLE) {
            printk("cr: Protocol state idle->active\n");
        }
        break;
    case CR_PROTO_STATE_IDLE:
        if (proto->state.protocol == CR_PROTO_STATE_ACTIVE) {
            printk("cr: Protocol state active->idle\n");
        }
        break;
    default:
        /* Switch statement is exhaustive with enum cr_proto_state */
        break;
    }

    proto->state.protocol = next;
    return proto->state.protocol;
}

int
text_transmit(const struct cr_port *port, const uint32_t tx, uint32_t *rx)
{
    static uint32_t state = 0u;
    printk("cr>> 0x%08x\n", tx);
    printk("cr<< 0x%08x\n", state);
    *rx = state;
    state++;
    return 0;
}

struct device *uart0;
void
uart_sink(const char *str)
{
    const size_t len = strlen(str);
#if defined(CONFIG_BOARD_NATIVE_POSIX)
    for (size_t i = 0u; i < len; ++i) {
        uart_poll_out(uart0, str[i]);
    }
#elif defined(CONFIG_BOARD_NUCLEO_F767ZI)
    uart_fifo_fill(uart0, str, len);
#endif
}

void
cr_run(void *a, void *b, void *c)
{
    struct cr_protocol proto;
    char ch = 0;

    printk("ChipRemote Command Processor online!\n");
    cr_process_init(&proto, cr_input, CR_MAX_LINE_SIZE,
                    text_transmit, uart_sink);
    for (;;) {
        k_msgq_get(&cr_charqueue, &ch, K_FOREVER);
        switch (cr_process_octet(&proto, ch)) {
        case CR_PROCESS_PENDING:
            /* Nothing to do. */
            break;
        case CR_PROCESS_COMMAND:
            proto.state.protocol = run_command(&proto);
            break;
        case CR_PROCESS_INPUT_TOO_LONG:
            printk("cr: Input too long (max: %d); line ignored.\n",
                   CR_MAX_LINE_SIZE);
            break;
        }
    }
}

K_THREAD_DEFINE(cr_run_thread, CR_STACK_SIZE,
                cr_run, NULL, NULL, NULL,
                CR_PRIORITY, 0, 0);

#if defined(CONFIG_BOARD_NATIVE_POSIX)
void
main(void)
{
    uart0 = device_get_binding(DT_LABEL(DT_NODELABEL(uart0)));
    if (uart0 == NULL) {
        printk("Could not access uart0. Giving up.\n");
        return;
    }
    char board[MAX_BOARD_NAME_LENGTH];
    strlcpy(board, CONFIG_BOARD, sizeof(board));
    printk("ChipRemoteFirmware running on %s\n", board);

    char ch = 0;
    for (;;) {
        /* Poll controlling UART port and feed fifo */
        const int rc = uart_poll_in(uart0, &ch);
        if (rc == 0) {
            k_msgq_put(&cr_charqueue, &ch, K_FOREVER);
        } else {
            k_usleep(1000);
        }
    }
}
#elif defined(CONFIG_BOARD_NUCLEO_F767ZI)

static void
cr_handle_usb(struct device *dev)
{
    char ch;
    while (uart_fifo_read(dev, &ch, 1u) > 0u)
        k_msgq_put(&cr_charqueue, &ch, K_NO_WAIT);
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

    struct device *led = device_get_binding(LED0);
    if (led == NULL) {
        printk("Could not access LED.\n");
        return;
    }

    int ret = gpio_pin_configure(led, PIN, GPIO_OUTPUT_ACTIVE);
    if (ret < 0) {
        return;
    }

    bool led_is_on = true;
    for (;;) {
        gpio_pin_set(led, PIN, (int)led_is_on);
        led_is_on = !led_is_on;
        k_msleep(200);
    }
}
#endif /* CONFIG_BOARD_NATIVE_POSIX || CONFIG_BOARD_NUCLEO_F767ZI */
