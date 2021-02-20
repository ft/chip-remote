#include <device.h>
#include <kernel.h>

#include <drivers/uart.h>

#include <string.h>

#include <chip-remote.h>
#include <cr-process.h>
#include <parse-string.h>

#include "init-common.h"

#ifndef CONFIG_ARCH_POSIX
#include "bitbang-spi.h"
#endif /* CONFIG_ARCH_POSIX */

#define CR_QUEUE_SIZE 16u
K_MSGQ_DEFINE(cr_charqueue, sizeof(char), CR_QUEUE_SIZE, 4u);

#define CR_STACK_SIZE 512u
#define CR_PRIORITY 5u

char cr_input[CR_MAX_LINE_SIZE];

const struct device *uart0;

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

#ifdef CONFIG_ARCH_POSIX

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

#else

int
spi_transmit(const struct cr_port *port, const uint32_t tx, uint32_t *rx)
{
    printk("cr>> 0x%08x\n", tx);
    *rx = cr_spi_xfer(&bbspi, tx);
    printk("cr<< 0x%08x\n", *rx);
    return 0;
}

#endif /* CONFIG_ARCH_POSIX */

void
cr_run(void *a, void *b, void *c)
{
    struct cr_protocol proto;
    char ch = 0;

    printk("ChipRemote Command Processor online!\n");

#ifdef CONFIG_ARCH_POSIX
    cr_process_init(&proto, cr_input, CR_MAX_LINE_SIZE,
                    text_transmit, uart_sink);
#else
    cr_spi_init(&bbspi);
    cr_process_init(&proto, cr_input, CR_MAX_LINE_SIZE,
                    spi_transmit, uart_sink);
#endif /* CONFIG_ARCH_POSIX */
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
