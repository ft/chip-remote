#include <device.h>
#include <kernel.h>

#include <drivers/uart.h>

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
    struct cr_command_result res;
    if (proto->cmd.result != CR_PROTO_RESULT_OK) {
        handle_error(proto);
        return proto->state.protocol;
    }

    if (proto->state.protocol != proto->cmd.parsed.cmd->state) {
        printk("cr: Command %s expects state %d but %d is current.\n",
               proto->cmd.parsed.cmd->name,
               proto->cmd.parsed.cmd->state,
               proto->state.protocol);
        return proto->state.protocol;
    }

    if (proto->state.protocol == CR_PROTO_STATE_MULTILINE) {
        res = proto->multiline_cb(proto->cmd.parsed.cmd,
                                  proto->cmd.parsed.args,
                                  proto->cmd.parsed.argn);
    } else {
        res = proto->cmd.parsed.cmd->cb(proto->cmd.parsed.cmd,
                                        proto->cmd.parsed.args,
                                        proto->cmd.parsed.argn);
    }

    switch (res.next_state) {
    case CR_PROTO_STATE_MULTILINE:
        if (proto->state.protocol == CR_PROTO_STATE_ACTIVE) {
            /* ACTIVE -> MULTI */
            printk("cr: Going to multiline mode\n");
            proto->multiline_cb = proto->cmd.parsed.cmd->cb;
        }
        break;
    case CR_PROTO_STATE_ACTIVE:
        if (proto->state.protocol == CR_PROTO_STATE_MULTILINE) {
            /* MULTI -> ACTIVE */
            printk("cr: Going back to active mode\n");
            proto->multiline_cb = NULL;
        }
        break;
    default:
        /* Nothing to do in the other cases. */
        break;
    }

    proto->state.protocol = res.next_state;
    proto->cmd.result = res.result;

    if (proto->cmd.result != CR_PROTO_RESULT_OK) {
        handle_error(proto);
    }

    return proto->state.protocol;
}

int
text_transmit(struct cr_port *port, uint32_t tx, uint32_t *rx)
{
    printk("Sending stuff: 0x%08x\n", tx);
    return 0;
}

void
cr_run(void *a, void *b, void *c)
{
    struct cr_protocol proto;
    char ch = 0;

    printk("ChipRemote Command Processor online!\n");
    cr_process_init(&proto, cr_input, CR_MAX_LINE_SIZE, text_transmit);
    for (;;) {
        k_msgq_get(&cr_charqueue, &ch, K_FOREVER);
        switch (cr_process_octet(&proto, ch)) {
        case CR_PROCESS_PENDING:
            /* Nothing to do. */
            break;
        case CR_PROCESS_COMMAND:
            proto.state.protocol = run_command(&proto);
            break;
        case CR_PROCESS_INPUT_TO_LONG:
            printk("cr: Input too long (max: %d); line ignored.\n",
                   CR_MAX_LINE_SIZE);
            break;
        }
    }
}

K_THREAD_DEFINE(cr_run_thread, CR_STACK_SIZE,
                cr_run, NULL, NULL, NULL,
                CR_PRIORITY, 0, 0);

#ifdef CONFIG_BOARD_NATIVE_POSIX
void
main(void)
{
    struct device * const uart = device_get_binding(DT_LABEL(DT_NODELABEL(uart0)));
    if (uart == NULL) {
        printk("Could not access uart-0. Giving up.\n");
        return;
    }
    char board[MAX_BOARD_NAME_LENGTH];
    strlcpy(board, CONFIG_BOARD, sizeof(board));
    printk("ChipRemoteFirmware running on %s\n", board);

    char ch = 0;
    for (;;) {
        /* Poll controlling UART port and feed fifo */
        const int rc = uart_poll_in(uart, &ch);
        if (rc == 0) {
            k_msgq_put(&cr_charqueue, &ch, K_FOREVER);
        } else {
            k_usleep(1000);
        }
    }
}
#endif /* CONFIG_BOARD_NATIVE_POSIX */
