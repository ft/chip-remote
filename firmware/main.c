#include <device.h>
#include <kernel.h>

#include <drivers/uart.h>

#include <string.h>
#include <c/compat.h>

#include <chip-remote.h>
#include <parse-string.h>

#define MAX_BOARD_NAME_LENGTH 32u
struct cr_protocol cr = CR_PROTOCOL_STATIC_INIT;

#define CR_QUEUE_SIZE 16u
K_MSGQ_DEFINE(cr_charqueue, sizeof(char), CR_QUEUE_SIZE, 4u);

#define CR_STACK_SIZE 512u
#define CR_PRIORITY 5u

#define CR_MAX_LINE_SIZE 64u
char cr_input[CR_MAX_LINE_SIZE];

void
cr_run(void *a, void *b, void *c)
{
    enum { CR_INPUT_IGNORE, CR_INPUT_PROCESS } state = CR_INPUT_PROCESS;
    struct cr_proto_parse cmd;
    size_t idx = 0u;
    char ch = 0;

    printk("Hello from cr_run\n");

    for (;;) {
        switch (state) {
        case CR_INPUT_IGNORE:
            k_msgq_get(&cr_charqueue, &ch, K_FOREVER);
            if (ch == '\n') {
                idx = 0u;
                state = CR_INPUT_PROCESS;
            }
            break;
        case CR_INPUT_PROCESS:
            if (idx >= CR_MAX_LINE_SIZE) {
                printk("Maximum line size reached. Ignoring until LF!\n");
                state = CR_INPUT_IGNORE;
                break;
            }
            k_msgq_get(&cr_charqueue, cr_input + idx, K_FOREVER);
            if (cr_input[idx] == '\n') {
                cr_input[idx] = '\0';
                switch (cr_parse_string(cr_input, &cmd)) {
                case CR_PROTO_RESULT_OK:
                    printk("cr: Command -> %s\n", cmd.cmd->name);
                    cmd.cmd->cb(cmd.cmd, cmd.args, cmd.argn);
                    break;
                case CR_PROTO_RESULT_MALFORMED:
                    printk("cr: Malformed input: %s\n", cr_input);
                    break;
                case CR_PROTO_RESULT_WTF:
                    printk("cr: Unknown command on input: %s\n", cr_input);
                    break;
                case CR_PROTO_RESULT_BROKEN_VALUE:
                    printk("cr: Broken value on input: %s\n", cr_input);
                    break;
                case CR_PROTO_RESULT_VALUE_OUTOFRANGE:
                    printk("cr: Value out of range on input: %s\n", cr_input);
                    break;
                case CR_PROTO_RESULT_DONE:
                    printk("cr: DONE found on input: %s\n", cr_input);
                    break;
                }
                idx = 0u;
            } else {
                printk("cr_run: Got %c from queue!\n", cr_input[idx]);
                idx++;
            }
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
    struct device *uart = device_get_binding(DT_LABEL(DT_NODELABEL(uart0)));
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
