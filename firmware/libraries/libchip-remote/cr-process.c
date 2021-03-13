#include <stdio.h>

#include <chip-remote.h>
#include <cr-process.h>
#include <parse-string.h>

#include <common/compiler.h>

void
cr_process_init(struct cr_protocol *p, char *b, size_t n, string_sink r)
{
    p->state.input = CR_INPUT_PROCESS;
    p->in.buffer = b;
    p->in.size = n;
    p->in.idx = 0u;
    p->reply = r;
}

static void
handle_proto_error(const struct cr_protocol *proto)
{
    switch (proto->cmd.result) {
    case CR_PROTO_RESULT_WTF:
        proto->reply("WTF\n");
        break;
    case CR_PROTO_RESULT_MALFORMED:
        proto->reply("MALFORMED-REQUEST\n");
        break;
    case CR_PROTO_RESULT_BROKEN_VALUE:
        proto->reply("BROKEN-VALUE\n");
        break;
    case CR_PROTO_RESULT_VALUE_OUTOFRANGE:
        proto->reply("VALUE-OUT-OF-RANGE\n");
        break;
    case CR_PROTO_RESULT_OK:
    default:
        printf("cr: Error signaled, but detected none: This is a bug!\n");
        break;
    }
}
static void
handle_cb_error(UNUSED const struct cr_protocol *proto,
                UNUSED const struct cr_proto_parse *parsed,
                UNUSED cr_callback_value value)
{
}

static void
run_command(struct cr_protocol *proto)
{
    /* Exit early, if the protocol parser signalled an error in proto */
    if (proto->cmd.result != CR_PROTO_RESULT_OK) {
        handle_proto_error(proto);
        return;
    }

    /* Install a couple of shorthands */
    const struct cr_proto_parse *parsed = &proto->cmd.parsed;
    const cr_command_callback cb = parsed->cmd->cb;

    if (cb == NULL) {
        printf("cr: Got NULL callback in command processing.\n");
        printf("cr: This should never happend and is likely a bug.\n");
        proto->reply("WTF NULL Callback. This is a bug!\n");
        return;
    }

    /* Actually run the callback picked depending on current protocol state */
    const cr_callback_value rv = cb(proto, parsed);
    if (rv != CR_CB_OK) {
        handle_cb_error(proto, parsed, rv);
    }
}

enum cr_process_result
cr_process_octet(struct cr_protocol *proto, const char ch)
{
    switch (proto->state.input) {
    case CR_INPUT_IGNORE:
        if (ch == '\n') {
            proto->in.idx = 0u;
            proto->state.input = CR_INPUT_PROCESS;
            return CR_PROCESS_INPUT_TOO_LONG;
        }
        break;
    case CR_INPUT_PROCESS:
        proto->in.buffer[proto->in.idx] = ch;
        if (proto->in.idx >= proto->in.size) {
            proto->state.input = CR_INPUT_IGNORE;
            break;
        }
        if (proto->in.buffer[proto->in.idx] == '\n') {
            proto->in.buffer[proto->in.idx] = '\0';
            proto->cmd.result = cr_parse_string(proto->in.buffer,
                                                &proto->cmd.parsed);
            proto->in.idx = 0u;
            return CR_PROCESS_COMMAND;
        }
        proto->in.idx++;
        break;
    }
    return CR_PROCESS_PENDING;
}

void
cr_toplevel(struct cr_protocol *proto, const char ch)
{
    switch (cr_process_octet(proto, ch)) {
    case CR_PROCESS_PENDING:
        /* Nothing to do. */
        break;
    case CR_PROCESS_COMMAND:
        run_command(proto);
        break;
    case CR_PROCESS_INPUT_TOO_LONG:
        printf("cr: Input too long (max: %zu); line ignored.\n",
               proto->in.size);
        break;
    }
}
