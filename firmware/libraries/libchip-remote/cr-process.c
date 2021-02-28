#include <stdio.h>

#include <chip-remote.h>
#include <cr-process.h>
#include <parse-string.h>

void
cr_process_init(struct cr_protocol *p, char *b, size_t n, string_sink r)
{
    p->state.protocol = CR_PROTO_STATE_IDLE;
    p->state.input = CR_INPUT_PROCESS;
    p->in.buffer = b;
    p->in.size = n;
    p->in.idx = 0u;
    p->reply = r;
}

static void
handle_error(struct cr_protocol *proto)
{
    printf("cr: Error in command processing: %d\n", proto->cmd.result);
}

static enum cr_proto_state
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
        printf("cr: Command %s expects state %d but %d is current.\n",
               proto->cmd.parsed.cmd->name,
               proto->cmd.parsed.cmd->state,
               proto->state.protocol);
        proto->reply("WTF Wrong state for command to be issued.\n");
        return proto->state.protocol;
    }

    if (cb == NULL) {
        printf("cr: Got NULL callback in command processing.\n");
        printf("cr: This should never happend and is likely a bug.\n");
        proto->reply("WTF NULL Callback. This is a bug!\n");
        return proto->state.protocol;
    }

    /* Actually run the callback picked depending on current protocol state */
    next = cb(proto, parsed->cmd, parsed->args, parsed->argn);

    /* Perform multiline-mode setup in protocol state */
    switch (next) {
    case CR_PROTO_STATE_MULTILINE:
        if (proto->state.protocol == CR_PROTO_STATE_ACTIVE) {
            printf("cr: Protocol state active->multiline\n");
            proto->multiline_cb = parsed->cmd->cb;
        }
        break;
    case CR_PROTO_STATE_ACTIVE:
        if (proto->state.protocol == CR_PROTO_STATE_MULTILINE) {
            printf("cr: Protocol state multiline->active\n");
            proto->multiline_cb = NULL;
        } else if (proto->state.protocol == CR_PROTO_STATE_IDLE) {
            printf("cr: Protocol state idle->active\n");
        }
        break;
    case CR_PROTO_STATE_IDLE:
        if (proto->state.protocol == CR_PROTO_STATE_ACTIVE) {
            printf("cr: Protocol state active->idle\n");
        }
        break;
    default:
        /* Switch statement is exhaustive with enum cr_proto_state */
        break;
    }

    proto->state.protocol = next;
    return proto->state.protocol;
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
        proto->state.protocol = run_command(proto);
        break;
    case CR_PROCESS_INPUT_TOO_LONG:
        printf("cr: Input too long (max: %zu); line ignored.\n",
               proto->in.size);
        break;
    }
}
