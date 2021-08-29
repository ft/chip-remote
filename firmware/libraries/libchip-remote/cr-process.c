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
run_command(struct cr_protocol *proto)
{
    if (proto->cmd.result != CR_PROTO_RESULT_OK)
        return;

    /* Install a couple of shorthands */
    const struct cr_proto_parse *parsed = &proto->cmd.parsed;
    const cr_command_callback cb = parsed->cmd->cb;

    if (cb == NULL) {
        printf("cr: Got NULL callback in command processing.\n");
        printf("cr: This should never happend and is likely a bug.\n");
        proto->reply("wtf NULL Callback. This is a bug!\n");
        return;
    }

    /* Actually run the callback picked depending on current protocol state */
    cb(proto, parsed);
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
        if (proto->in.idx >= proto->in.size) {
            proto->state.input = CR_INPUT_IGNORE;
            break;
        }
        proto->in.buffer[proto->in.idx] = ch;
        if (proto->in.buffer[proto->in.idx] == '\n') {
            proto->in.buffer[proto->in.idx] = '\0';
            proto->cmd.result = cr_parse_string(proto->reply,
                                                proto->in.buffer,
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
        proto->reply("wtf Input too long\n");
        break;
    }
}
