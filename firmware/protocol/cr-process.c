#include <chip-remote.h>
#include <cr-process.h>
#include <parse-string.h>

void
cr_process_init(struct cr_protocol *proto,
                char *buf, size_t n,
                transmit_impl transmit)
{
    proto->state.protocol = CR_PROTO_STATE_IDLE;
    proto->state.input = CR_INPUT_PROCESS;
    proto->in.buffer = buf;
    proto->in.size = n;
    proto->in.idx = 0u;
    proto->transmit = transmit;
}

enum cr_process_result
cr_process_octet(struct cr_protocol *proto, const char ch)
{
    switch (proto->state.input) {
    case CR_INPUT_IGNORE:
        if (ch == '\n') {
            proto->in.idx = 0u;
            proto->state.protocol = CR_INPUT_PROCESS;
            return CR_PROCESS_INPUT_TO_LONG;
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
