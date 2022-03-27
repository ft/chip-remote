#include "cr-utilities.h"
#include <stdio.h>

#include <chip-remote.h>
#include <commands.h>
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
handle_parser_error(string_sink reply, enum cr_parser_result result)
{
    reply("wtf Parser error: ");
    switch (result) {
    case CR_PARSER_GENERIC_FAILURE:
        reply("Generic failure.");
        break;
    case CR_PARSER_INVALID_DIGIT_BIN:
        reply("Invalid binary digit.");
        break;
    case CR_PARSER_INVALID_DIGIT_HEX:
        reply("Invalid hexadecimal digit.");
        break;
    case CR_PARSER_INVALID_DIGIT_OCT:
        reply("Invalid octal digit.");
        break;
    case CR_PARSER_INVALID_DIGIT_DEC:
        reply("Invalid decial digit.");
        break;
    case CR_PARSER_TOO_MANY_TOKENS:
        reply("Too many tokens in input.");
        break;
    case CR_PARSER_VALUE_OUT_OF_RANGE:
        reply("Value out of range.");
        break;
    default:
        reply("Unknown error.");
        break;
    }
    reply("\n");
}

static void
handle_empty_command(string_sink reply)
{
    reply("wtf Empty command line.\n");
}

static void
handle_non_symbol_command(string_sink reply)
{
    reply("wtf Not a symbol in command position.\n");
}

static void
handle_unknown_command(string_sink reply, const char *name)
{
    reply("wtf Unknown command: ");
    reply(name);
    reply("\n");
}

static void
handle_missing_callback(string_sink reply, const char *name)
{
    printf("cr: Got NULL callback in command processing.\n");
    printf("cr: This should never happend and is likely a bug.\n");
    reply("wtf NULL Callback. This is a bug! Command name: ");
    reply(name);
    reply("\n");
}

static void
run_command(struct cr_protocol *proto)
{
    enum cr_argument_type type = proto->parser.tokens.token[0].type;
    unsigned int n = proto->parser.tokens.used;

    if (proto->parser.result != CR_PARSER_SUCCESS) {
        handle_parser_error(proto->reply, proto->parser.result);
        return;
    } else if (n == 0u) {
        handle_empty_command(proto->reply);
        return;
    } else if (type != CR_PROTO_ARG_TYPE_SYMBOL) {
        handle_non_symbol_command(proto->reply);
        return;
    }

    /* Install a couple of shorthands */
    char *name = proto->parser.tokens.token[0].data.symbol;
    struct cr_value *tokens = proto->parser.tokens.token;
    struct cr_command *cmd = cr_lookup_command(name);

    if (cmd->id == CR_PROTO_CMD_UNKNOWN) {
        handle_unknown_command(proto->reply, name);
        return;
    } else if (cmd->cb == NULL) {
        handle_missing_callback(proto->reply, name);
        return;
    }

    /* Actually run the callback picked depending on current protocol state */
    cmd->cb(proto, cmd, tokens, n);
}

static enum cr_parser_result
parse(struct cr_protocol *proto)
{
    char *input = proto->in.buffer;
    const unsigned int n = CR_PROTOCOL_MAX_TOKENS;

    cr_parser_init(&proto->parser.state, input);
    cr_tokens_init(&proto->parser.tokens, proto->data.token, n);
    return cr_parse(&proto->parser.state, &proto->parser.tokens);
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
            proto->parser.result = parse(proto);
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
