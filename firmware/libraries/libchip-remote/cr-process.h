#ifndef INC_PROCESS_H
#define INC_PROCESS_H

#include <chip-remote.h>
#include <cr-port.h>

enum cr_process_result {
    CR_PROCESS_PENDING,
    CR_PROCESS_COMMAND,
    CR_PROCESS_INPUT_TOO_LONG
};

enum cr_input_state {
    CR_INPUT_PROCESS,
    CR_INPUT_IGNORE
};

struct cr_protocol {
    struct {
        enum cr_input_state input;
    } state;
    struct {
        struct cr_value token[CR_PROTOCOL_MAX_TOKENS];
    } data;
    struct {
        char *buffer;
        size_t size;
        size_t idx;
    } in;
    struct {
        struct cr_parser_state state;
        struct cr_tokens tokens;
        enum cr_parser_result result;
    } parser;
    struct {
        struct cr_port **table;
        size_t tablesize;
        size_t current;
        bool focused;
    } ports;
    string_sink reply;
};

void cr_process_init(struct cr_protocol*, char*, size_t, string_sink);
enum cr_process_result cr_process_octet(struct cr_protocol*, const char);
void cr_toplevel(struct cr_protocol*, const char);
void cr_protocol_boot(struct cr_protocol*);

#endif /* INC_PROCESS_H */
