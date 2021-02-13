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

typedef int (*transmit_impl)(const struct cr_port*, uint32_t, uint32_t*);

struct cr_protocol {
    struct {
        enum cr_proto_state protocol;
        enum cr_input_state input;
    } state;
    struct {
        char *buffer;
        size_t size;
        size_t idx;
    } in;
    struct {
        struct cr_proto_parse parsed;
        enum cr_proto_result result;
    } cmd;
    struct {
        struct cr_port *table;
        size_t tablesize;
        size_t current;
        bool focused;
    } ports;
    string_sink reply;
    transmit_impl transmit;
    cr_command_callback multiline_cb;
};

void cr_process_init(struct cr_protocol*, char*, size_t,
                     transmit_impl, string_sink);
enum cr_process_result cr_process_octet(struct cr_protocol*, const char);

#endif /* INC_PROCESS_H */
