#ifndef INC_PROCESS_H
#define INC_PROCESS_H

#include <chip-remote.h>

enum cr_process_result {
    CR_PROCESS_PENDING,
    CR_PROCESS_COMMAND,
    CR_PROCESS_INPUT_TO_LONG
};

enum cr_input_state {
    CR_INPUT_PROCESS,
    CR_INPUT_IGNORE
};

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
};

void cr_process_init(struct cr_protocol*, char*, size_t);
enum cr_process_result cr_process_octet(struct cr_protocol*, const char);

#endif /* INC_PROCESS_H */
