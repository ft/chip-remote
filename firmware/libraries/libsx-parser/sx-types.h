#ifndef INC_SX_NODE_H
#define INC_SX_NODE_H

#include <stddef.h>
#include <stdint.h>

enum sx_status {
    SXS_SUCCESS,
    SXS_FOUND_LIST,
    SXS_BROKEN_INTEGER,
    SXS_BROKEN_SYMBOL,
    SXS_UNKNOWN_INPUT,
    SXS_UNEXPECTED_END
};

enum sx_node_type {
    SXT_SYMBOL,
    SXT_INTEGER,
    SXT_PAIR,
    SXT_EMPTY_LIST
};

struct sx_pair;

struct sx_node {
    enum sx_node_type type;
    union {
        uint64_t u64;
        char *symbol;
        struct sx_pair *pair;
    } data;
};

struct sx_pair {
    struct sx_node *car;
    struct sx_node *cdr;
};

struct sx_parse_result {
    size_t position;
    enum sx_status status;
    struct sx_node *node;
};

#define SX_PARSE_RESULT_INIT {          \
        .position = 0u,                 \
        .status = SXS_SUCCESS,          \
        .node = NULL }

#endif /* INC_SX_NODE_H */
