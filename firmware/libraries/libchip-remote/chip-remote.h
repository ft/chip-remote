/*
 * Copyright (c) 2020-2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file chip-remote.c
 * @brief Chip-remote protocol type definitions and API
 */

#ifndef INC_CHIP_REMOTE_H
#define INC_CHIP_REMOTE_H

#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define CR_PROTOCOL_VERSION_MAJOR      3u
#define CR_PROTOCOL_VERSION_MINOR      0u
#define CR_PROTOCOL_VERSION_PATCHLEVEL 0u

#define CR_PROTOCOL_MAX_TOKENS 32u

/** chip-remote number type */
typedef uint64_t cr_number;

#define CR_NUMBER_MAX UINT64_MAX
#define PRIuCRN PRIu64
#define PRIoCRN PRIo64
#define PRIxCRN PRIx64
#define PRIXCRN PRIX64

/**
 * Flags that encode possible outcomes of requests within the protocol
 */
enum cr_proto_result {
    CR_PROTO_RESULT_OK = 0,
    CR_PROTO_RESULT_WTF,
    CR_PROTO_RESULT_MALFORMED,
    CR_PROTO_RESULT_BROKEN_VALUE,
    CR_PROTO_RESULT_VALUE_OUTOFRANGE
};

/**
 * Encoding of commands (requests) of the protocol
 */
enum cr_proto_command {
    CR_PROTO_CMD_BYE = 0,
    CR_PROTO_CMD_CAPABILITIES,
    CR_PROTO_CMD_FOCUS,
    CR_PROTO_CMD_HASHED,
    CR_PROTO_CMD_HI,
    CR_PROTO_CMD_INIT,
    CR_PROTO_CMD_LINES,
    CR_PROTO_CMD_LINE,
    CR_PROTO_CMD_MODES,
    CR_PROTO_CMD_MODE,
    CR_PROTO_CMD_PORTS,
    CR_PROTO_CMD_PORT,
    CR_PROTO_CMD_SET,
    CR_PROTO_CMD_TRANSMIT,
    CR_PROTO_CMD_UVERSION,
    CR_PROTO_CMD_VERSION,
    /**
     * CR_PROTO_CMD_FW_* are commands in the private namespace "+*". For
     * instance, the CMD_PROTO_CMD_FW_VERSION command refers to the "+version"
     * keyword.
     */
    CR_PROTO_CMD_FW_VERSION,
    /**
     * Encode unknown commands
     *
     * The UNKNOWN tag needs to be last in this enumeration.
     */
    CR_PROTO_CMD_UNKNOWN
};

/**
 * Encoding for different types of values within the protocol
 *
 * The VOID type is used to signify something empty, like a list of arguments
 * that is empty. It is not a valid type of argument.
 */
enum cr_argument_type {
    CR_PROTO_ARG_TYPE_VOID = 0,
    CR_PROTO_ARG_TYPE_BOOLEAN,
    CR_PROTO_ARG_TYPE_INTEGER,
    CR_PROTO_ARG_TYPE_SYMBOL,
    CR_PROTO_ARG_TYPE_KEYVALUE
};

/**
 * Key value pair, symbol to number
 */
struct cr_keyvalue {
    char *key;
    cr_number value;
};

/**
 * Encoding of a value
 *
 * The argument list of parsed commands is of this type.
 */
struct cr_value {
    enum cr_argument_type type;
    union {
        struct cr_keyvalue kv;
        cr_number number;
        char *symbol;
        bool boolean;
    } data;
};

struct cr_command;
struct cr_protocol;
struct cr_proto_parse;

typedef void (*string_sink)(const char*);

typedef void(*cr_command_callback)(struct cr_protocol*,
                                   struct cr_command *,
                                   struct cr_value *,
                                   unsigned int);

/**
 * Represenation of a command within the protocol
 */
struct cr_command {
    enum cr_proto_command id;
    const char *name;
    cr_command_callback cb;
};

enum cr_parser_result {
    CR_PARSER_SUCCESS = 0,
    CR_PARSER_GENERIC_FAILURE,
    CR_PARSER_TOO_MANY_TOKENS,
    CR_PARSER_VALUE_OUT_OF_RANGE,
    CR_PARSER_INVALID_DIGIT_BIN,
    CR_PARSER_INVALID_DIGIT_OCT,
    CR_PARSER_INVALID_DIGIT_DEC,
    CR_PARSER_INVALID_DIGIT_HEX
};

struct cr_parser_state {
    char *input;
    size_t length;
    size_t position;
};

struct cr_tokens {
    struct cr_value *token;
    unsigned int size;
    unsigned int used;
};

void cr_parser_init(struct cr_parser_state*, char *);
void cr_tokens_init(struct cr_tokens*, struct cr_value*, size_t);
enum cr_parser_result cr_parse(struct cr_parser_state*, struct cr_tokens*);

#endif /* INC_CHIP_REMOTE_H */
