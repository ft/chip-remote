/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file chip-remote.c
 * @brief Chip-remote protocol type definitions and API
 */

#ifndef INC_CHIP_REMOTE_H
#define INC_CHIP_REMOTE_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define CR_PROTO_MAX_ARGS 4u

#define CR_PROTOCOL_VERSION_MAJOR      2u
#define CR_PROTOCOL_VERSION_MINOR      0u
#define CR_PROTOCOL_VERSION_PATCHLEVEL 0u

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
    CR_PROTO_CMD_ADDRESS = 0,
    CR_PROTO_CMD_BYE,
    CR_PROTO_CMD_FEATURES,
    CR_PROTO_CMD_FOCUS,
    CR_PROTO_CMD_HASHED,
    CR_PROTO_CMD_HI,
    CR_PROTO_CMD_INIT,
    CR_PROTO_CMD_LINES,
    CR_PROTO_CMD_LINE,
    CR_PROTO_CMD_MODES,
    CR_PROTO_CMD_MORE,
    CR_PROTO_CMD_PORTS,
    CR_PROTO_CMD_PORT,
    CR_PROTO_CMD_SET,
    CR_PROTO_CMD_TRANSMIT,
    CR_PROTO_CMD_VERSION,
    /**
     * CR_PROTO_CMD_FW_* are commands in the private namespace "+*". For
     * instance, the CMD_PROTO_CMD_FW_VERSION command refers to the "+VERSION"
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
    CR_PROTO_ARG_TYPE_STRING
};

/**
 * Encoding of a value
 *
 * The argument list of parsed commands is of this type.
 */
struct cr_value {
    enum cr_argument_type type;
    union {
        uint32_t u32;
        char *string;
        bool boolean;
    } data;
};

struct cr_command;
struct cr_protocol;
struct cr_proto_parse;

typedef void (*string_sink)(const char*);

typedef void(*cr_command_callback)(const struct cr_protocol*,
                                   const struct cr_proto_parse*);

/**
 * Description of command argument specification
 *
 * This is used in struct cr_command to describe all the arguments, mandatory
 * or optional, a command takes.
 */
struct cr_argument {
    bool optional;
    enum cr_argument_type type;
};

/**
 * Represenation of a command within the protocol
 */
struct cr_command {
    enum cr_proto_command id;
    const char *name;
    struct cr_argument *args;
    cr_command_callback cb;
};

/**
 * Parser return type
 *
 * Since this module is made for use in embedded systems, this offers a way
 * to statically allocate enough memory to encode the most complex of parser
 * results.
 */
struct cr_proto_parse {
    struct cr_command *cmd;
    struct cr_value args[CR_PROTO_MAX_ARGS];
    unsigned int argn;
};

#endif /* INC_CHIP_REMOTE_H */
