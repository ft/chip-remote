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
#include <stdint.h>

#define CR_PROTO_MAX_ARGS 4u

/**
 * Flags that encode possible outcomes of requests within the protocol
 */
enum cr_proto_result {
    CR_PROTO_RESULT_OK = 0,
    CR_PROTO_RESULT_WTF,
    CR_PROTO_RESULT_MALFORMED,
    CR_PROTO_RESULT_BROKEN_VALUE,
    CR_PROTO_RESULT_VALUE_OUTOFRANGE,
    CR_PROTO_RESULT_DONE
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
    CR_PROTO_CMD_PORT,
    CR_PROTO_CMD_SET,
    CR_PROTO_CMD_TRANSMIT,
    CR_PROTO_CMD_VERSION,
    /**
     * Encode unknown commands
     *
     * The UNKNOWN tag needs to be last in this enumeration.
     */
    CR_PROTO_CMD_UNKNOWN
};

/**
 * Flags to encode possible states the protocol can be in.
 */
enum cr_proto_state {
    CR_PROTO_STATE_ACTIVE,
    CR_PROTO_STATE_IDLE,
    CR_PROTO_STATE_MULTILINE
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

typedef enum cr_proto_result(*cr_command_callback)(const struct cr_command*,
                                                   const struct cr_value*,
                                                   unsigned int);

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
    enum cr_proto_state state;
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

enum cr_input_state {
    CR_INPUT_PROCESS,
    CR_INPUT_IGNORE
};

struct cr_protocol {
    enum cr_proto_state state;
    enum cr_input_state input;
};

#define CR_PROTOCOL_STATIC_INIT {               \
        .state = CR_PROTO_STATE_IDLE,           \
        .input = CR_INPUT_PROCESS }

enum cr_port_type {
    CR_PORT_TYPE_SPI,
    CR_PORT_TYPE_IO
};

enum cr_bit_order {
    CR_BIT_MSB_FIRST,
    CR_BIT_LSB_FIRST
};

enum cr_active_high_low {
    CR_ACTIVE_HIGH,
    CR_ACTIVE_LOW
};

enum cr_edge_rising_falling {
    CR_RISING_EDGE,
    CR_FALLING_EDGE
};

struct cr_line {
    unsigned int flags;
};

struct cr_port_spi {
    uint16_t frame_length;
    enum cr_bit_order bit_order;
    struct {
        uint16_t number;
        enum cr_active_high_low polarity;
    } cs;
    struct {
        uint32_t rate;
        enum cr_edge_rising_falling polarity;
        bool phase_delay;
    } clk;
};

struct cr_port_io {
    unsigned int flags;
};

struct cr_port {
    enum cr_port_type type;
    union {
        struct cr_port_spi spi;
        struct cr_port_io io;
    } impl;
};

#endif /* INC_CHIP_REMOTE_H */
