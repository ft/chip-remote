/*
 * Copyright (c) 2013-2014 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file chip-remote.h
 * @brief Header for main chip-remote operation
 */

#ifndef INC_CHIP_REMOTE_H
#define INC_CHIP_REMOTE_H

#include <stdlib.h>
#include "platform.h"

#define CR_MAX_WORDS 8
#define CR_MAX_LINE 127
#define CR_MAX_ROLE_STRING 8
#define CR_INT_MAX_LEN 8
#define CR_STRING_PROP_MAX 16
#define CR_NO_INDEX -1

#define CR_IMMUTABLE 0
#define CR_MUTABLE 1

enum cr_pin_role {
    CR_ROLE_INVALID = -1,
    CR_ROLE_NONE,
    CR_ROLE_CLK,
    CR_ROLE_CS,
    CR_ROLE_SPI_MISO,
    CR_ROLE_SPI_MOSI
};

enum cr_access_mode {
    CR_ACCESS_READ = 0,
    CR_ACCESS_WRITE
};

struct cr_state {
    int cr_active;
    int line_pending;
    int fport;
};

struct cr_symb_tab {
    char *symbol;
    int value;
};

struct cr_int_prop {
    int mutable_p;
    int value;
};

struct cr_string_prop {
    int mutable_p;
    char value[CR_STRING_PROP_MAX + 1];
};

struct cr_parameter {
    char *name;
    struct cr_string_prop value;
};

enum cr_port_modes {
    CR_MODE_NONE = 0,
    CR_MODE_SPI,
    CR_MODE_INVALID
};

/**
 * Description of a line (or pin) within chip-remote
 *
 * In chip-remote, a port is made up of a number of pins. This structure
 * describes such a pin.
 *
 * On microcontrollers, ports are made up of pins, too. Usually in packs of
 * eight or so, like this:
 *
 * @code
 *   +-------------------------------+
 *   | micro-controller port A       |
 *   +-------------------------------+
 *   | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
 *   +-------------------------------+
 *   | CR Port 0 |     CR Port 1     |
 *   +-------------------------------+
 * @endcode
 *
 * And like listed below that, a micro-controller port can host multiple
 * chip-remote ports. Or the other way around: A big chip-remote port can span
 * over multiple micro-controller ports.
 *
 * So, a pin is described by micro-controller port address and bit-mask (in
 * which only one bit should be set unless you are being tricky).
 */
struct cr_line {
    int (*access)(struct cr_line *, enum cr_access_mode mode, int value);
    cr_pin_mask bitmask;
    char rolestr[CR_MAX_ROLE_STRING + 1];
    enum cr_pin_role role;
    size_t index;
    int mutable_p;
};

#include "line-maps.h"

/**
 * Description of a port configuration
 *
 * Each port has a current mode attached to it. Each mode has a specific
 * configuration structure, that is accessable via this structure's "u" union.
 */
struct cr_port_mode {
    int mutable_p;
    enum cr_port_modes mode;
    union {
        struct cr_spi_map *spi;
    } map;
};

/**
 * Description of a port in chip-remote terms
 *
 * A port has a list of lines and a configuration.
 */
struct cr_port {
    int lines;
    int status;
    struct cr_int_prop rate;
    struct cr_port_mode mode;
    struct cr_parameter *params;
    struct cr_line *l;
    uint32_t (*transmit)(struct cr_port *port, uint32_t word);
};

struct cr_args {
    unsigned int min;
    /** If -1, there is no maximum */
    int max;
};

struct cr_word {
    char *start;
    size_t length;
};

struct cr_words {
    struct cr_word word[CR_MAX_WORDS];
    size_t count;
};

enum cr_request_ids {
    REQUEST_HI = 0,
    REQUEST_BYE,
    REQUEST_FEATURES,
    REQUEST_FOCUS,
    REQUEST_INIT,
    REQUEST_LINES,
    REQUEST_LINE,
    REQUEST_MODES,
    REQUEST_PORT,
    REQUEST_PORTS,
    REQUEST_SET,
    REQUEST_TRANSMIT,
    REQUEST_VERSION,
    MAX_REQUEST
};

enum cr_top_states {
    CR_IN_CONVERSATION,
    CR_NO_CONVERSATION
};

enum cr_conv_states {
    CR_SINGLE_LINE = 0,
    CR_MULTI_LINE
};

struct cr_request {
    char *request;
    enum cr_request_ids id;
    enum cr_conv_states state;
    int (*cb)(int cnt, struct cr_words *words);
    struct cr_args args;
    int optional;
};

void cr_set_active(int);
void cr_set_line_pending(int);
void cr_set_focused_port(int);
int cr_get_focused_port(void);
void cr_set_active(int);
void cr_init(int);
void cr_top_level(void);
enum cr_request_ids cr_string_to_request(struct cr_words *, size_t);
extern struct cr_request requests[MAX_REQUEST + 1];
extern struct cr_port cr_ports[];

#endif /* INC_CHIP_REMOTE_H */
