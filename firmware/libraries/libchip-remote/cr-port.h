/**
 * @file cr-port.h
 * @brief Chip Remote Port abstraction
 *
 * A port is a set of lines that work in concert to implement a interface type.
 * There are static port types like SPI, that implements SPI and only SPI. This
 * may be implemented by a peripheral or by bit-banging GPIOs.
 *
 * The special port type FLEX allows selecting one of many interface types that
 * a given set of lines can implement.
 */

#ifndef INC_CR_PORT_H
#define INC_CR_PORT_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

enum cr_port_type {
    CR_PORT_TYPE_FLEX,
    CR_PORT_TYPE_UART,
    CR_PORT_TYPE_SPI,
    CR_PORT_TYPE_I2C,
    CR_PORT_TYPE_IO
};

enum cr_bit_order {
    CR_BIT_MSB_FIRST,
    CR_BIT_LSB_FIRST
};

enum cr_logic {
    CR_LOGIC_DIRECT,
    CR_LOGIC_INVERTED
};

enum cr_edge {
    CR_EDGE_RISING,
    CR_EDGE_FALLING
};

enum cr_line_mode {
    CR_LINE_INPUT_NOPULL,
    CR_LINE_INPUT_PULLUP,
    CR_LINE_INPUT_PULLDOWN,
    CR_LINE_OUTPUT_OPEN,
    CR_LINE_OUTPUT_PUSHPULL,
    CR_LINE_PERIPHERAL
};

struct cr_line {
    const void *port;
    unsigned int pin;
    enum cr_line_mode mode;
};

struct cr_port_spi {
    uint32_t address;
    uint16_t frame_length;
    enum cr_bit_order bit_order;
    struct {
        uint16_t number;
        enum cr_logic polarity;
    } cs;
    struct {
        uint32_t rate;
        enum cr_edge edge;
        bool phase_delay;
    } clk;
};

struct cr_port_spi_bb {
    struct cr_line *cs;
    struct cr_line *clk;
    struct cr_line *mosi;
    struct cr_line *miso;
};

struct cr_port_io {
    unsigned int flags;
};

struct cr_port;

struct cr_port_api {
    int (*init)(struct cr_port*);
    int (*xfer)(struct cr_port*, uint32_t, uint32_t*);
    int (*address)(struct cr_port*, uint32_t);
};

struct cr_port {
    /* Generic data */
    const char *name;
    enum cr_port_type type;
    bool initialised;
    /* Generic port API */
    const struct cr_port_api *api;
    /* Port type specific configuration */
    union {
        struct cr_port_spi spi;
    } cfg;
    /* Line table */
    size_t lines;
    struct cr_line *line;
    /* Implementation specific data */
    void *data;
};

int cr_transmit(struct cr_port*, uint32_t, uint32_t*);

#endif /* INC_CR_PORT_H */
