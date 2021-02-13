#ifndef INC_CR_PORT_H
#define INC_CR_PORT_H

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

#endif /* INC_CR_PORT_H */
