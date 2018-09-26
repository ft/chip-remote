/*
 * Copyright (c) 2013-2018 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <stdint.h>

#include "chip-remote.h"
#include "protocol.h"
#include "utils.h"

#include "nucleo-144.h"

int
access_port1(struct cr_line *line, enum cr_access_mode mode, int value)
{
    (void)line;
    (void)mode;
    (void)value;
    return 0;
}

void
dir_port1(struct cr_line *line, enum cr_access_mode mode)
{
    (void)line;
    (void)mode;
}

int
access_port2(struct cr_line *line, enum cr_access_mode mode, int value)
{
    (void)line;
    (void)mode;
    (void)value;
    return 0;
}

void
dir_port2(struct cr_line *line, enum cr_access_mode mode)
{
    (void)line;
    (void)mode;
}

int
access_port3(struct cr_line *line, enum cr_access_mode mode, int value)
{
    (void)line;
    (void)mode;
    (void)value;
    return 0;
}

void
dir_port3(struct cr_line *line, enum cr_access_mode mode)
{
    (void)line;
    (void)mode;
}

int
access_port4(struct cr_line *line, enum cr_access_mode mode, int value)
{
    (void)line;
    (void)mode;
    (void)value;
    return 0;
}

void
dir_port4(struct cr_line *line, enum cr_access_mode mode)
{
    (void)line;
    (void)mode;
}

int
access_port5(struct cr_line *line, enum cr_access_mode mode, int value)
{
    (void)line;
    (void)mode;
    (void)value;
    return 0;
}

void
dir_port5(struct cr_line *line, enum cr_access_mode mode)
{
    (void)line;
    (void)mode;
}

int
access_port6(struct cr_line *line, enum cr_access_mode mode, int value)
{
    (void)line;
    (void)mode;
    (void)value;
    return 0;
}

void
dir_port6(struct cr_line *line, enum cr_access_mode mode)
{
    (void)line;
    (void)mode;
}

void
xcr_pre_top_level(void)
{
    /*
     * This function could be used to poll data from the UART port until a line
     * is complete and then work on that. The simulator version of the firmware
     * does exactly that. But on this microcontroller, we do have interupts and
     * we will use that to handle incoming data instead.  This doesn't have any
     * work to do, therefore.
     */
}

void
xcr_post_bye(void)
{
    /*
     * The xcr_post_bye function is mostly in place for the simulator version
     * of the firmware to be able to exit after the conversation is done. The
     * actual firmware should just wait for the next conversion. So this is a
     * no-op here.
     */
}

#define SEND_BYTE(byte)

void
xcr_send_host(char *buf)
{
    char *ptr;

    ptr = buf;
    while (*ptr != '\0') {
        SEND_BYTE(*ptr);
        ptr++;
    }
    SEND_BYTE('\n');
}

void
xcr_wait(uint32_t n)
{
    uint32_t i;

    for (i = 0; i < n; ++i)
        __asm(" nop");
}

#define CR_RX_STATE_COPY 0
#define CR_RX_STATE_IGNORE 1

void uart_rx_interrupt(void);

void
uart_rx_interrupt(void)
{
    static int inputlen = 0;
    static int state = CR_RX_STATE_COPY;
    uint8_t devnull;

    switch (state) {
    case CR_RX_STATE_COPY:
        if (inputlen >= CR_MAX_LINE) {
            /* Input too long! Ignoring everything until next newline! */
            inputlen = 0;
            state = CR_RX_STATE_IGNORE;
        } else if ('f' == '\n') {
            rxbuf[inputlen] = '\0';
            inputlen = 0;
            cr_set_line_pending(1);
        } else {
            rxbuf[inputlen] = 'f';
            inputlen++;
        }
        break;
    default:
        devnull = 'f';
        if (devnull == '\n') {
            xcr_send_host("WTF Input too long and therefore ignored.");
            state = CR_RX_STATE_COPY;
        }
    }
    return;
}
