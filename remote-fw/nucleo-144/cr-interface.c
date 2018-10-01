/*
 * Copyright (c) 2013-2018 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <stdint.h>

#include <chip-remote.h>
#include <protocol.h>
#include <utils.h>

#include "cr-interface.h"
#include "usb-tty-interface.h"

static void consume_chunk(uint8_t*, uint32_t);

int
access_port0(struct cr_line *line, enum cr_access_mode mode, int value)
{
    (void)line;
    (void)mode;
    (void)value;
    return 0;
}

void
dir_port0(struct cr_line *line, enum cr_access_mode mode)
{
    (void)line;
    (void)mode;
}

void
xcr_init(void)
{
    set_tty_recv_cb(consume_chunk);
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
#ifdef WITH_SEMIHOSTING
    printf("Remote host terminated chip-remote connection.\n");
#endif /* WITH_SEMIHOSTING */
}

void
xcr_send_host(char *buf)
{
    cr_set_line_pending(0);
#ifdef WITH_SEMIHOSTING
    printf("< %s\n", buf);
#endif /* WITH_SEMIHOSTING */
    while (tty_send((uint8_t*)buf, strlen(buf)) == USBD_BUSY)
        __asm__(" nop");
    while (tty_send((uint8_t*)"\n", 1u) == USBD_BUSY)
        __asm__(" nop");
}

void
xcr_wait(uint32_t n)
{
    HAL_Delay(n);
}

static uint32_t
add_chunk(char *dst, uint32_t dstsize, uint32_t fill,
          uint8_t *src, uint32_t srcsize)
{
    static enum { COPY, IGNORE } state = COPY;

    if (state == IGNORE) {
        char *end = strchr((char*)src, '\n');
        if (end != NULL) {
            state = COPY;
            xcr_send_host("WTF Input too long and therefore ignored.");
            return fill;
        }
    }

    if ((fill + srcsize) > dstsize) {
        state = IGNORE;
        return fill;
    }

    /* We're in copy-mode and the source fits into the destination. Good. */
    memcpy(dst + fill, src, srcsize);
    {
        char *end = strchr(dst, '\n');
        if (end != NULL) {
            *end = '\0';
#ifdef WITH_SEMIHOSTING
            printf("> %s\n", rxbuf);
#endif /* WITH_SEMIHOSTING */
            cr_set_line_pending(1);
            return 0ull;
        }
    }

    return (fill + srcsize);
}

static void
consume_chunk(uint8_t *buf, uint32_t n)
{
    static int inputlen = 0;
    inputlen = add_chunk(rxbuf, CR_MAX_LINE, inputlen, buf, n);
}
