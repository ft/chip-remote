/*
 * Copyright (c) 2013-2018 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <stdlib.h>
#include <stdio.h>

#include "../chip-remote.h"
#include "../platform.h"
#include "../protocol.h"

#include "stdout.h"

static int
access_port(char c, struct cr_line *line, enum cr_access_mode mode, int value)
{
    static int val = 0;

    printf("-!- %c%c[%04x]{%s}",
           c,
           mode == CR_ACCESS_READ ? '>' : '<',
           line->bitmask,
           line->rolestr);

    if (mode == CR_ACCESS_READ) {
        int v = (val++)%2;
        printf("(%x)\n", v);
        return v;
    }

    printf("(%d)\n", value ? 1 : 0);
    return -1;
}

static void
dir_port(char c, struct cr_line *line, enum cr_access_mode mode)
{
    if (mode == CR_ACCESS_READ)
        printf("-!- %cR[%04x]{%s}\n", c, line->bitmask, line->rolestr);
    else
        printf("-!- %cW[%04x]{%s}\n", c, line->bitmask, line->rolestr);
}

int
access_portA(struct cr_line *line, enum cr_access_mode mode, int value)
{
    return access_port('A', line, mode, value);
}

int
access_portB(struct cr_line *line, enum cr_access_mode mode, int value)
{
    return access_port('B', line, mode, value);
}

void
dir_portA(struct cr_line *line, enum cr_access_mode mode)
{
    dir_port('A', line, mode);
}

void
dir_portB(struct cr_line *line, enum cr_access_mode mode)
{
    dir_port('B', line, mode);
}

void
xcr_pre_top_level(void)
{
    int ch, i, length_notice;

    length_notice = i = 0;
    for (;;) {
        ch = getchar();
        if (ch == '\n')
            break;
        if (ch == EOF) {
            printf("-!- Input died, giving up.\n");
            exit(1);
        }
        if (i == CR_MAX_LINE) {
            if (!length_notice) {
                length_notice = 1;
                printf("-!- Input too long. Ignoring rest until EOL!\n");
            }
        } else
            rxbuf[i++] = (char)ch;
    }
    rxbuf[i] = '\0';
    printf(">>> %s\n", rxbuf);
    cr_set_line_pending(1);
}

void
xcr_post_bye(void)
{
    exit(0);
}

void
xcr_send_host(char *buf)
{
    printf("<<< %s\n", buf);
    fflush(NULL);
}

void
xcr_wait(uint32_t n)
{
    printf("-!- time: %u\n", n);
}
