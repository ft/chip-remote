#include <stdlib.h>
#include <stdio.h>

#include "../chip-remote.h"
#include "../platform.h"
#include "../protocol.h"

#include "stdout.h"

int
access_portA(struct cr_line *line, enum cr_access_mode mode, int value)
{
    static int val = 0;

    printf("-!- %c%c[%04x]{%s}",
           'A',
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
