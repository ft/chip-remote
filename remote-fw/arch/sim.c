#include <stdlib.h>
#include <stdio.h>

#include "../chip-remote.h"
#include "../platform.h"
#include "../protocol.h"

#include "stdout.h"

int
access_portA(cr_pin_mask mask, enum cr_access_mode mode, int value)
{
    if (mode == CR_ACCESS_READ)
        return 1;
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
