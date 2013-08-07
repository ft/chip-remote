#include <stdint.h>
#include <string.h>

#include "utils.h"

static char chtable[] = {
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'a', 'b', 'c', 'd', 'e', 'f' };

void
uint2str(uint32_t num, char *buf)
{
    /* buf needs to be able to hold 8+1 characters. */
    int i, step;

    for (i = 7; i >= 0; --i) {
        step = 4 * i;
        buf[7-i] = chtable[(num & (uint32_t)(0xful << step)) >> step];
    }
    buf[8] = '\0';
}

uint32_t
str2uint(const char *buf, int *err)
{
    char c;
    int idx, i;
    uint32_t rc, tmp;

    *err = 0;
    rc = 0;
    idx = strlen(buf) - 1;

    if (idx > 7)
        goto error;

    for (i = idx; i >= 0; --i) {
        c = buf[i];
        if (c >= '0' && c <= '9')
            tmp = c - '0';
        else if (c >= 'a' && c <= 'f')
            tmp = c - 'a' + 10;
        else if (c >= 'A' && c <= 'F')
            tmp = c - 'A' + 10;
        else
            goto error;

        rc |= tmp << ((idx - i) * 4);
    }
    return rc;

error:
    *err = 1;
    return 0;
}
