#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "proto-utils.h"
#include "chip-remote.h"
#include "utils.h"

static char txbuf[CR_MAX_LINE + 1];
static int txbuflen;

void
tx_init(void)
{
    memset((void*)txbuf, 0, CR_MAX_LINE + 1);
    txbuflen = 0;
}

void
tx_add_n(char *buf, size_t len)
{
    int left;

    if (len == 0)
        len = strlen(buf);
    left = CR_MAX_LINE - txbuflen;
    if (left <= 0 || len > left)
        return;
    txbuflen += len;
    strncat(txbuf, buf, len);
}

void
tx_add(char *buf)
{
    tx_add_n(buf, 0);
}

void
tx_add_space(void)
{
    tx_add(" ");
}

void
tx_add_integer(uint32_t i)
{
    char ibuf[CR_INT_MAX_LEN + 1];

    uint2str(i, ibuf);
    tx_add(ibuf);
}

void
tx_trigger(void)
{
    xcr_send_host(txbuf);
}
