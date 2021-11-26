#include <string.h>

#include "cr-process.h"
#include "cr-utilities.h"

void
cr_proto_put_space(const struct cr_protocol *proto)
{
    proto->reply(" ");
}

void
cr_proto_put_newline(const struct cr_protocol *proto)
{
    proto->reply("\n");
}

static char chtable[] = {
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'a', 'b', 'c', 'd', 'e', 'f' };

static void
stringify_u64(uint64_t num, char *buf)
{
    /* buf needs to be able to hold 8+1 characters. */
    int i, max, step;

    for (i = 15; i >= 0; --i) {
        step = 4 * i;
        if (num & (uint64_t)(0xful << step)) {
            max = i;
            break;
        }
    }

    if (i < 0) {
        buf[0] = '0';
        buf[1] = '\0';
        return;
    }

    buf[i+1] = '\0';
    for (i = max; i >= 0; --i) {
        step = 4 * i;
        buf[max-i] = chtable[(num & (uint64_t)(0xful << step)) >> step];
    }
    buf[8] = '\0';
}

uint64_t
cr_parse_u64(const char *buf, int *err)
{
    const size_t len = strlen(buf);
    char c;
    int idx, i;
    uint64_t rc, tmp;

    *err = 0;
    rc = 0;
    idx = len - 1;

    if (idx > 15) {
        *err = 1;
        return 0u;
    }

    for (i = idx; i >= 0; --i) {
        c = buf[i];
        if (c >= '0' && c <= '9')
            tmp = c - '0';
        else if (c >= 'a' && c <= 'f')
            tmp = c - 'a' + 10;
        else if (c >= 'A' && c <= 'F')
            tmp = c - 'A' + 10;
        else {
            *err = 2;
            return 0u;
        }

        rc |= tmp << ((idx - i) * 4);
    }
    return rc;
}

void
cr_proto_put_u64(const struct cr_protocol *proto, uint64_t value)
{
    char buf[9];
    stringify_u64(value, buf);
    proto->reply(buf);
}
