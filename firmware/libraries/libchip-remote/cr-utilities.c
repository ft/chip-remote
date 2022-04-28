#include <assert.h>
#include <limits.h>
#include <stdint.h>
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
stringify_number(cr_number num, char *buf)
{
    /* buf needs to be able to hold 16+1 characters. */
    int i, max, step;

    for (i = 15; i >= 0; --i) {
        step = 4 * i;
        if (num & (cr_number)(0xful << step)) {
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
        buf[max-i] = chtable[(num & (cr_number)(0xful << step)) >> step];
    }
    buf[16] = '\0';
}

static inline int
value_of_digit(const char c, const unsigned int base)
{
    int rv;

    if (c >= '0' && c <= '9')
        rv = c - '0';
    else if (c >= 'a' && c <= 'z')
        rv = c - 'a' + 10;
    else if (c >= 'A' && c <= 'Z')
        rv = c - 'A' + 10;
    else
        rv = -1;

    return (rv >= (int)base) ? -1 : rv;
}

static inline bool
would_be_oob(const cr_number rv, const cr_number ls,
             const cr_number digit, const cr_number li)
{
    return (rv > ls) || ((rv == ls) && (digit > li));
}

cr_number
cr_parse_number(const char *buf, unsigned int base,
                char **stop, unsigned int *flags)
{
    assert(base >= 2u && base <= 36u);
    assert(buf != NULL);

    cr_number rv = 0ull;
    size_t len = strlen(buf);
    bool oob = false;
    *flags = 0u;

    const cr_number lastsafe = CR_NUMBER_MAX / (cr_number)base;
    const cr_number lastincr = CR_NUMBER_MAX % (cr_number)base;

    for (size_t i = 0; i < len; ++i) {
        int d = value_of_digit(buf[i], base);
        if (d < 0) {
            *flags = 1;
            *stop = (char*)buf + i;
            break;
        }

        if (oob)
            continue;

        if (would_be_oob(rv, lastsafe, d, lastincr)) {
            oob = true;
            rv = CR_NUMBER_MAX;
        } else {
            rv = rv * base + d;
        }
    }

    if (*flags == 0) {
        *stop = (char*)buf + len;
        if (oob) {
            *flags = 2;
        }
    }

    return rv;
}

void
cr_proto_put_number(const struct cr_protocol *proto, cr_number value)
{
    char buf[17];
    stringify_number(value, buf);
    proto->reply("h#");
    proto->reply(buf);
}

bool
cr_require_numofargs(struct cr_protocol *proto,
                     cr_number actual, cr_number expected)
{
    if (actual != expected) {
        proto->reply("malformed-request Invalid number of argument: ");
        cr_proto_put_number(proto, actual);
        proto->reply("!= ");
        cr_proto_put_number(proto, expected);
        cr_proto_put_newline(proto);
        return false;
    }

    return true;
}

bool
cr_require_arg_type(struct cr_protocol *proto, struct cr_value *token,
                    unsigned int n, enum cr_argument_type type)
{
    if (token[n].type != type) {
        proto->reply("malformed-request Invalid argument type\n");
        return false;
    }

    return true;
}

bool
cr_unknown_port(struct cr_protocol *proto, struct cr_port *port)
{
    if (port == NULL) {
        proto->reply("wtf Port lookup failed. This should not happen!\n");
        return true;
    }

    return false;
}

bool
cr_value_max(struct cr_protocol *proto, struct cr_value *token,
             unsigned int n, cr_number limit)
{
    assert(token[n].type == CR_PROTO_ARG_TYPE_INTEGER);

    if (token[n].data.number > limit) {
        proto->reply("value-out-of-range ");
        cr_proto_put_number(proto, token[n].data.number);
        proto->reply("> ");
        cr_proto_put_number(proto, limit);
        cr_proto_put_newline(proto);
        return false;
    }

    return true;
}
