/**
 * @file proto.c
 * @brief Serial protocol between host and serial device
 */

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "proto.h"
#include "serial.h"

static int proto_snprintf(char *, size_t, const char *, ...);

static int
proto_snprintf(char *buf, size_t len, const char *fmt, ...)
{
    int rc;
    size_t cnt;
    va_list ap;

    va_start(ap, fmt);
    rc = 1;
    cnt = vsnprintf(buf, len, fmt, ap);
    if (cnt >= SERIAL_BUF_MAX) {
        (void)printf("BUG: snprintf(): buffer truncated! Giving up.\n");
        rc = 0;
    }
    va_end(ap);
    return rc;
}

int
proto_expect_ok(void)
{
    return proto_expect_reply("OK");
}

int
proto_expect_reply(char *expected)
{
    int rc;
    char got[SERIAL_BUF_MAX + 1];

    rc = serial_read(got);
    if (rc <= 0)
        return 0;
    if (!strcmp(expected, got))
        return 1;
    return 0;
}

int
proto_hi(void)
{
    return serial_write("HI");
}


int
proto_bye(void)
{
    return serial_write("BYE");
}

int
proto_get_reg(unsigned int reg)
{
    int rc;
    char buf[SERIAL_BUF_MAX + 1];

    rc = proto_snprintf(buf, SERIAL_BUF_MAX,
                        "READ-REGISTER %x", reg);
    if (!rc)
        return 0;

    return serial_write(buf);
}

uint32_t
proto_read_integer(void)
{
    int rc;
    char reply[SERIAL_BUF_MAX + 1];

    rc = serial_read(reply);
    if (rc <= 0)
        return 0;
    return (uint32_t)scm_to_uint32(
        scm_c_locale_stringn_to_number(reply, strlen(reply), 16));
}

int
proto_write_reg(unsigned int reg, uint32_t value)
{
    int rc;
    char buf[SERIAL_BUF_MAX + 1];

    rc = proto_snprintf(buf, SERIAL_BUF_MAX,
                        "WRITE-REGISTER %x %08x", reg, value);
    if (!rc)
        return 0;

    return serial_write(buf);
}
