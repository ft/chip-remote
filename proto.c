/**
 * @file proto.c
 * @brief Serial protocol between host and serial device
 */

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "proto.h"
#include "serial.h"

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
    int cnt;
    char buf[SERIAL_BUF_MAX + 1];

    cnt = snprintf(buf, SERIAL_BUF_MAX, "READ-REGISTER %x", reg);
    if (cnt >= SERIAL_BUF_MAX) {
        (void)printf("BUG: snprintf(): buffer truncated! Giving up.\n");
        return 0;
    }
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
